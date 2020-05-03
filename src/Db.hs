{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ViewPatterns #-}

module Db
    ( User(..)
    , addUser
    , fetchUser
    , fetchUsers
    , withDbConn
    , mkDBCfg
    , databaseUrl
    , PGConfig
    ) where

import Control.Exception.Safe
import Data.ByteString (ByteString)
import GHC.Generics

import System.Envy

import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.Errors
import Database.PostgreSQL.Simple.FromRow
import Database.PostgreSQL.Simple.SqlQQ
import Database.PostgreSQL.Simple.ToRow

newtype PGConfig = PGConfig {
    databaseUrl :: ByteString -- "DATABASE_URL"
  } deriving (Generic, Show)

instance FromEnv PGConfig

mkDBCfg :: IO PGConfig
mkDBCfg = decodeWithDefaults defaultPGConfig

defaultPGConfig :: PGConfig
defaultPGConfig = PGConfig "postgres://postgres@localhost/test?sslmode=disable"

data User = User { userId :: Int
                 , userName :: String 
                 , userEmail :: String
                 , userPassword :: String
                 } deriving (Show, Eq)

instance FromRow User where
    fromRow = User <$> field <*> field <*> field <*> field

instance ToRow User where
    toRow User{..} = toRow (userName, userEmail, userPassword)

addUser :: Connection -> User -> IO (Either String User)
addUser conn newUser = do
    res <- try $ query conn [sql| 
        INSERT INTO users (name, email, password) 
        VALUES (?, ?, ?) RETURNING user_id       |] 
        newUser
    case res of 
        Right [Only uid] -> return $ Right newUser { userId = uid }
        Right _ -> throwString "PG hasn't returned userId"
        Left (constraintViolation -> Just (UniqueViolation _)) -> return $ Left "user already exists"
        Left e -> throwString $ "Unhandled PG exception: " <> show e

fetchUser :: Connection -> Int -> IO (Maybe User)
fetchUser conn userID = do
    user <- query conn [sql|
        SELECT user_id, name, email, password 
        FROM users WHERE user_id = ?         |] 
        (Only userID)
    pure $ case user of
        [u] -> Just u
        _ -> Nothing

fetchUsers :: Connection -> IO [User]
fetchUsers conn =
    query conn [sql| 
        SELECT user_id, name, email, password
        FROM users
        WHERE name IS NOT NULL 
            AND user_type = ? 
            AND deleted = ?
        ORDER BY user_id DESC
        LIMIT ?                              |]
        ("normal" :: String, False, 5 :: Int)

withDbConn :: ByteString -> (Connection -> IO ()) -> IO ()
withDbConn uri action = do
    conn <- connectPostgreSQL uri
    action conn
    close conn
