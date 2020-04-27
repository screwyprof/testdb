{-# LANGUAGE OverloadedStrings #-}

module Lib
    ( someFunc
    , User(..)
    , addUser
    , fetchUser
    , withDbConn
    ) where

import Data.Maybe (fromJust)

import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.ToField
import Database.PostgreSQL.Simple.FromRow
import Database.PostgreSQL.Simple.ToRow

data User = User { userId :: Int
                 , userName :: String 
                 , userEmail :: String
                 , userPassword :: String
                 } deriving (Show, Eq)

instance FromRow User where
  fromRow = User <$> field <*> field <*> field <*> field

instance ToRow User where
  toRow u = [toField (userName u), toField (userEmail u), toField (userPassword u)]

someFunc :: IO ()
someFunc =
    withDbConn mkConnInfo $ \conn -> do
        let newUser = User { userId = -1
                           , userName="CoolUser"
                           , userEmail="cool@cool.net"
                           , userPassword="qwerty123"
                           }
        addedUser <- addUser conn newUser
        let uid = userId addedUser
        putStrLn $ "UserID : " ++ show uid
        u <- fromJust <$>fetchUser conn uid
        print u
        users <- fetchUsers conn
        mapM_ print users

addUser :: Connection -> User -> IO User
addUser conn u = do
    let q = "INSERT INTO users (name, email, password) VALUES (?, ?, ?) RETURNING user_id"
    [Only id] <- query conn q u
    return u { userId = id }

fetchUser :: Connection -> Int -> IO (Maybe User)
fetchUser conn userID = do
    u <- query conn "SELECT user_id, name, email, password FROM users WHERE user_id = ?" (Only userID)
    pure $ case u of
        [u] -> Just u
        _ -> Nothing

fetchUsers :: Connection -> IO [User]
fetchUsers conn =
    query conn
       ("SELECT user_id, name, email, password " <>
       "FROM users " <>
       "WHERE name IS NOT NULL AND user_type = ? AND deleted = ? " <>
       "ORDER BY user_id DESC " <>
       "LIMIT ?")
       ("normal" :: String, False, 5 :: Int)

withDbConn :: ConnectInfo -> (Connection -> IO ()) -> IO ()
withDbConn connInfo action = do
    conn <- connect connInfo
    action conn
    close conn

mkConnInfo :: ConnectInfo
mkConnInfo =
      defaultConnectInfo
      { connectHost = "localhost"
      , connectDatabase = "verisart"
      , connectUser = "postgres"
      , connectPassword = ""
      }