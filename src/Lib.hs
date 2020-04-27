{-# LANGUAGE OverloadedStrings #-}

module Lib
    ( someFunc
    , User(..)
    , fetchUser
    , withDbConn
    ) where

import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromRow

data User = User { userID :: Int
                 , userName :: String 
                 , userEmail :: String
                 } deriving (Show, Eq)

instance FromRow User where
  fromRow = User <$> field <*> field <*> field

someFunc :: IO ()
someFunc =
    withDbConn mkConnInfo $ \conn -> do
        u <- fetchUser conn 2341
        print u
        users <- fetchUsers conn
        mapM_ print users

fetchUser :: Connection -> Int -> IO [User]
fetchUser conn userID = 
    query conn
       "SELECT user_id, name, email FROM users WHERE user_id = ?"
       (Only userID)

fetchUsers :: Connection -> IO [User]
fetchUsers conn =
    query conn
       ("SELECT user_id, name, email " <>
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