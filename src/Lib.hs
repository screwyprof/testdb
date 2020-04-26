{-# LANGUAGE OverloadedStrings #-}

module Lib
    ( someFunc
    ) where

import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromRow

data User = User { userID :: Int
                 , userName :: String 
                 , userEmail :: String
                 } deriving (Show)

instance FromRow User where
  fromRow = User <$> field <*> field <*> field

someFunc :: IO ()
someFunc = do
    conn <- mkConn
    u <- fetchUser conn 2341
    print u
    users <- fetchUsers conn
    mapM_ print users
    close conn

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
       ("normal" :: String, False, 5 :: Int) :: IO [User]

mkConn :: IO Connection
mkConn = connect
      defaultConnectInfo
      { connectHost = "localhost"
      , connectDatabase = "verisart"
      , connectUser = "postgres"
      , connectPassword = ""
      }