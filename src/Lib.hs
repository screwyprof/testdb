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
  -- Connect using libpq strings
  --conn <- connectPostgreSQL "host='localhost' port=5432 user=postgres pass=postgres"
    conn <- mkConn 
    -- conn <- connect
    --   defaultConnectInfo
    --   { connectHost = "localhost"
    --   , connectDatabase = "verisart"
    --   , connectUser = "postgres"
    --   , connectPassword = ""
    --   }
    --mapM_ print =<< (query_ conn "SELECT 1 + 1" :: IO [Only Int])
    --[Only i] <- query_ conn "select 2 + 2" :: IO [Only Int]
    --print i
    --[Only u] <- fetchUser conn :: IO [Only (Int, String, String)]
    u <- fetchUser conn
    print u

mkConn :: IO Connection
mkConn = connect
      defaultConnectInfo
      { connectHost = "localhost"
      , connectDatabase = "verisart"
      , connectUser = "postgres"
      , connectPassword = ""
      }

fetchUser :: Connection -> IO [User]
fetchUser conn = 
    query conn
       "SELECT user_id, name, email FROM users WHERE user_id = ?"
       (Only (2341::Int))