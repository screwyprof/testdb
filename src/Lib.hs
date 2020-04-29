{-# LANGUAGE OverloadedStrings #-}

module Lib
    ( someFunc
    , User(..)
    , addUser
    , fetchUser
    , withDbConn
    ) where

import Data.ByteString (ByteString)
import Database.PostgreSQL.Simple

import Db

someFunc :: IO ()
someFunc =
    withDbConn connURI $ \conn -> do
        let newUser = User { userId = -1
                           , userName="CoolUser"
                           , userEmail="cool@cool.net"
                           , userPassword="qwerty123"
                           }
        createUser conn newUser
        showUsers conn

createUser :: Connection -> User -> IO ()
createUser conn newUser = do 
    putStrLn "Creating user..."
    addedUser <- addUser conn newUser
    case addedUser of 
        Left err -> putStrLn $ "Cannot add user: " <> err
        Right u -> showUser conn $ userId u

showUser :: Connection -> Int -> IO ()
showUser conn uid = do
    putStrLn $ "User #" ++ show uid ++ " created"
    putStrLn "User info:"
    mapM_ print =<< fetchUser conn uid

showUsers :: Connection -> IO ()
showUsers conn = do 
    putStrLn "Users:"
    mapM_ print =<< fetchUsers conn

connURI :: ByteString
connURI = "postgres://postgres@localhost/verisart"