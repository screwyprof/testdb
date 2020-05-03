{-# LANGUAGE OverloadedStrings #-}

module Lib
    ( someFunc
    , User(..)
    , addUser
    , fetchUser
    , withDbConn
    ) where

import Control.Exception
import Database.PostgreSQL.Simple

import Db

someFunc :: IO ()
someFunc = do
    pgCfg <- mkDBCfg
    manageUsers pgCfg

manageUsers :: PGConfig -> IO ()
manageUsers pgCfg =
    catch (withDbConn (databaseUrl pgCfg) $ \conn -> do
        createUser conn newUser
        showUsers conn)
    (\e -> do 
        let err = show (e :: IOException)
        putStr $ "Cannot connect to the database: " <> err)

newUser :: User
newUser = User { userId = -1
               , userName="CoolUser"
               , userEmail="cool@cool.net"
               , userPassword="qwerty123"
               }

createUser :: Connection -> User -> IO ()
createUser conn nu = do 
    putStrLn "Creating user..."
    addedUser <- addUser conn nu
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
