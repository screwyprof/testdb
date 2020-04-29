{-# LANGUAGE OverloadedStrings #-}

module DbSpec (main, spec) where

import Data.ByteString (ByteString)
import Test.Hspec

import Db

main :: IO ()
main = hspec spec

spec :: Spec
spec = around (withDbConn testConnURI) $
    describe "fetch user" $ 
        it "returns valid user" $ \conn -> do
            Right want <- addUser conn testUser
            got <- fetchUser conn (userId want)
            got `shouldBe` Just want

testUser =  User { userId=0
                 , userName="Cool Guy"
                 , userEmail="cool@cool"
                 , userPassword="qwerty123"
                 }

testConnURI :: ByteString
testConnURI = "postgres://postgres@localhost/verisart"