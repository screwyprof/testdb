{-# LANGUAGE OverloadedStrings #-}

module LibSpec (main, spec) where

import Data.ByteString (ByteString)
import Test.Hspec
--import Database.PostgreSQL.Simple
--import Control.Exception.Base (bracket)

import Lib

main :: IO ()
main = hspec spec

spec :: Spec
spec = around (withDbConn testConnURI) $
    describe "fetch user" $ 
        it "returns valid user" $ \conn -> do
            let newUser = User { userId=0
                               , userName="Cool Guy"
                               , userEmail="cool@cool"
                               , userPassword="qwerty123"
                               }
            want <- addUser conn newUser
            got <- fetchUser conn (userId want)
            got `shouldBe` Just want

testConnURI :: ByteString
testConnURI = "postgres://postgres@localhost/verisart"