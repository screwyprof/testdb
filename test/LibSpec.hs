module LibSpec (main, spec) where

import Test.Hspec
import Database.PostgreSQL.Simple
-- import Test.QuickCheck
import Lib

import Control.Monad.IO.Class
import Control.Exception.Base (bracket)

main :: IO ()
main = hspec spec

spec :: Spec
spec = around withDBConn $
    describe "fetch user" $ 
        it "returns valid user" $ \conn -> do
            got <- fetchUser conn 2341
            let want = [User {userID=2341, userName="Max O'Donnell", userEmail="max@verisart.com"}]
            got `shouldBe` want

withDBConn :: (Connection -> IO ()) -> IO ()
withDBConn = bracket mkTestConn close

mkTestConn :: IO Connection
mkTestConn = connect
      defaultConnectInfo
      { connectHost = "localhost"
      , connectDatabase = "verisart"
      , connectUser = "postgres"
      , connectPassword = ""
      }