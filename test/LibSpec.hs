module LibSpec (main, spec) where

import Test.Hspec
import Database.PostgreSQL.Simple
import Control.Exception.Base (bracket)

import Lib

main :: IO ()
main = hspec spec

spec :: Spec
spec = around (withDbConn mkTestConnInfo) $
    describe "fetch user" $ 
        it "returns valid user" $ \conn -> do
            got <- fetchUser conn 2341
            let want = [User {userID=2341, userName="Max O'Donnell", userEmail="max@verisart.com"}]
            got `shouldBe` want

mkTestConnInfo :: ConnectInfo
mkTestConnInfo =
      defaultConnectInfo
      { connectHost = "localhost"
      , connectDatabase = "verisart"
      , connectUser = "postgres"
      , connectPassword = ""
      }