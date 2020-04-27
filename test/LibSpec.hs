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
            let newUser = User { userId=0
                               , userName="Cool Guy"
                               , userEmail="cool@cool"
                               , userPassword="qwerty123"
                               }
            want <- addUser conn newUser
            got <- fetchUser conn (userId want)
            got `shouldBe` Just want

mkTestConnInfo :: ConnectInfo
mkTestConnInfo =
      defaultConnectInfo
      { connectHost = "localhost"
      , connectDatabase = "verisart"
      , connectUser = "postgres"
      , connectPassword = ""
      }