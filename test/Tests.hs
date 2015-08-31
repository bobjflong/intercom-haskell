{-# LANGUAGE OverloadedStrings #-}

import Test.Hspec
import Control.Lens
import Data.Maybe
import Web.Intercom.Types
import Data.Aeson
import qualified Data.HashMap.Lazy as M

userL :: UserList
userL = fromJust $ decode "{\"users\":[{\"name\":\"bob\", \"custom_attributes\":{\"foo\":\"bar\"}}], \"pages\":{\"next\":\"nextpage\"}}"

main :: IO ()
main = hspec $ do
  describe "user parsing" $ do
    it "grabs the name" $ do
      (userL ^. users . ix 0 . name) `shouldBe` (Just "bob")
    it "grabs the custom_attributes" $ do
      (userL ^. users . ix 0 . customAttributes & M.lookup "foo") `shouldBe` (Just (String "bar"))
    it "grabs the next page" $ do
      (userL ^. next) `shouldBe` (Just "nextpage")
  describe "user serializing" $ do
    it "encodes identity attributes when not Nothing" $ do
      let myUser = blankUser & email .~ (Just "bob@foo.com")
      (encode myUser) `shouldBe` "{\"email\":\"bob@foo.com\",\"custom_attributes\":{}}"
