{-# LANGUAGE OverloadedStrings #-}

import           Control.Lens
import           Data.Aeson
import qualified Data.Map           as M
import           Data.Maybe
import           Test.Hspec
import           Web.Intercom.Types

userL :: UserList
userL = fromJust $ decode "{\"users\":[{\"name\":\"bob\", \"custom_attributes\":{\"foo\":\"bar\"}}], \"pages\":{\"next\":\"nextpage\"}}"

main :: IO ()
main = hspec $ do
  describe "user parsing" $ do
    it "grabs the name" $ do
      (userL ^. users . ix 0 . name) `shouldBe` Just "bob"
    it "grabs the custom_attributes" $ do
      (userL ^. users . ix 0 . customAttributes & M.lookup "foo") `shouldBe` Just (String "bar")
    it "grabs the next page" $ do
      (userL ^. next) `shouldBe` Just "nextpage"
