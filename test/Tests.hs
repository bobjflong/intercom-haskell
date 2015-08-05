{-# LANGUAGE OverloadedStrings #-}

import Test.Hspec
import Control.Lens
import Data.Maybe
import Web.Intercom.Types
import Data.Aeson
import qualified Data.Map as M

userL :: UserList
userL = fromJust $ decode "{\"users\":[{\"name\":\"bob\", \"custom_attributes\":{\"foo\":\"bar\"}}]}"

main :: IO ()
main = hspec $ do
  describe "user parsing" $ do
    it "grabs the name" $ do
      (userL ^. users . ix 0 . name) `shouldBe` (Just "bob")
    it "grabs the custom_attributes" $ do
      (userL ^. users . ix 0 . customAttributes & M.lookup "foo") `shouldBe` (Just (String "bar"))

