{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module Web.Intercom.Types (
    appId,
    apiKey,
    Client,
    ping,
    userList,
    name,
    email,
    customAttributes,
    users,
    User,
    UserList,
    module Control.Lens
  ) where

import Data.ByteString
import Control.Lens
import Network.Wreq
import Data.Text
import Data.Aeson
import Control.Applicative
import Control.Monad
import Data.Map

data Client = Client {
  _appId :: ByteString,
  _apiKey :: ByteString
} deriving (Show)

$(makeLenses ''Client)

defaultClient :: Client
defaultClient = Client "" ""

-- | Test the connection to Intercom
-- > let client = defaultClient & appId .~ "foo" & apiKey .~ "bar"
-- > ping client
-- Status {statusCode = 200, statusMessage = "OK"}
ping :: Client -> IO Status
ping c = do
  r <- getWith (opts c) "https://api.intercom.io/admins"
  return $ r ^. responseStatus

opts c = defaults & auth ?~ basicAuth (c ^. appId) (c ^. apiKey)
                  & header "Accept" .~ ["application/json"]

data User = User {
  _name :: Maybe Text,
  _email :: Maybe Text,
  _userId :: Maybe Text,
  _customAttributes :: Map String Value
} deriving (Eq, Show)

$(makeLenses ''User)

instance FromJSON User where
  parseJSON (Object v) = do
    User <$> v .:? "name"
    <*> v .:? "email"
    <*> v .:? "user_id"
    <*> v .: "custom_attributes"
  parseJSON _ = mzero

data UserList = UserList {
  _users :: [User]
} deriving (Eq, Show)

$(makeLenses ''UserList)

instance FromJSON UserList where
  parseJSON (Object v) = UserList <$> (v .: "users")
  parseJSON _ = mzero

-- | Grab a page of users from your user list
-- > let client = defaultClient & appId .~ "foo" & apiKey .~ "bar"
-- > userList client
-- Just (UserList {_users = [User {_name = "bob" ...
userList :: Client -> IO (Maybe UserList)
userList c = do
  r <- getWith (opts c) "https://api.intercom.io/users"
  return $ decode (r ^. responseBody)
