{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module Web.Intercom.Types (
    appId,
    apiKey,
    Client,
    ping,
    userList,
    nextPage,
    name,
    email,
    customAttributes,
    users,
    next,
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
import Data.Maybe

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
  _users :: [User],
  _next :: Maybe Text
} deriving (Eq, Show)

$(makeLenses ''UserList)

instance FromJSON UserList where
  parseJSON (Object v) = do
    UserList <$> (v .: "users")
    <*> ((v .: "pages") >>= (.: "next"))
  parseJSON _ = mzero

-- | Grab a page of users from your user list
-- > let client = defaultClient & appId .~ "foo" & apiKey .~ "bar"
-- > userList client
-- Just (UserList {_users = [User {_name = "bob" ...
userList :: Client -> IO (Maybe UserList)
userList = userList' Nothing

-- | Grabs the page of users from your user list (cycles around at the end)
-- > nextPage client userList
-- Just (UserList {_users = [User {_name = "jim" ...
nextPage :: Client -> UserList -> IO (Maybe UserList)
nextPage c u = userList' (show <$> u ^. next) c

userList' :: (Maybe String) -> Client -> IO (Maybe UserList)
userList' u c = do
  r <- getWith (opts c) (fromMaybe "https://api.intercom.io/users" u)
  return $ decode (r ^. responseBody)
