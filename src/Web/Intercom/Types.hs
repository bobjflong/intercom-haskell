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
    blankUser,
    UserList,
    module Control.Lens
  ) where

import Data.ByteString
import Control.Lens hiding ((.=))
import Network.Wreq
import Data.Text
import Data.Aeson
import Control.Applicative
import Control.Monad
import Data.HashMap.Lazy
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

postOpts c = (opts c) & header "Content-Type" .~ ["application/json"]

data User = User {
  _name :: Maybe Text,
  _email :: Maybe Text,
  _userId :: Maybe Text,
  _customAttributes :: HashMap Text Value
} deriving (Eq, Show)

-- | A blank user, for overriding
-- > let myUser = blankUser & email .~ (Just "bob@foo.com")
blankUser :: User
blankUser = User Nothing Nothing Nothing (fromList [])

$(makeLenses ''User)

instance FromJSON User where
  parseJSON (Object v) =
    User <$> v .:? "name"
    <*> v .:? "email"
    <*> v .:? "user_id"
    <*> v .: "custom_attributes"
  parseJSON _ = mzero

instance ToJSON User where
  toJSON (User n e u c) = object $ nonEmptys attributes
    where nonEmptys xs = catMaybes $ fmap maybePair xs
          attributes = [
                         ("name", String <$> n),
                         ("email", String <$> e),
                         ("user_id", String <$> u),
                         ("custom_attributes", Just $ Object c)
                       ]

maybePair :: (Text, Maybe a) -> Maybe (Text, a)
maybePair (t, Just x) = Just (t, x)
maybePair _ = Nothing

data UserList = UserList {
  _users :: [User],
  _next :: Maybe Text
} deriving (Eq, Show)

$(makeLenses ''UserList)

instance FromJSON UserList where
  parseJSON (Object v) =
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

userList' :: Maybe String -> Client -> IO (Maybe UserList)
userList' u c = do
  r <- getWith (opts c) (fromMaybe "https://api.intercom.io/users" u)
  return $ decode (r ^. responseBody)

-- | Create or update a user in Intercom
-- > let myUser = blankUser & email .~ (Just "bob@foo.com")
-- Just (User {_name = Nothing, _email = Just "bob@foo.com", _userId = Nothing, _customAttributes = fromList []})
createOrUpdateUser :: User -> Client -> IO (Maybe User)
createOrUpdateUser u c = do
  r <- postWith (postOpts c) "https://api.intercom.io/users" (encode u)
  return $ decode (r ^. responseBody)
