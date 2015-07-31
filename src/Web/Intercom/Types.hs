{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module Web.Intercom.Types (
    appId,
    apiKey,
    Client,
    ping
  ) where

import Data.ByteString
import Control.Lens
import Network.Wreq

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
  r <- getWith opts "https://api.intercom.io/admins"
  return $ r ^. responseStatus
  where opts = defaults & auth ?~ basicAuth (c ^. appId) (c ^. apiKey)
                        & header "Accept" .~ ["application/json"]
