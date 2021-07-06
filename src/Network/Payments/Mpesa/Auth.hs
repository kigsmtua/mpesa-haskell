-- |
-- Module: Network.Payments.Mpesa.Auth
-- Copyright: (C) 2021 John Kiragu
-- License: BSD3 (see LICENSE file)
-- Maintainer: John Kiragu <mutuakiragu@gmail.com>
-- Stability: experimental

{-# LANGUAGE OverloadedStrings #-}

module Network.Payments.Mpesa.Auth where

import Network.Wreq
import Data.Aeson
import Control.Lens

import Control.Monad
import Control.Exception
import Data.Time.Clock
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text as T
import qualified Network.Wreq.Types as WTypes
import qualified Network.HTTP.Client as HTTP


-- | Mpesa Consumer Key to generated for access to the Mpesa API
type ConsumerKey = String

-- | Mpeesa Consumer Secret generated to access the Mpesa API
type ConsumerSecret = String

-- | Number representing seconds of time.
type Seconds = Integer


-- | Access token returned from OAuth.
data AccessToken = AccessToken
  {
    aToken :: String
  , aTokenExpiresIn :: Seconds
  } deriving (Eq, Show)


data AccessTokenError = AccessTokenHttpError HTTP.HttpException |
                        AccessTokenStatusError Int |
                        AccessTokenParseError String LBS.ByteString

instance FromJSON AccessToken where
  parseJSON (Object obj) =
    AccessToken <$>
    obj .: "access_token" <*>
    obj .: "expiry_in"
  parseJSON _ = mzero


-- |Either an access token or the error encountered while fetching it.
type AccessTokenResult = Either AccessTokenError AccessToken


-- | Fetch acess token from the Mpesa API
fetchAccessToken :: String -> ConsumerKey -> ConsumerSecret -> IO AccessTokenResult
fetchAccessToken url username password = do
  let usernameBS = BS8.pack username
      passwordBS = BS8.pack password
      fullUrl = url ++ "/v1/oauth2/token"
      options' = defaults & header "Accept" .~ ["application/json"] &
                            auth ?~ basicAuth usernameBS passwordBS
      contentType = "application/x-www-form-urlencoded"
      content = "grant_type=client_credentials"
      payload = WTypes.Raw contentType $ HTTP.RequestBodyBS content
  responseOrErr <- (try $ postWith options' fullUrl payload) ::
                   IO (Either HTTP.HttpException (Response LBS.ByteString))
  case responseOrErr of
    Left err -> return $ Left $ AccessTokenHttpError err
    Right response ->
      let statusCode' = response ^. responseStatus . statusCode
      in if statusCode' == 200 then
        let responseText = response ^. responseBody
        in return $ case eitherDecode responseText of
             Left errMsg -> Left $ AccessTokenParseError errMsg responseText
             Right result -> Right result
      else
        return $ Left $ AccessTokenStatusError statusCode'