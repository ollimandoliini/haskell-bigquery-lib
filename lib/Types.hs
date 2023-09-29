{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverloadedStrings #-}

module Types where

import Control.Exception (Exception)
import Crypto.JWT (JWTError)
import Data.Aeson (FromJSON (..), Value, withObject, (.:))
import Data.Aeson.Types (Parser)
import Data.ByteString (StrictByteString)
import Data.Text.Encoding (encodeUtf8)
import Network.HTTP.Req (HttpException)

data AppError
    = AppServiceAccountDecodeError
    | AppPrivateKeyReadError
    | AppJWTError JWTError
    | AppHttpError HttpException
    deriving (Show)

instance Exception AppError


newtype AccessToken = AccessToken
    { unAccessToken :: StrictByteString
    }
    deriving (Show)

instance FromJSON AccessToken where
    parseJSON :: Value -> Parser AccessToken
    parseJSON = withObject "AccessToken" $ \o -> AccessToken . encodeUtf8 <$> o .: "access_token"

