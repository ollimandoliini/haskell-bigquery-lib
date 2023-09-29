-- {-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Credentials (createSignedJWT, getAccessToken, getPrivateKey) where

import Control.Exception (throwIO)
import Control.Lens ((#), (&), (?~))
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Time (MonadTime (..))
import Crypto.JOSE (Alg (RS256), MonadRandom)
import Crypto.JWT (Audience (Audience), HasClaimsSet (..), JWTError, NumericDate (NumericDate), SignedJWT, addClaim, emptyClaimsSet, encodeCompact, fromRSA, newJWSHeader, runJOSE, signClaims, string)
import Crypto.PubKey.RSA (PrivateKey)
import Data.Aeson (Value, eitherDecodeFileStrict, withObject, (.:))
import Data.Aeson qualified as Aeson (Value (String))
import Data.Aeson.Types (Parser, parseEither)
import Data.ByteString (StrictByteString)
import Data.ByteString.Char8 qualified as B
import Data.Text (Text, unwords)
import Data.Text.Lazy.Encoding (decodeUtf8)
import Data.Time (addUTCTime)
import Data.X509 qualified as X509
import Data.X509.Memory (readKeyFileFromMemory)
import Network.HTTP.Req (POST (POST), ReqBodyUrlEnc (ReqBodyUrlEnc), defaultHttpConfig, https, jsonResponse, req, responseBody, runReq, (/:), (=:))
import Types (AccessToken (unAccessToken), AppError (AppJWTError, AppPrivateKeyReadError, AppServiceAccountDecodeError))
import Prelude hiding (unwords)

getPrivateKey :: (MonadIO m) => FilePath -> m PrivateKey
getPrivateKey file = do
    eVal <- liftIO $ eitherDecodeFileStrict file
    case eVal of
        Left _ -> liftIO $ throwIO AppServiceAccountDecodeError
        Right v -> case parseEither extractPrivateKey v of
            Left _ -> liftIO $ throwIO AppPrivateKeyReadError
            Right privateKeyString ->
                case parsePrivateKey $ B.pack privateKeyString of
                    Just pk -> pure pk
                    Nothing -> liftIO $ throwIO AppPrivateKeyReadError
  where
    extractPrivateKey :: Value -> Parser String
    extractPrivateKey =
        withObject "ServiceAccount" $ \o -> o .: "private_key"

    parsePrivateKey :: StrictByteString -> Maybe PrivateKey
    parsePrivateKey bs =
        case readKeyFileFromMemory bs of
            [X509.PrivKeyRSA key] -> Just key
            _ -> Nothing

createSignedJWT ::
    ( MonadIO m
    , MonadRandom m
    ) =>
    Text ->
    Maybe Text ->
    [Text] ->
    PrivateKey ->
    m SignedJWT
createSignedJWT issuer msub scopes key = do
    t <- liftIO currentTime
    foo <- runJOSE $ signClaims jwk header (claims t)
    case foo of
        Right val -> pure val
        Left (err :: JWTError) -> liftIO $ throwIO $ AppJWTError err
  where
    jwk = fromRSA key
    header = newJWSHeader ((), RS256)
    claims t =
        emptyClaimsSet
            & claimIss ?~ (string # issuer)
            & maybe id (\sub -> claimSub ?~ (string # sub)) msub
            & claimAud ?~ Audience ["https://www.googleapis.com/oauth2/v4/token"]
            & claimIat ?~ NumericDate t
            & claimExp ?~ NumericDate (addUTCTime 3600 t)
            & addClaim "scope" (Aeson.String $ unwords scopes)

getAccessToken :: (MonadIO m) => SignedJWT -> m StrictByteString
getAccessToken signedJwt = do
    let tokenUrl = https "oauth2.googleapis.com" /: "token"
        tokenPayload =
            ReqBodyUrlEnc $
                ("grant_type" =: ("urn:ietf:params:oauth:grant-type:jwt-bearer" :: Text))
                    <> ("assertion" =: decodeUtf8 (encodeCompact signedJwt))
    unAccessToken . responseBody <$> runReq defaultHttpConfig (req POST tokenUrl tokenPayload jsonResponse mempty)
