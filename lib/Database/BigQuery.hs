{-# LANGUAGE OverloadedStrings #-}

module Database.BigQuery where

import Credentials (createSignedJWT, getAccessToken, getPrivateKey)
import Data.Aeson (Value)
import Data.Text (Text)
import Network.HTTP.Req (GET (GET), JsonResponse, NoReqBody (NoReqBody), defaultHttpConfig, https, jsonResponse, oAuth2Bearer, req, responseBody, runReq, (/:), (=:))

import Control.Monad.IO.Class (MonadIO)
import Data.ByteString (StrictByteString)
import Database.BigQuery.Types (BigQueryTableData (BigQueryTableData), FromBigQuery (fromBigQuery), Schema)

getToken :: IO StrictByteString
getToken = do
    key <- getPrivateKey "service-account.json"
    jwt <-
        createSignedJWT
            "haskell-bigquery@things-394312.iam.gserviceaccount.com"
            Nothing
            ["https://www.googleapis.com/auth/cloud-platform"]
            key
    getAccessToken jwt

getDataset :: (MonadIO m) => StrictByteString -> Text -> Text -> m (JsonResponse Value)
getDataset token project dataset =
    let url =
            https
                "bigquery.googleapis.com"
                /: "bigquery"
                /: "v2"
                /: "projects"
                /: project
                /: "datasets"
                /: dataset
     in runReq
            defaultHttpConfig
            (req GET url NoReqBody jsonResponse (oAuth2Bearer token))

getTableSchema :: (MonadIO m) => StrictByteString -> Text -> Text -> Text -> m Schema
getTableSchema token project dataset table =
    let url =
            https
                "bigquery.googleapis.com"
                /: "bigquery"
                /: "v2"
                /: "projects"
                /: project
                /: "datasets"
                /: dataset
                /: "tables"
                /: table
     in responseBody <$> runReq defaultHttpConfig (req GET url NoReqBody jsonResponse (oAuth2Bearer token))

getTableData :: (MonadIO m, FromBigQuery a) => StrictByteString -> Text -> Text -> Text -> m [a]
getTableData token project dataset table =
    let url =
            https
                "bigquery.googleapis.com"
                /: "bigquery"
                /: "v2"
                /: "projects"
                /: project
                /: "datasets"
                /: dataset
                /: "tables"
                /: table
                /: "data"
        options = oAuth2Bearer token <> "maxResults" =: (10 :: Int)
     in do
            BigQueryTableData tableData <- responseBody <$> runReq defaultHttpConfig (req GET url NoReqBody jsonResponse options)
            pure $ fromBigQuery <$> tableData
