{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where

import Database.BigQuery
import Database.BigQuery.TH (mkBigQuery)

import Language.Haskell.TH (runIO)

-- import Data.Aeson.Encode.Pretty (encodePretty)
import Data.Text (unpack)
import Data.Vector ((!), Vector)

-- data Shakespeare = Shakespeare
--     { word :: String
--     , wordCount :: Int
--     , corpus :: String
--     , corpusDate :: Int
--     } deriving Show



$(do
    token <- runIO getToken
    mkBigQuery token "bigquery-public-data" "samples" "shakespeare"
    )

main :: IO ()
main = do
    undefined
    -- token <- getToken
    -- values :: [Shakespeare] <- getTableData token "bigquery-public-data" "samples" "shakespeare"
    -- print values
    


