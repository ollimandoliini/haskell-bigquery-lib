{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Database.BigQuery.TH where

import Data.ByteString (StrictByteString)
import Data.Char (toUpper)
import Data.Data (Proxy (Proxy))
import Data.Map.Strict qualified as Map ((!))
import Data.Text (Text)
import Data.Text qualified as T
import Data.Vector ((!))
import Database.BigQuery (getTableSchema)
import Database.BigQuery.Types (BigQueryTableRow (BigQueryTableRow), Field (Field, fieldType), FromBigQuery (fromBigQuery), Schema (Schema), fieldTypeToType, FieldType (..))
import Language.Haskell.TH (Bang (Bang), Clause (Clause), Con (..), Dec (DataD, DataInstD, FunD, InstanceD), Name, Pat (ConP, RecP), Q, SourceStrictness (NoSourceStrictness), SourceUnpackedness (NoSourceUnpackedness), Type (AppT, ConT), VarBangType, appE, appT, conE, conT, mkName, runIO, varE, lamE, varP, Exp)
import Data.Map.Strict (fromList)

-- createRecord :: String -> [String] -> Q [Dec]
-- createRecord typeName fieldNames =
--   [d| data $(recCName) = $(recCName) { $(fields) }
--     |]
--   where
--     recCName = conT (mkName typeName)
--     fields   = foldl appE (conE '()) (map (\name -> [|(mkName name, Bang NoSourceUnpackedness NoSourceStrictness, ConT ''Int)|]) fieldNames)

-- deriveCountableSimple :: Name -> Q [Dec]
-- deriveCountableSimple name = [d|
--   instance Countable $a where
--     count Proxy = fromIntegral $
--       1 + fromEnum (maxBound :: $a) - fromEnum (minBound :: $a)
--   |]
--   where
--     a = conT name

mkBigQueryType :: Text -> Schema -> Q [Dec]
mkBigQueryType typeName (Schema fields) =
    pure [DataD [] (mkName $ T.unpack typeName) [] Nothing [con] []]
  where
    con = RecC (mkName $ T.unpack typeName) (map fieldToVarBangType fields)
    bang = Bang NoSourceUnpackedness NoSourceStrictness
    fieldToVarBangType :: Field -> VarBangType
    fieldToVarBangType (Field fieldName fieldMode fieldType fieldNestedFields) = (mkName' fieldName, bang, ConT (mkName (fieldTypeToType Map.! fieldType)))

-- mkBigQueryDecoder :: Text -> Schema -> Q [Dec]
-- mkBigQueryDecoder typeName (Schema schemaFields) =
--   pure [InstanceD Nothing [] (appT (conT className) (conT typeName)) [funD]]
--   where
--       className = mkName "FromBigquery"
--       typeName = mkName


-- class DecodeBigQuery a where
--   decodeBigQuery :: Text -> a

-- instance DecodeBigQuery String where
--   decodeBigQuery = T.unpack

-- instance DecodeBigQuery Int where
--   decodeBigQuery = read . T.unpack




mkBigQueryDecoder :: Text -> Schema -> Q [Dec]
mkBigQueryDecoder typeName (Schema schemaFields) =
    [d|
        instance FromBigQuery $typeCon where
            fromBigQuery (BigQueryTableRow fields) =
                $( foldl appE (conE $ mkName' typeName) $
                    zipWith (\i field -> [| $(getLambda $ fieldType field) (fields ! i)|]) [0 :: Int ..] schemaFields
                 )
        |]
  where
    typeCon = conT . mkName' $ typeName

    getLambda :: FieldType -> Q Exp
    getLambda FieldTypeString = lamE [varP (mkName' "input")] undefined
    getLambda _ = error "foo"

mkBigQuery :: StrictByteString -> Text -> Text -> Text -> Q [Dec]
mkBigQuery token project dataset table = do
    schema <- runIO $ getTableSchema token project dataset table
    let name = snakeToPascal table
    bqType <- mkBigQueryType name schema
    bqDecoder <- mkBigQueryDecoder name schema
    pure (bqType <> bqDecoder)

mkName' :: Text -> Name
mkName' = mkName . T.unpack . snakeToCamel

snakeToPascal :: Text -> Text
snakeToPascal = T.concat . map capitalize . T.splitOn "_"
  where
    capitalize txt = case T.uncons txt of
        Nothing -> ""
        Just (x, xs) -> T.cons (toUpper x) xs

snakeToCamel :: Text -> Text
snakeToCamel txt =
    let parts = T.splitOn "_" txt
     in case parts of
            [] -> ""
            x : xs -> x <> T.concat (map capitalize xs)
  where
    capitalize segment = case T.uncons segment of
        Nothing -> ""
        Just (h, t) -> T.cons (toUpper h) t