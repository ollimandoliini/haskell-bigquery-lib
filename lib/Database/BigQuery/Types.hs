{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Database.BigQuery.Types where

import Data.Aeson (
    FromJSON,
    Value,
    withObject,
    withText,
    (.:),
    (.:?), withArray,
 )
import Data.Aeson.Types (Parser, parseJSON)
import Data.Map.Strict (Map, fromList, keys, (!?))
import Data.Text (Text, intercalate, unpack)
import Data.Vector (Vector)

data FieldMode
    = FieldModeNullable
    | FieldModeRequired
    | FieldModeRepeated
    deriving (Show)

-- | https://cloud.google.com/bigquery/docs/reference/rest/v2/tables#tablefieldschema
data FieldType
    = FieldTypeString
    | FieldTypeBytes
    | FieldTypeInteger
    | FieldTypeFloat
    | FieldTypeBoolean
    | FieldTypeTimestamp
    | FieldTypeDate
    | FieldTypeTime
    | FieldTypeDateTime
    | FieldTypeGeography
    | FieldTypeNumeric
    | FieldTypeBigNumeric
    | FieldTypeJson
    | FieldTypeRecord
    deriving (Show, Eq, Ord)

data Field = Field
    { fieldName :: Text
    , fieldMode :: FieldMode
    , fieldType :: FieldType
    , fieldNestedFields :: Maybe [Field]
    }
    deriving (Show)

newtype Schema = Schema
    { schemaFields :: [Field]
    }
    deriving (Show)

instance FromJSON Field where
    parseJSON = withObject "Field" $ \v ->
        Field
            <$> v .: "name"
            <*> v .: "mode"
            <*> v .: "type"
            <*> v .:? "fields"

instance FromJSON Schema where
    parseJSON = withObject "Schema" $ \v -> do
        x <- v .: "schema"
        Schema <$> x .: "fields"

instance FromJSON FieldMode where
    parseJSON = withText "FieldMode" $ \case
        "NULLABLE" -> pure FieldModeNullable
        "REPEATED" -> pure FieldModeRepeated
        "REQUIRED" -> pure FieldModeRequired
        s -> fail $ "Expected one of 'NULLABLE', 'REPEATED' OR 'REQUIRED'. Got: " <> unpack s

fieldTypeMapping :: Map Text FieldType
fieldTypeMapping =
    fromList
        [ ("STRING", FieldTypeString)
        , ("BYTES", FieldTypeBytes)
        , ("INTEGER", FieldTypeInteger)
        , ("INT64", FieldTypeInteger)
        , ("FLOAT", FieldTypeFloat)
        , ("FLOAT64", FieldTypeFloat)
        , ("BOOLEAN", FieldTypeBoolean)
        , ("BOOL", FieldTypeBoolean)
        , ("TIMESTAMP", FieldTypeTimestamp)
        , ("DATE", FieldTypeDate)
        , ("TIME", FieldTypeTime)
        , ("DATETIME", FieldTypeDateTime)
        , ("GEOGRAPHY", FieldTypeGeography)
        , ("NUMERIC", FieldTypeNumeric)
        , ("BIGNUMERIC", FieldTypeBigNumeric)
        , ("JSON", FieldTypeBigNumeric)
        , ("RECORD", FieldTypeRecord)
        , ("STRUCT", FieldTypeRecord)
        ]

fieldTypeToType :: Map FieldType String
fieldTypeToType =
    fromList
        [ (FieldTypeString, "String")
        , (FieldTypeBytes, "ByteString")
        , (FieldTypeInteger, "Int")
        , (FieldTypeInteger, "Int")
        , (FieldTypeFloat, "Double")
        , (FieldTypeFloat, "Double")
        , (FieldTypeBoolean, "Bool")
        , (FieldTypeBoolean, "Bool")
        -- , (FieldTypeTimestamp)
        -- , (FieldTypeDate)
        -- , (FieldTypeTime)
        -- , (FieldTypeDateTime)
        -- , (FieldTypeGeography)
        -- , (FieldTypeNumeric)
        -- , (FieldTypeBigNumeric)
        -- , (FieldTypeBigNumeric)
        -- , (FieldTypeRecord)
        -- , (FieldTypeRecord)
        ]

instance FromJSON FieldType where
    parseJSON = withText "FieldType" $ \text ->
        case fieldTypeMapping !? text of
            Just val -> pure val
            Nothing ->
                fail $
                    mconcat
                        [ "Could not parse field type. Expected one of: '"
                        , unpack (intercalate "," (keys fieldTypeMapping))
                        , "'. Got '"
                        , unpack text
                        , "'."
                        ]

class FromBigQuery a where
    fromBigQuery :: BigQueryTableRow -> a

newtype BigQueryTableRow = BigQueryTableRow (Vector Text) deriving (Show)

instance FromJSON BigQueryTableRow where
    parseJSON v = do
        row <- withObject "BigQueryTableRow" (.: "f") v
        BigQueryTableRow <$> withArray "row" parseField row
      where
        parseField :: Vector Value -> Parser (Vector Text)
        parseField = traverse (withObject "field" (\o -> (o .: "v") >>= withText "value" pure))

newtype BigQueryTableData = BigQueryTableData [BigQueryTableRow] deriving (Show)

instance FromJSON BigQueryTableData where
    parseJSON = withObject "TableData" $ \o -> BigQueryTableData <$> o .: "rows"
