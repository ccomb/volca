{-# LANGUAGE OverloadedStrings #-}

module MCPSchemaSpec (spec) where

import Data.Aeson (Value (..))
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KM
import Data.Text (Text)
import qualified Data.Text as T
import Test.Hspec

import API.MCP (toolDefinitions)

spec :: Spec
spec =
    describe "MCP tool schemas" $
        it "emit a valid JSON Schema shape for every tool" $
            mapM_ checkTool toolDefinitions

-- Every tool entry must be an object carrying 'name' and 'inputSchema', and
-- every property in its inputSchema must be a well-formed JSON Schema node.
-- In particular, arrays must declare 'items' — OpenAI's MCP validator rejects
-- the whole tool otherwise.
checkTool :: Value -> Expectation
checkTool (Object o) = do
    let name = case KM.lookup "name" o of
            Just (String n) -> n
            _ -> "<unknown>"
    case KM.lookup "inputSchema" o of
        Just schema -> checkSchema name schema
        Nothing ->
            expectationFailure $
                T.unpack name <> ": tool entry is missing 'inputSchema'"
checkTool v = expectationFailure $ "tool entry is not an object: " <> show v

checkSchema :: Text -> Value -> Expectation
checkSchema toolName (Object s) = case KM.lookup "properties" s of
    Just (Object props) -> mapM_ (checkProperty toolName) (KM.toList props)
    _ ->
        expectationFailure $
            T.unpack toolName <> ": inputSchema is missing 'properties'"
checkSchema toolName v =
    expectationFailure $
        T.unpack toolName <> ": inputSchema is not an object: " <> show v

checkProperty :: Text -> (KM.Key, Value) -> Expectation
checkProperty toolName (k, Object p) = case KM.lookup "type" p of
    Just (String "array") -> case KM.lookup "items" p of
        Just (Object _) -> return ()
        _ -> expectationFailure $ label <> " has type=array but no valid 'items' subschema"
    _ -> return ()
  where
    label = T.unpack toolName <> "." <> Key.toString k
checkProperty toolName (k, v) =
    expectationFailure $
        T.unpack toolName <> "." <> Key.toString k <> " is not an object: " <> show v
