{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

{- | An added line whose amount the assistant does not know: the person types
it in a form when the client shows forms, and the history says so.
-}
module MCPTypedAmountsSpec (spec) where

import Control.Monad (void)
import Data.Aeson (Key, Value (..), object, (.=))
import qualified Data.Aeson.KeyMap as KM
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.Map.Strict as M
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import Servant (runHandler)
import Test.Hspec

import API.DatabaseHandlers (databaseHistoryHandler)
import API.MCP (AmountsOutcome (..), EditShape (..), Missing (..), Place (..), Refusal (..), amountsStep, callTool, fillAmounts, missingAmounts, noRequestId)
import API.Types (AddedList (..), DatabaseHistoryEntry (..))
import ActivityWriteHandlerSpec (buildFixture, cheese, create, keyOf, supplierPid, uploadedConfig, withDb, withWritableDb)
import App.Env (AppEnv (..), runApp)
import Config (DatabaseConfig (..))
import qualified Data.Set as S
import Types (Acceptance (..), Attribution (..), Licence (..), OwnLicence (..), UsageCount (..))
import Usage (Asking (..), Caller (..), anonymous)

-- | One call of a tool on the writable database, asked as a client that shows forms or not.
call :: AppEnv -> Asking -> Text -> [(Key, Value)] -> IO Value
call env asking name args =
    callTool anonymous{callerAsking = asking} (aeDbManager env) [] Nothing Nothing noRequestId name (KM.fromList (("database", String "authored") : args))

-- | Adding milk to the cheese, with or without an amount.
addMilk :: [(Key, Value)] -> [(Key, Value)]
addMilk line = [("process_id", String (keyOf cheese)), ("add_inputs", toArray [object (("provider" .= supplierPid) : line)])]

toArray :: [Value] -> Value
toArray = Array . foldMap pure

showsForms :: Asking
showsForms = InForm mempty

-- | The answer to the amounts form, with what was typed in each field.
typed :: Text -> [(Key, Value)] -> Asking
typed action fields = InForm (KM.fromList [("amounts", object (("action" .= action) : ["content" .= object fields | action == "accept"]))])

at :: [Key] -> Value -> Maybe Value
at [] v = Just v
at (k : ks) (Object o) = KM.lookup k o >>= at ks
at _ _ = Nothing

result :: Value -> Value
result r = fromMaybe r (at ["result"] r)

textOf :: Value -> Text
textOf r = case at ["content"] (result r) of
    Just (Array xs) -> T.concat [t | Object o <- foldr (:) [] xs, Just (String t) <- [KM.lookup "text" o]]
    _ -> ""

isError :: Value -> Bool
isError r = at ["isError"] (result r) == Just (Bool True)

details :: AppEnv -> IO [Text]
details env = either (const []) (concatMap dheDetails) <$> runHandler (runApp env (databaseHistoryHandler "authored"))

asObject :: Value -> Maybe (KM.KeyMap Value)
asObject = \case
    Object o -> Just o
    _ -> Nothing

-- | Terms the database's publisher asks accepted before anyone reads it.
askedTerms :: Licence
askedTerms = LicenceOwn OwnLicence{ownText = "Read with care.", ownRefused = S.empty, ownAttribution = AttributionRequired, ownAcceptance = AcceptanceAsked NotCounted}

withCheese :: (AppEnv -> IO ()) -> IO ()
withCheese act = withWritableDb $ \env -> do
    void (create env "authored" [cheese])
    act env

spec :: Spec
spec = do
    describe "an added line with no amount" $ do
        it "is refused to a client that shows no form, naming the line, with nothing written" $
            withCheese $ \env -> do
                before <- details env
                r <- call env InChat "edit_exchanges" (addMilk [])
                isError r `shouldBe` True
                textOf r `shouldSatisfy` T.isInfixOf "milk production (FR), kg"
                textOf r `shouldSatisfy` T.isInfixOf "Ask the user"
                details env `shouldReturn` before

        it "takes a null amount as one left out" $
            withCheese $ \env -> do
                r <- call env InChat "edit_exchanges" (addMilk ["amount" .= Null])
                textOf r `shouldSatisfy` T.isInfixOf "These lines have no amount"

        it "asks a client that shows forms for the amount, one number field per line" $
            withCheese $ \env -> do
                r <- call env showsForms "edit_exchanges" (addMilk [])
                at ["resultType"] (result r) `shouldBe` Just (String "input_required")
                let form = at ["inputRequests", "amounts", "params"] (result r)
                (form >>= at ["requestedSchema", "properties", "add_inputs-0", "type"]) `shouldBe` Just (String "number")
                (form >>= at ["requestedSchema", "properties", "add_inputs-0", "title"]) `shouldBe` Just (String "milk production (FR), kg")
                (form >>= at ["requestedSchema", "required"]) `shouldBe` Just (toArray [String "add_inputs-0"])
                case form >>= at ["message"] of
                    Just (String m) -> m `shouldSatisfy` T.isInfixOf "cheese, at dairy (FR)"
                    other -> expectationFailure ("no message: " <> show other)

        it "adds the line with the typed amount, and the history says the person typed it" $
            withCheese $ \env -> do
                r <- call env (typed "accept" [("add_inputs-0", Number 0.42)]) "edit_exchanges" (addMilk [])
                isError r `shouldBe` False
                textOf r `shouldSatisfy` T.isInfixOf "The user typed these amounts"
                textOf r `shouldSatisfy` T.isInfixOf "milk production (FR), kg, added to cheese, at dairy (FR): 0.42"
                lines' <- details env
                filter (T.isInfixOf "typed by the user") lines' `shouldSatisfy` ((== 1) . length)
                filter (T.isInfixOf "0.42") lines' `shouldSatisfy` (not . null)

        it "does not mark a line whose amount the call gave" $
            withCheese $ \env -> do
                r <- call env showsForms "edit_exchanges" (addMilk ["amount" .= (2 :: Int)])
                textOf r `shouldSatisfy` (not . T.isInfixOf "The user typed")
                lines' <- details env
                filter (T.isInfixOf "typed by the user") lines' `shouldBe` []

        it "asks again, saying why, when the person typed zero or nothing" $
            withCheese $ \env -> do
                zero <- call env (typed "accept" [("add_inputs-0", Number 0)]) "edit_exchanges" (addMilk [])
                at ["resultType"] (result zero) `shouldBe` Just (String "input_required")
                case at ["inputRequests", "amounts", "params", "message"] (result zero) of
                    Just (String m) -> m `shouldSatisfy` T.isInfixOf "other than zero"
                    other -> expectationFailure ("no message: " <> show other)
                none <- call env (typed "accept" []) "edit_exchanges" (addMilk [])
                at ["resultType"] (result none) `shouldBe` Just (String "input_required")

        it "changes nothing when the person declines or closes the form" $
            withCheese $ \env -> do
                before <- details env
                no <- call env (typed "decline" []) "edit_exchanges" (addMilk [])
                textOf no `shouldSatisfy` T.isInfixOf "did not give the amounts"
                closed <- call env (typed "cancel" []) "edit_exchanges" (addMilk [])
                textOf closed `shouldSatisfy` T.isInfixOf "source"
                details env `shouldReturn` before

        it "refuses before any form an edit refused whatever the amount" $
            withCheese $ \env -> do
                r <- call env showsForms "edit_exchanges" [("process_id", String (keyOf cheese)), ("add_inputs", toArray [object ["provider" .= ("nope" :: Text)]])]
                isError r `shouldBe` True
                at ["inputRequests"] (result r) `shouldBe` Nothing
                textOf r `shouldSatisfy` T.isInfixOf "unknown supplier"

        it "asks once for the lines of several processes, each field naming its process" $
            withCheese $ \env -> do
                let element = object ["process_id" .= keyOf cheese, "add_inputs" .= toArray [object ["provider" .= supplierPid]]]
                    other = object ["process_id" .= supplierPid, "set_name" .= ("milk, raw" :: Text)]
                r <- call env showsForms "edit_activities" [("edits", toArray [other, element])]
                let props = at ["inputRequests", "amounts", "params", "requestedSchema", "properties"] (result r)
                (props >>= at ["1-add_inputs-0", "description"]) `shouldBe` Just (String "Added to cheese, at dairy (FR)")
                done <- call env (typed "accept" [("1-add_inputs-0", Number 3)]) "edit_activities" [("edits", toArray [other, element])]
                isError done `shouldBe` False
                lines' <- details env
                filter (T.isInfixOf "typed by the user") lines' `shouldSatisfy` ((== 1) . length)

        it "asks only the lines left out, each by its own place, and marks only those" $
            withCheese $ \env -> do
                let nitrous = object ["direction" .= ("emission" :: Text), "name" .= ("Nitrous oxide" :: Text), "compartment" .= ("air" :: Text), "unit" .= ("kg" :: Text)]
                    args =
                        [ ("process_id", String (keyOf cheese))
                        , ("add_inputs", toArray [object ["provider" .= supplierPid, "amount" .= (2 :: Int)], object ["provider" .= supplierPid]])
                        , ("add_biosphere", toArray [nitrous])
                        ]
                r <- call env showsForms "edit_exchanges" args
                let props = at ["inputRequests", "amounts", "params", "requestedSchema", "properties"] (result r)
                fmap KM.keys (props >>= asObject) `shouldBe` Just ["add_biosphere-0", "add_inputs-1"]
                (props >>= at ["add_inputs-1", "title"]) `shouldBe` Just (String "milk production (FR), kg")
                (props >>= at ["add_biosphere-0", "title"]) `shouldSatisfy` maybe False (\case String t -> "Nitrous oxide" `T.isInfixOf` t; _ -> False)
                done <- call env (typed "accept" [("add_inputs-1", Number 7), ("add_biosphere-0", Number 0.03)]) "edit_exchanges" args
                isError done `shouldBe` False
                lines' <- details env
                filter (T.isInfixOf "typed by the user") lines' `shouldSatisfy` ((== 2) . length)
                filter (\l -> "2.0" `T.isInfixOf` l && "typed by the user" `T.isInfixOf` l) lines' `shouldBe` []

        it "refuses before any form what edit_activities refuses: a note on one element" $
            withCheese $ \env -> do
                let element = object ["process_id" .= keyOf cheese, "note" .= ("measured" :: Text), "add_inputs" .= toArray [object ["provider" .= supplierPid]]]
                r <- call env showsForms "edit_activities" [("edits", toArray [element])]
                isError r `shouldBe` True
                at ["inputRequests"] (result r) `shouldBe` Nothing
                textOf r `shouldSatisfy` T.isInfixOf "a note is given once"

        it "refuses the elements of edit_activities in the words the tool uses, each named by its process" $
            withCheese $ \env -> do
                let bogus = toArray [object ["kind" .= ("bogus" :: Text)]]
                    first' = object ["process_id" .= supplierPid, "remove" .= bogus]
                    second' = object ["process_id" .= keyOf cheese, "remove" .= bogus, "add_inputs" .= toArray [object ["provider" .= supplierPid]]]
                r <- call env showsForms "edit_activities" [("edits", toArray [first', second'])]
                at ["inputRequests"] (result r) `shouldBe` Nothing
                textOf r `shouldSatisfy` T.isInfixOf supplierPid
                textOf r `shouldSatisfy` T.isInfixOf (keyOf cheese)

        it "comes after the database's terms: accepted once, then the amounts, then the edit" $
            withDb buildFixture (\name dir -> (uploadedConfig name dir){dcLicence = askedTerms}) $ \env -> do
                void (create env "authored" [cheese])
                first' <- call env showsForms "edit_exchanges" (addMilk [])
                termsAnswer <- case at ["inputRequests"] (result first') >>= asObject of
                    Just forms | [k] <- KM.keys forms, k /= "amounts" -> pure (KM.singleton k (object ["action" .= ("accept" :: Text)]))
                    other -> expectationFailure ("no terms form: " <> show other) >> pure mempty
                second <- call env (InForm termsAnswer) "edit_exchanges" (addMilk [])
                fmap KM.keys (at ["inputRequests"] (result second) >>= asObject) `shouldBe` Just ["amounts"]
                third <- call env (typed "accept" [("add_inputs-0", Number 5)]) "edit_exchanges" (addMilk [])
                isError third `shouldBe` False
                lines' <- details env
                filter (T.isInfixOf "typed by the user") lines' `shouldSatisfy` ((== 1) . length)

    describe "the amounts left out of a call" $ do
        let args = KM.fromList [("add_inputs", toArray [object ["provider" .= ("a" :: Text)], object ["provider" .= ("b" :: Text), "amount" .= (1 :: Int)]]), ("add_biosphere", toArray [object ["amount" .= Null]])]
        it "are the lines with no amount or a null one" $
            missingAmounts OneEdit args `shouldBe` [Place Nothing AddInputs 0, Place Nothing AddBiosphere 0]

        it "are filled where they were left out, and nowhere else" $ do
            let filled = fillAmounts (M.fromList [(Place Nothing AddInputs 0, 5)]) args
            missingAmounts OneEdit filled `shouldBe` [Place Nothing AddBiosphere 0]

    describe "amountsStep" $ do
        let one = Missing{missingPlace = Place Nothing AddInputs 0, missingProcess = "cheese (FR)", missingLine = "milk (FR), kg"}
        it "asks in the conversation a client that shows no form" $
            amountsStep InChat (one :| []) `shouldBe` AmountsInChat (one :| [])
        it "takes the typed numbers" $
            amountsStep (typed "accept" [("add_inputs-0", Number 2)]) (one :| []) `shouldBe` Typed (M.fromList [(Place Nothing AddInputs 0, 2)])
        it "stops when the person declines" $
            amountsStep (typed "decline" []) (one :| []) `shouldBe` AmountsRefused SaidNo
