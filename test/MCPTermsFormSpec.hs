{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

{- | A database's terms of use, asked of the person in a form their client
shows, when the client says it shows forms; asked through the assistant, in
the conversation, otherwise.

The fixture is the root and dependency of "DependencyLicenceSpec": the root
buys from the dependency, whose publisher asks its terms accepted, so reading
the root asks the dependency's terms.
-}
module MCPTermsFormSpec (spec) where

import Data.Aeson (Key, Value (..), decode, encode, object, (.=))
import Data.Aeson.Key (fromText)
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString.Builder as BB
import qualified Data.ByteString.Lazy as BL
import Data.IORef
import qualified Data.List.NonEmpty as NE
import Data.Maybe (fromMaybe)
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Network.HTTP.Types (RequestHeaders)
import Network.Wai (defaultRequest, requestHeaders, requestMethod, setRequestBodyChunks)
import Network.Wai.Internal (Response (..), ResponseReceived (..))
import Test.Hspec

import API.MCP (Era (..), Refusal (..), RequestId (..), RpcRequest (..), TermsOutcome (..), askingOf, mcpApp, modernVersion, termsStep, toolAnswered)
import qualified Database.Manager as DM
import DependencyLicenceSpec (dependency, managerOn, root)
import Types (Acceptance (..), Attribution (..), Licence (..), OwnLicence (..), Permission (..), Terms (..), UsageCount (..), processIdToText, termsAsked)
import Usage (Asking (..))

-- | Terms the dependency's publisher asks accepted, refusing what the given permissions open.
asking :: [Permission] -> Licence
asking refused = LicenceOwn OwnLicence{ownText = "Read with care.", ownRefused = S.fromList refused, ownAttribution = AttributionRequired, ownAcceptance = AcceptanceAsked NotCounted}

theTerms :: Licence -> Terms
theTerms licence = case termsAsked [("dep", licence)] of
    [t] -> t
    _ -> error "the licence asks no terms"

formKey :: Licence -> Key
formKey licence = fromText ("terms-" <> termsFingerprint (theTerms licence))

-- | Who calls: a client of the old era, or of the modern era declaring these capabilities.
data Client = OldClient | ModernClient Value

-- | A client of the modern era that shows forms.
showsForms :: Client
showsForms = ModernClient (object ["elicitation" .= object []])

-- | One tools/call, with the answers to forms if any; the result, or Null.
call :: DM.DatabaseManager -> Client -> [(Key, Value)] -> Text -> [(Key, Value)] -> IO Value
call = callWith []

-- | One tools/call with more headers.
callWith :: RequestHeaders -> DM.DatabaseManager -> Client -> [(Key, Value)] -> Text -> [(Key, Value)] -> IO Value
callWith extra manager client answers tool args = do
    app <- mcpApp manager [] [] False Nothing Nothing id Nothing
    let (meta, headers) = case client of
            OldClient -> ([], [])
            ModernClient caps ->
                ( ["_meta" .= object ["io.modelcontextprotocol/protocolVersion" .= modernVersion, "io.modelcontextprotocol/clientCapabilities" .= caps]]
                , [("MCP-Protocol-Version", TE.encodeUtf8 modernVersion), ("Mcp-Method", "tools/call"), ("Mcp-Name", TE.encodeUtf8 tool)]
                )
        responses = ["inputResponses" .= object answers | not (null answers)]
        body = encode (object ["jsonrpc" .= ("2.0" :: Text), "id" .= (1 :: Int), "method" .= ("tools/call" :: Text), "params" .= object (["name" .= tool, "arguments" .= object args] <> meta <> responses)])
    chunks <- newIORef (BL.toChunks body)
    let next = atomicModifyIORef' chunks $ \case
            [] -> ([], mempty)
            (c : rest) -> (rest, c)
    ref <- newIORef Null
    _ <- app (setRequestBodyChunks next defaultRequest{requestMethod = "POST", requestHeaders = extra <> headers}) $ \case
        ResponseBuilder _ _ b -> writeIORef ref (fromMaybe Null (decode (BB.toLazyByteString b) >>= at ["result"])) >> pure ResponseReceived
        _ -> pure ResponseReceived
    readIORef ref

at :: [Key] -> Value -> Maybe Value
at [] v = Just v
at (k : ks) (Object o) = KM.lookup k o >>= at ks
at _ _ = Nothing

textOf :: Value -> Text
textOf r = case at ["content"] r of
    Just (Array cs) | (Object c : _) <- foldr (:) [] cs, Just (String t) <- KM.lookup "text" c -> t
    _ -> ""

resultType :: Value -> Maybe Value
resultType = at ["resultType"]

searchRoot :: [(Key, Value)]
searchRoot = ["database" .= ("root" :: Text), "name" .= ("act" :: Text)]

-- | An answer to a form: what the person did.
answer :: Text -> Value
answer action = object ["action" .= action]

accepted :: DM.DatabaseManager -> IO (S.Set Text)
accepted manager = DM.acceptedTerms manager Nothing

spec :: Spec
spec = do
    let licence = asking []
        fingerprint = termsFingerprint (theTerms licence)
    describe "a client that shows no form" $ do
        it "of the old era is refused with the terms, to show in the conversation" $ do
            manager <- managerOn root dependency licence
            r <- call manager OldClient [] "search_activities" searchRoot
            at ["isError"] r `shouldBe` Just (Bool True)
            textOf r `shouldSatisfy` T.isInfixOf "call accept_terms"

        it "of the modern era, saying nothing of forms, is refused the same way" $ do
            manager <- managerOn root dependency licence
            r <- call manager (ModernClient (object [])) [] "search_activities" searchRoot
            textOf r `shouldSatisfy` T.isInfixOf "call accept_terms"

    describe "a client that shows forms" $ do
        it "is asked to show the terms in full, with no field to fill, naming the database asked for" $ do
            manager <- managerOn root dependency licence
            r <- call manager showsForms [] "search_activities" searchRoot
            resultType r `shouldBe` Just (String "input_required")
            let form = at ["inputRequests", formKey licence, "params"] r
            (form >>= at ["mode"]) `shouldBe` Just (String "form")
            case form >>= at ["message"] of
                Just (String m) -> do
                    m `shouldSatisfy` T.isInfixOf "Read with care."
                    m `shouldSatisfy` T.isInfixOf "Reading root reads dep"
                other -> expectationFailure ("no message: " <> show other)
            (form >>= at ["requestedSchema", "properties"]) `shouldBe` Just (object [])
            accepted manager `shouldReturn` S.empty

        it "runs the tool once the person accepts, and asks no more" $ do
            manager <- managerOn root dependency licence
            r <- call manager showsForms [(formKey licence, answer "accept")] "search_activities" searchRoot
            resultType r `shouldBe` Just (String "complete")
            at ["isError"] r `shouldNotBe` Just (Bool True)
            accepted manager `shouldReturn` S.singleton fingerprint
            again <- call manager showsForms [] "search_activities" searchRoot
            resultType again `shouldBe` Just (String "complete")

        it "is refused, with nothing to get around, when the person declines" $ do
            manager <- managerOn root dependency licence
            r <- call manager showsForms [(formKey licence, answer "decline")] "search_activities" searchRoot
            at ["isError"] r `shouldBe` Just (Bool True)
            textOf r `shouldSatisfy` T.isInfixOf "did not accept"
            textOf r `shouldNotSatisfy` T.isInfixOf "accept_terms"
            accepted manager `shouldReturn` S.empty

        it "is refused when the form is closed with no answer, naming the cookie a client with no screen accepts by" $ do
            manager <- managerOn root dependency licence
            r <- callWith [("Cookie", "volca_terms=f0")] manager showsForms [(formKey licence, answer "cancel")] "search_activities" searchRoot
            at ["isError"] r `shouldBe` Just (Bool True)
            textOf r `shouldSatisfy` T.isInfixOf "closed with no answer"
            -- The cookie keeps the text it already opened.
            textOf r `shouldSatisfy` T.isInfixOf ("volca_terms=" <> T.intercalate "." (S.toList (S.fromList ["f0", fingerprint])))
            textOf r `shouldNotSatisfy` T.isInfixOf "accept_terms"
            accepted manager `shouldReturn` S.empty

        it "asks again when the answer is for another text" $ do
            manager <- managerOn root dependency licence
            r <- call manager showsForms [("terms-older", answer "accept")] "search_activities" searchRoot
            resultType r `shouldBe` Just (String "input_required")
            accepted manager `shouldReturn` S.empty

        it "is refused a tool its licence withholds before any form" $ do
            let withholding = asking [ReadInventory]
            manager <- managerOn root dependency withholding
            r <- call manager showsForms [] "get_inventory" ["database" .= ("dep" :: Text), "process_id" .= processIdToText dependency 0]
            at ["isError"] r `shouldBe` Just (Bool True)
            at ["inputRequests"] r `shouldBe` Nothing

    describe "accept_terms" $ do
        it "shows the form to a client that shows forms, and records only the person's answer" $ do
            manager <- managerOn root dependency licence
            asked <- call manager showsForms [] "accept_terms" ["database" .= ("dep" :: Text)]
            resultType asked `shouldBe` Just (String "input_required")
            accepted manager `shouldReturn` S.empty
            _ <- call manager showsForms [(formKey licence, answer "accept")] "accept_terms" ["database" .= ("dep" :: Text)]
            accepted manager `shouldReturn` S.singleton fingerprint

        it "records at once for a client of the old era, as it always did" $ do
            manager <- managerOn root dependency licence
            _ <- call manager OldClient [] "accept_terms" ["database" .= ("dep" :: Text)]
            accepted manager `shouldReturn` S.singleton fingerprint

    describe "askingOf" $ do
        let req caps = RpcRequest (Just (RequestId (Number 1))) "tools/call" (Just (object ["_meta" .= object ["io.modelcontextprotocol/clientCapabilities" .= caps]]))
            isForm = \case
                InForm _ -> True
                InChat -> False
        it "asks in a form a modern client that shows forms, said outright or by an empty capability" $ do
            isForm (askingOf Modern (req (object ["elicitation" .= object []]))) `shouldBe` True
            isForm (askingOf Modern (req (object ["elicitation" .= object ["form" .= object []]]))) `shouldBe` True

        it "asks in the conversation a client that opens links only, says nothing, or is of the old era" $ do
            isForm (askingOf Modern (req (object ["elicitation" .= object ["url" .= object []]]))) `shouldBe` False
            isForm (askingOf Modern (req (object []))) `shouldBe` False
            isForm (askingOf Legacy (req (object ["elicitation" .= object []]))) `shouldBe` False

    describe "termsStep" $ do
        let one = theTerms licence
            other = one{termsDatabase = "other", termsFingerprint = "f2"}
            yes = answer "accept"
        it "asks every text again while one is unanswered, so none is recorded alone" $
            termsStep (InForm (KM.fromList [(fromText ("terms-" <> fingerprint), yes)])) [one, other] `shouldBe` AskInForm (one NE.:| [other])

        it "goes on when every text is accepted, and when none is missing" $ do
            termsStep (InForm (KM.fromList [(fromText ("terms-" <> fingerprint), yes), ("terms-f2", yes)])) [one, other] `shouldBe` AcceptedAll (one NE.:| [other])
            termsStep InChat [] `shouldBe` NothingMissing

        it "names only the terms the person refused" $
            termsStep (InForm (KM.fromList [(fromText ("terms-" <> fingerprint), yes), ("terms-f2", answer "decline")])) [one, other] `shouldBe` Declined SaidNo (other NE.:| [])

    describe "toolAnswered" $
        it "does not count a question to the person as an answer" $ do
            toolAnswered (object ["result" .= object ["resultType" .= ("input_required" :: Text)]]) `shouldBe` False
            toolAnswered (object ["result" .= object ["resultType" .= ("complete" :: Text)]]) `shouldBe` True
