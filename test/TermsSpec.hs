{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

{- | A database whose publisher asks its readers to accept its terms first.

The dependency of the root is served under such terms: reading the root reads
the dependency's data, so it is refused until the reader accepts them, over
HTTP with the fingerprint of the text in a cookie, and through the MCP tool
accept_terms.
-}
module TermsSpec (spec) where

import Data.Aeson (Value (..), decode, encode, object, toJSON, (.=))
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KM
import Data.ByteString (ByteString)
import Data.ByteString.Builder (toLazyByteString)
import qualified Data.ByteString.Lazy as BL
import Data.Foldable (toList)
import Data.IORef (atomicModifyIORef', newIORef, readIORef, writeIORef)
import Data.Maybe (fromMaybe, isJust, listToMaybe)
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.UUID as UUID
import Network.HTTP.Types (Method, methodGet, methodPost, status200, statusCode)
import Network.HTTP.Types.Header (HeaderName, hContentType, hCookie)
import Network.Wai (Request (..), defaultRequest, responseLBS, setRequestBodyChunks, strictRequestBody)
import Network.Wai.Internal (Response (..), ResponseReceived (..))
import Test.Hspec

import API.MCP (callTool, noRequestId)
import API.Terms (acceptedFromCookie, databasesNamed, termsMiddleware)
import DependencyLicenceSpec (climate, collection, dependency, managerOn, root, rootPid)
import qualified Method.Types as M
import Types
import Usage (Caller (..))

termsOf :: Text -> Licence
termsOf text = LicenceOwn OwnLicence{ownText = text, ownRefused = S.empty, ownAttribution = AttributionRequired, ownAcceptance = AcceptanceAsked NotCounted}

keys :: LicenceKeys
keys = LicenceKeys Nothing Nothing Nothing Nothing Nothing Nothing

fingerprintOf :: Licence -> Text
fingerprintOf = fromMaybe (error "no fingerprint") . licenceFingerprint

spec :: Spec
spec = do
    describe "the accept key" $ do
        it "asks an own licence's readers to accept it" $
            licenceFromKeys keys{lkText = Just "Ours", lkAccept = Just True} `shouldBe` Right (termsOf "Ours")

        it "asks nothing when the publisher says nothing" $
            (ownAcceptance <$> own (licenceFromKeys keys{lkText = Just "Ours"})) `shouldBe` Right NoAcceptance

        it "belongs to an own licence" $ do
            licenceFromKeys keys{lkAccept = Just True} `shouldSatisfy` isLeft'
            licenceFromKeys keys{lkId = Just "CC-BY-4.0", lkAccept = Just True} `shouldSatisfy` isLeft'

        it "is written back as it was read" $
            licenceFromKeys (licenceKeys (termsOf "Ours")) `shouldBe` Right (termsOf "Ours")

        it "travels on the wire, with the fingerprint a reader presents" $ do
            let wire = toJSON (termsOf "Ours")
            decode (encode wire) `shouldBe` Just (termsOf "Ours")
            field "acceptance" wire `shouldBe` Just (String (fingerprintOf (termsOf "Ours")))

        it "is refused on a standard licence, which is not adjusted" $
            (decode (encode (object ["kind" .= ("standard" :: Text), "id" .= ("CC-BY-4.0" :: Text), "accept" .= True])) :: Maybe Licence) `shouldBe` Nothing

    describe "the usage key" $ do
        it "counts the computations of readers who accept the terms" $
            (licenceUsage <$> licenceFromKeys keys{lkText = Just "Ours", lkAccept = Just True, lkUsage = Just "organizations"}) `shouldBe` Right (Counted ReadersNamed)

        it "counts nothing when the terms say nothing" $
            licenceUsage (termsOf "Ours") `shouldBe` NotCounted

        it "is refused without acceptance, which would count readers who never saw it" $ do
            licenceFromKeys keys{lkText = Just "Ours", lkUsage = Just "anonymous"} `shouldSatisfy` isLeft'
            licenceFromKeys keys{lkText = Just "Ours", lkAccept = Just False, lkUsage = Just "anonymous"} `shouldSatisfy` isLeft'

        it "is refused when it names no known counting" $
            licenceFromKeys keys{lkText = Just "Ours", lkAccept = Just True, lkUsage = Just "everyone"} `shouldSatisfy` isLeft'

        it "is written back as it was read" $ do
            let counted = LicenceOwn OwnLicence{ownText = "Ours", ownRefused = S.empty, ownAttribution = AttributionRequired, ownAcceptance = AcceptanceAsked (Counted ReadersAnonymous)}
            licenceFromKeys (licenceKeys counted) `shouldBe` Right counted
            decode (encode counted) `shouldBe` Just counted

        it "is refused on a standard licence, which is not adjusted" $
            (decode (encode (object ["kind" .= ("standard" :: Text), "id" .= ("CC-BY-4.0" :: Text), "usage" .= ("anonymous" :: Text)])) :: Maybe Licence) `shouldBe` Nothing

        it "is shown with the terms a reader accepts" $
            map termsUsage (termsAsked [("ei", LicenceOwn OwnLicence{ownText = "Ours", ownRefused = S.empty, ownAttribution = AttributionRequired, ownAcceptance = AcceptanceAsked (Counted ReadersNamed)})]) `shouldBe` [Just ReadersNamed]

    describe "licenceFingerprint" $ do
        it "changes with the text, so new terms are asked again" $
            fingerprintOf (termsOf "Ours") `shouldNotBe` fingerprintOf (termsOf "Ours, revised")

        it "stays the hash of the text alone when nothing is counted, so earlier acceptances hold" $
            fingerprintOf (termsOf "Ours") `shouldBe` "db031768f8774819"

        it "changes with the counting, so a reader accepts a new clause again" $ do
            let counting usage = LicenceOwn OwnLicence{ownText = "Ours", ownRefused = S.empty, ownAttribution = AttributionRequired, ownAcceptance = AcceptanceAsked usage}
            fingerprintOf (counting (Counted ReadersAnonymous)) `shouldNotBe` fingerprintOf (counting NotCounted)
            fingerprintOf (counting (Counted ReadersNamed)) `shouldNotBe` fingerprintOf (counting (Counted ReadersAnonymous))

        it "is absent when the licence asks nothing" $ do
            licenceFingerprint (LicenceStandard CCBY) `shouldBe` Nothing
            licenceFingerprint (LicenceOwn OwnLicence{ownText = "Ours", ownRefused = S.empty, ownAttribution = AttributionRequired, ownAcceptance = NoAcceptance}) `shouldBe` Nothing

    describe "termsAsked" $
        it "asks a text once, however many databases are served under it" $
            map termsDatabase (termsAsked [("bafu", termsOf "Ours"), ("my-bafu", termsOf "Ours"), ("open", LicenceStandard CC0)]) `shouldBe` ["bafu"]

    describe "acceptedFromCookie" $ do
        it "reads the fingerprints among the other cookies" $
            acceptedFromCookie (Just "volca_session=x; volca_terms=aaa.bbb") `shouldBe` S.fromList ["aaa", "bbb"]

        it "reads none without the cookie" $
            acceptedFromCookie (Just "volca_session=x") `shouldBe` S.empty

    describe "databasesNamed" $ do
        it "reads the database of the path" $
            databasesNamed methodGet ["api", "v1", "db", "bafu", "activities"] [] Nothing `shouldBe` ["bafu"]

        it "reads none to manage a database" $ do
            databasesNamed methodGet ["api", "v1", "db", "bafu", "setup"] [] Nothing `shouldBe` []
            databasesNamed methodPost ["api", "v1", "db", "bafu", "load"] [] Nothing `shouldBe` []

        it "reads the database compared with" $
            databasesNamed methodGet ["api", "v1", "db", "mine", "compare"] [("other_database", Just "bafu")] Nothing `shouldBe` ["mine", "bafu"]

        it "reads a database a substitution reaches into" $
            databasesNamed methodPost ["api", "v1", "db", "mine", "activity", "p", "inventory"] [] (Just (object ["substitutions" .= [object ["from" .= ("p1" :: Text), "to" .= ("bafu::p2" :: Text)]]]))
                `shouldBe` ["mine", "bafu"]

    describe "termsMiddleware" $ do
        let terms = [Terms "bafu" "Ours" "f1" Nothing]
            asked names = pure (if "bafu" `elem` names then terms else [])
        it "refuses a reader who has not accepted the terms, with them" $ do
            (status, body) <- through asked methodGet ["api", "v1", "db", "bafu", "activities"] [] ""
            status `shouldBe` 403
            (field "terms" =<< decode body) `shouldBe` Just (toJSON terms)

        it "lets through a reader who accepted them" $
            fst <$> through asked methodGet ["api", "v1", "db", "bafu", "activities"] [(hCookie, "volca_terms=f1")] "" `shouldReturn` 200

        it "refuses a reader who accepted an older text" $
            fst <$> through asked methodGet ["api", "v1", "db", "bafu", "activities"] [(hCookie, "volca_terms=f0")] "" `shouldReturn` 403

        it "lets the setup of a database through, which reads none of its data" $
            fst <$> through asked methodGet ["api", "v1", "db", "bafu", "setup"] [] "" `shouldReturn` 200

        it "hands a body it read on to the route" $ do
            let body = encode (object ["substitutions" .= [object ["from" .= ("p1" :: Text), "to" .= ("other::p2" :: Text)]]])
            (status, echoed) <- through asked methodPost ["api", "v1", "db", "mine", "activity", "p", "inventory"] [(hContentType, "application/json")] body
            (status, echoed) `shouldBe` (200, body)

        it "refuses a substitution into a database whose terms are missing" $ do
            let body = encode (object ["substitutions" .= [object ["from" .= ("p1" :: Text), "to" .= ("bafu::p2" :: Text)]]])
            fst <$> through asked methodPost ["api", "v1", "db", "mine", "activity", "p", "inventory"] [(hContentType, "application/json")] body `shouldReturn` 403

        it "reads the body whatever the case of its content type" $ do
            let body = encode (object ["substitutions" .= [object ["from" .= ("p1" :: Text), "to" .= ("bafu::p2" :: Text)]]])
            fst <$> through asked methodPost ["api", "v1", "db", "mine", "activity", "p", "inventory"] [(hContentType, "Application/JSON")] body `shouldReturn` 403

    describe "the MCP tools" $ do
        let asked = termsOf "Terms of the dependency"
            callOn manager reader tool =
                callTool (Caller reader Nothing) manager [] Nothing Nothing noRequestId tool $
                    KM.fromList
                        [ ("database", String "root")
                        , ("process_id", String rootPid)
                        , ("method_id", String (UUID.toText (M.methodId climate)))
                        , ("collection", String collection)
                        ]
        it "refuse a database whose dependency asks terms, with them" $ do
            manager <- managerOn root dependency asked
            reply <- callOn manager (Just "ana") "get_impacts"
            fmap ("Terms of the dependency" `T.isInfixOf`) (errorText reply) `shouldBe` Just True

        it "say what the publisher sees of the computations the terms ask counted" $ do
            manager <- managerOn root dependency (LicenceOwn OwnLicence{ownText = "Counted", ownRefused = S.empty, ownAttribution = AttributionRequired, ownAcceptance = AcceptanceAsked (Counted ReadersNamed)})
            reply <- callOn manager (Just "ana") "get_impacts"
            fmap (disclosureSentence ReadersNamed `T.isInfixOf`) (errorText reply) `shouldBe` Just True

        it "ask for them before saying a tool name is unknown" $ do
            manager <- managerOn root dependency asked
            reply <- callOn manager (Just "ana") "no_such_tool"
            fmap ("Terms of the dependency" `T.isInfixOf`) (errorText reply) `shouldBe` Just True

        it "answer once the reader accepted them, and only that reader" $ do
            manager <- managerOn root dependency asked
            accepted <- callOn manager (Just "ana") "accept_terms"
            errorText accepted `shouldBe` Nothing
            errorText <$> callOn manager (Just "ana") "get_impacts" `shouldReturn` Nothing
            isJust . errorText <$> callOn manager (Just "bob") "get_impacts" `shouldReturn` True

        it "refuse to accept the terms of a database the engine does not know" $ do
            manager <- managerOn root dependency asked
            reply <- callTool (Caller (Just "ana") Nothing) manager [] Nothing Nothing noRequestId "accept_terms" (KM.fromList [("database", String "roto")])
            errorText reply `shouldBe` Just "Database not found: roto"

        it "answer as before when nothing asks terms" $ do
            manager <- managerOn root dependency LicenceUnstated
            errorText <$> callOn manager Nothing "get_impacts" `shouldReturn` Nothing
  where
    own (Right (LicenceOwn o)) = Right o
    own other = Left other

    isLeft' :: Either a b -> Bool
    isLeft' = either (const True) (const False)

field :: Text -> Value -> Maybe Value
field name (Object o) = KM.lookup (Key.fromText name) o
field _ _ = Nothing

-- | The text of a tool error, or nothing when the tool answered.
errorText :: Value -> Maybe Text
errorText reply = do
    Object o <- Just reply
    Object r <- KM.lookup "result" o
    Bool True <- KM.lookup "isError" r
    Array content <- KM.lookup "content" r
    Object c <- listToMaybe (toList content)
    String text <- KM.lookup "text" c
    pure text

{- | Send a request through the middleware to a route that echoes its body,
and read the status and the body answered.
-}
through :: ([Text] -> IO [Terms]) -> Method -> [Text] -> [(HeaderName, ByteString)] -> BL.ByteString -> IO (Int, BL.ByteString)
through asked method path headers body = do
    chunks <- newIORef (BL.toChunks body)
    let next = atomicModifyIORef' chunks (\case [] -> ([], mempty); c : cs -> (cs, c))
        req = setRequestBodyChunks next defaultRequest{requestMethod = method, pathInfo = path, requestHeaders = headers}
        echo r respond = strictRequestBody r >>= respond . responseLBS status200 []
    answered <- newIORef Nothing
    _ <- termsMiddleware asked echo req $ \response -> writeIORef answered (Just response) >> pure ResponseReceived
    readIORef answered >>= \case
        Just (ResponseBuilder status _ builder) -> pure (statusCode status, toLazyByteString builder)
        _ -> fail "no answer"
