{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

{- | The @\/mcp@ endpoint serves skills, as the skills extension of the
2026-07-28 revision describes them: a list, one skill by its address, and the
file it is read from. A client compares the header announced with the file it
reads, and its digest with the bytes, and drops the skill on any difference.
-}
module MCPSkillsSpec (spec) where

import Crypto.Hash (Digest, SHA256, hash)
import Data.Aeson (Value (..), decode, encode, object, toJSON, (.=))
import Data.Aeson.Key (Key)
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as BB
import qualified Data.ByteString.Lazy as BL
import Data.Either (isLeft)
import Data.IORef
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Vector as V
import Network.HTTP.Types (RequestHeaders)
import Network.Wai (defaultRequest, requestHeaders, requestMethod, setRequestBodyChunks)
import Network.Wai.Internal (Response (..), ResponseReceived (..))
import qualified TOML
import Test.Hspec

import API.MCP (mcpApp, modernVersion)
import Config (Config (..), defaultConfig, loadConfigOrDefault, validateConfig)
import Database.Manager (CachePolicy (..), initDatabaseManager)
import Skill (Skill, skillFile, skillFrom, skillName)
import Skill.Builtin (builtinSkills, servedSkills)

-- | The skills an engine with no configuration serves.
builtins :: IO [Skill]
builtins = either (\err -> fail ("a built-in skill is refused: " <> T.unpack err)) pure builtinSkills

-- | Post one body to an endpoint serving the built-in skills, and decode its answer.
post :: RequestHeaders -> Value -> IO (Maybe Value)
post hdrs payload = do
    manager <- initDatabaseManager defaultConfig NoCache
    skills <- builtins
    app <- mcpApp manager [] skills False Nothing Nothing id Nothing
    chunks <- newIORef (BL.toChunks (encode payload))
    let next = atomicModifyIORef' chunks $ \case
            [] -> ([], mempty)
            (c : rest) -> (rest, c)
    ref <- newIORef Nothing
    _ <- app (setRequestBodyChunks next defaultRequest{requestMethod = "POST", requestHeaders = hdrs}) $ \resp -> do
        writeIORef ref (Just resp)
        pure ResponseReceived
    readIORef ref >>= \case
        Just (ResponseBuilder _ _ b) -> pure (decode (BB.toLazyByteString b))
        _ -> fail "no buffered response"

-- | Ask a method of the 2026-07-28 era, with its headers.
ask :: Text -> [(Key, Value)] -> IO (Maybe Value)
ask method params = post headers (request method (("_meta", meta) : params))
  where
    meta = object ["io.modelcontextprotocol/protocolVersion" .= modernVersion, "io.modelcontextprotocol/clientCapabilities" .= object []]
    headers = [("MCP-Protocol-Version", TE.encodeUtf8 modernVersion), ("Mcp-Method", TE.encodeUtf8 method)]

request :: Text -> [(Key, Value)] -> Value
request method params = object ["jsonrpc" .= ("2.0" :: Text), "id" .= (1 :: Int), "method" .= method, "params" .= object params]

at :: [Key] -> Value -> Maybe Value
at [] v = Just v
at (k : ks) (Object o) = KM.lookup k o >>= at ks
at (k : ks) (Array a) | k == "0", Just v <- a V.!? 0 = at ks v
at _ _ = Nothing

uri :: Text
uri = "skill://volca/carbon-footprint/SKILL.md"

sha256 :: BS.ByteString -> Text
sha256 bytes = T.pack (show (hash bytes :: Digest SHA256))

spec :: Spec
spec = do
    describe "the skills served over MCP" $ do
        it "are announced by server/discover, with the resources they are read from" $ do
            a <- ask "server/discover" []
            (a >>= at ["result", "capabilities", "resources"]) `shouldBe` Just (object [])
            (a >>= at ["result", "capabilities", "extensions", "io.modelcontextprotocol/skills"]) `shouldBe` Just (object [])

        it "lists each built-in skill with its header, its file and caching hints" $ do
            a <- ask "skills/list" []
            let names = [n | Just (Array skills) <- [a >>= at ["result", "skills"]], s <- V.toList skills, Just (String n) <- [at ["frontmatter", "name"] s]]
            names `shouldBe` ["carbon-footprint", "pef-hotspots"]
            (a >>= at ["result", "skills", "0", "uri"]) `shouldBe` Just (String uri)
            (a >>= at ["result", "skills", "0", "resources", "0", "uri"]) `shouldBe` Just (String uri)
            (a >>= at ["result", "ttlMs"]) `shouldBe` Just (Number 3600000)
            (a >>= at ["result", "cacheScope"]) `shouldBe` Just (String "public")
            (a >>= at ["result", "resultType"]) `shouldBe` Just (String "complete")

        it "serves a file whose digest and size are those skills/get announces" $ do
            entry <- ask "skills/get" ["uri" .= uri]
            file <- ask "resources/read" ["uri" .= uri]
            let text = [t | Just (String t) <- [file >>= at ["result", "contents", "0", "text"]]]
                bytes = TE.encodeUtf8 (T.concat text)
            length text `shouldBe` 1
            (file >>= at ["result", "contents", "0", "mimeType"]) `shouldBe` Just (String "text/markdown")
            (entry >>= at ["result", "skill", "resources", "0", "digest"]) `shouldBe` Just (String ("sha256:" <> sha256 bytes))
            (entry >>= at ["result", "skill", "resources", "0", "size"]) `shouldBe` Just (toJSON (BS.length bytes))
            (entry >>= at ["result", "ttlMs"]) `shouldBe` Just (Number 3600000)

        it "refuses an address that names no skill, or no address, with -32602" $ do
            unknown <- ask "skills/get" ["uri" .= ("skill://volca/nothing/SKILL.md" :: Text)]
            missing <- ask "skills/get" []
            notText <- ask "resources/read" ["uri" .= (3 :: Int)]
            map (>>= at ["error", "code"]) [unknown, missing, notText] `shouldBe` replicate 3 (Just (Number (-32602)))

        it "lists the skill files as resources, and no resource template" $ do
            resources <- ask "resources/list" []
            templates <- ask "resources/templates/list" []
            (resources >>= at ["result", "resources", "0", "uri"]) `shouldBe` Just (String uri)
            (templates >>= at ["result", "resourceTemplates"]) `shouldBe` Just (Array mempty)
            (templates >>= at ["result", "cacheScope"]) `shouldBe` Just (String "public")

        it "are not served to a client of the old era" $ do
            a <- post [] (request "skills/list" [])
            (a >>= at ["error", "code"]) `shouldBe` Just (Number (-32601))

    describe "a skill's file" $ do
        it "writes its header as quoted strings, whatever the description holds" $
            fmap skillFile (skillFrom "a-b" "Says: \"#1\" \\ done" "Body.\n")
                `shouldBe` Right "---\nname: \"a-b\"\ndescription: \"Says: \\\"#1\\\" \\\\ done\"\n---\n\nBody.\n"

        it "is refused when a client would read its header differently, or drop it" $ do
            let refused name description = isLeft (skillFrom name description "Body.\n")
            map (`refused` "ok") ["-a", "a-", "a--b", "A", "", T.replicate 65 "a"] `shouldBe` replicate 6 True
            map (refused "a") ["", " ", T.replicate 1025 "a", "a\x85\&b", "a\x7f", "a\x2028\&b", "a\tb", "a --- b"] `shouldBe` replicate 8 True
            refused "a" "Fine: any \"quote\" & #sign." `shouldBe` False
            isLeft (skillFrom "a" "ok" "  \n") `shouldBe` True

    describe "the skills of a configuration" $ do
        it "replace a built-in skill of the same name, and keep the others" $ do
            own <- either (fail . T.unpack) pure (skillFrom "carbon-footprint" "Our own footprint." "Ours.\n")
            fmap (map skillName) (servedSkills [own]) `shouldBe` Right ["carbon-footprint", "pef-hotspots"]
            fmap (map skillFile . take 1) (servedSkills [own]) `shouldBe` Right [skillFile own]

        it "are read from [[skills]], their text kept as written" $
            case TOML.decode "[[skills]]\nname = \"local\"\ndescription = \"Local.\"\ntext = '''\n# Title\n1. A \\\"step\\\".\n'''\n" :: Either TOML.TOMLError Config of
                Left err -> expectationFailure (show err)
                Right cfg -> map skillFile (cfgSkills cfg) `shouldBe` ["---\nname: \"local\"\ndescription: \"Local.\"\n---\n\n# Title\n1. A \\\"step\\\".\n"]

        it "refuse a [[skills]] entry that could not be served" $
            (TOML.decode "[[skills]]\nname = \"Bad Name\"\ndescription = \"x\"\ntext = \"y\"\n" :: Either TOML.TOMLError Config) `shouldSatisfy` isLeft

        it "refuse two skills of the same name" $ do
            own <- either (fail . T.unpack) pure (skillFrom "local" "Local." "Ours.\n")
            validateConfig defaultConfig{cfgSkills = [own, own]} `shouldSatisfy` isLeft

        it "are served with the built-in ones when no configuration is given" $ do
            loaded <- loadConfigOrDefault Nothing
            fmap (map skillName . cfgSkills) loaded `shouldBe` Right ["carbon-footprint", "pef-hotspots"]
