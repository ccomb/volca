{-# LANGUAGE OverloadedStrings #-}

module ConfigSpec (spec) where

import Builtin (BuiltinMethod (..), BuiltinTable (..), DataVersion (..), builtinDataVersion, builtinMethods, builtinName, builtinTables)
import Config (
    CFPatchOp (..),
    ClassificationEntry (..),
    ClassificationPreset (..),
    Config (..),
    DatabaseConfig (..),
    HostingConfig (..),
    Listen (..),
    MethodConfig (..),
    MethodOrigin (..),
    MethodPatch (..),
    MethodPatchMatch (..),
    RefDataConfig (..),
    RefDataSource (..),
    ScoringSetConfig (..),
    ServerConfig (..),
    applyDataDir,
    builtinEntry,
    clientHost,
    configKeys,
    dataBundleDir,
    defaultConfig,
    documentKeyPaths,
    expandClassificationPreset,
    keyPaths,
    listenOn,
    loadConfigOrDefault,
    readDataVersion,
    redirectIntoDataDir,
    refDataDecoder,
    resolveConfigPaths,
    unknownKeys,
    validateConfig,
    withBuiltins,
 )
import Data.Either (isRight)
import Data.List (sort)
import qualified Data.Map.Strict as M
import Data.Maybe (listToMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.FilePath (normalise)
import TOML (getArrayOf, getFieldWith)
import qualified TOML
import Test.Hspec
import Types (ClassificationFilter (..), ClassificationMatch (..))

serverOn :: Text -> ServerConfig
serverOn host =
    ServerConfig
        { scPort = 8080
        , scHost = host
        , scPassword = Nothing
        , scName = Nothing
        }

mkRef :: FilePath -> RefDataConfig
mkRef p =
    RefDataConfig
        { rdName = "test"
        , rdSource = FromFile p
        , rdActive = True
        , rdIsUploaded = False
        , rdIsAuto = False
        , rdDescription = Nothing
        }

{- | A decoded database and method entry carrying the given paths. Read through
the real decoders so a required key added to either one fails here rather than
in whichever test happened to hand-build the record.
-}
withParsedPaths :: FilePath -> FilePath -> (DatabaseConfig -> MethodConfig -> Expectation) -> Expectation
withParsedPaths dbPath methodPath body =
    case ( TOML.decode (entry "agb" dbPath) :: Either TOML.TOMLError DatabaseConfig
         , TOML.decode (entry "EF" methodPath) :: Either TOML.TOMLError MethodConfig
         ) of
        (Right db, Right method) -> body db method
        (Left e, _) -> expectationFailure (show e)
        (_, Left e) -> expectationFailure (show e)
  where
    entry name path = "name = \"" <> name <> "\"\npath = \"" <> T.pack path <> "\"\n"

spec :: Spec
spec = do
    describe "listenOn" $ do
        it "listens on the interface the configuration names" $
            listenOn Nothing (serverOn "0.0.0.0") `shouldBe` ListenOn "0.0.0.0" 8080

        -- Read through the real decoder rather than a hand-built record: what
        -- has to hold is the decoder's own fallback, since it is what a file
        -- with no host at all gets, and what a password left unset assumes.
        it "keeps a configuration that names no host on loopback" $
            case TOML.decode "port = 8080\n" :: Either TOML.TOMLError ServerConfig of
                Left err -> expectationFailure (show err)
                Right sc -> listenOn Nothing sc `shouldBe` ListenOn "127.0.0.1" 8080

        it "lets --port override the configured port without moving the interface" $
            listenOn (Just 9000) (serverOn "0.0.0.0") `shouldBe` ListenOn "0.0.0.0" 9000

        -- --port 0 goes through the free-port path, which binds loopback and
        -- takes no host, so the configured one cannot be honoured there.
        it "asks for a free loopback port whatever host the configuration names" $
            listenOn (Just 0) (serverOn "0.0.0.0") `shouldBe` ListenOnFreeLoopbackPort

    describe "clientHost" $ do
        -- A listening address names interfaces to accept on. Handing one to a
        -- client as a destination gives http://0.0.0.0:8080, which fails
        -- outright on Windows, or http://*:8080, which is not a URL at all.
        it "sends a client to this machine when the server accepts on every interface" $
            map clientHost ["0.0.0.0", "::", "*", "*4", "!4", "*6", "!6"]
                `shouldBe` replicate 7 "localhost"

        it "leaves an address that names one interface alone" $
            map clientHost ["127.0.0.1", "::1", "192.168.1.10", "engine.internal"]
                `shouldBe` ["127.0.0.1", "::1", "192.168.1.10", "engine.internal"]

    describe "unknownKeys" $ do
        let unread t = case TOML.decode t :: Either TOML.TOMLError TOML.Table of
                Left err -> ["did not parse: " <> T.pack (show err)]
                Right doc -> unknownKeys configKeys doc

        -- The one that matters: a key wrongly reported unread is a warning on
        -- a file that is perfectly good, which teaches the reader to ignore
        -- warnings. The shipped configuration exercises about a quarter of the
        -- schema, so the fixture below names the rest.
        it "reads every key of the configuration this repository ships" $ do
            shipped <- TIO.readFile "volca.toml"
            unread shipped `shouldBe` []

        it "reads every key a document can name" $ do
            everyKey <- TIO.readFile "test/data/every-config-key.toml"
            unread everyKey `shouldBe` []

        -- The other direction, so the fixture cannot quietly stop covering
        -- what it claims to: whatever the schema names, the fixture spells
        -- out. Without this, a key dropped from configKeys would go on being
        -- reported unread on every valid file and no test would notice.
        it "leaves no key of the schema unexercised" $ do
            everyKey <- TIO.readFile "test/data/every-config-key.toml"
            case TOML.decode everyKey :: Either TOML.TOMLError TOML.Table of
                Left err -> expectationFailure (show err)
                Right doc ->
                    -- Compared without the [] an array carries in a path: the
                    -- schema names keys, not how many of each a file holds.
                    let named = map (T.replace "[]" "") (documentKeyPaths doc)
                     in filter (`notElem` named) (keyPaths configKeys) `shouldBe` []

        -- How geographies went missing from the Docker image's own config: a
        -- top-level key written below a header belongs to that header.
        it "names a top-level key written under a section" $
            unread "[server]\nport = 8080\ngeographies = \"data/geographies.csv\"\n"
                `shouldBe` ["server.geographies"]

        it "names a key an array of tables does not carry" $
            unread "[[databases]]\nname = \"a\"\npath = \"a.zip\"\nactive = true\n"
                `shouldBe` ["databases[].active"]

        it "says once what twenty entries get wrong" $
            unread "[[databases]]\nname=\"a\"\npath=\"a\"\nactive=true\n[[databases]]\nname=\"b\"\npath=\"b\"\nactive=true\n"
                `shouldBe` ["databases[].active"]

        -- Scoring variables, computed formulas and location aliases are named
        -- by whoever writes the file, so no name there can be unknown.
        it "leaves the keys the file's author invents alone" $
            unread
                "[[methods]]\nname=\"EF\"\npath=\"x.zip\"\n\
                \[[methods.scoring]]\nname=\"ECS\"\n\
                \[methods.scoring.variables]\ncch = \"Climate change\"\n\
                \[methods.scoring.weighting]\ncch = 0.21\n"
                `shouldBe` []

        it "reaches a section nested two deep" $
            unread
                "[[methods]]\nname=\"EF\"\npath=\"x.zip\"\n\
                \[[methods.patches]]\nscale = 0.6\nmatch = { flow-name = \"Uranium\", flavour = \"x\" }\n"
                `shouldBe` ["methods[].patches[].match.flavour"]

    describe "keyPaths" $ do
        -- What `volca dump-config-schema` prints, which another build reads.
        it "names a key of every section, sorted" $ do
            let paths = keyPaths configKeys
            paths `shouldSatisfy` elem "hosting.read_only"
            paths `shouldSatisfy` elem "methods.scoring.weighting"
            paths `shouldSatisfy` elem "geographies"
            paths `shouldBe` sort paths

        -- A section whose keys the file's author invents is named, but its
        -- contents cannot be enumerated, so the list stops there.
        it "stops at a section the file's author fills" $
            filter (T.isPrefixOf "methods.scoring.variables") (keyPaths configKeys)
                `shouldBe` ["methods.scoring.variables"]

    describe "expandClassificationPreset" $ do
        let raw =
                ClassificationPreset
                    { cpName = "raw"
                    , cpLabel = "Raw"
                    , cpDescription = Nothing
                    , cpFilters =
                        [ ClassificationEntry{ceSystem = "AGB", ceValue = "Agriculture", ceMode = "exact"}
                        , ClassificationEntry{ceSystem = "AGB", ceValue = "Food", ceMode = "contains"}
                        ]
                    }
        it "expands a configured preset into its filters" $
            expandClassificationPreset [raw] (Just "raw")
                `shouldBe` Right
                    [ ClassificationFilter{clfSystem = "AGB", clfValue = "Agriculture", clfMatch = MatchExact}
                    , ClassificationFilter{clfSystem = "AGB", clfValue = "Food", clfMatch = MatchContains}
                    ]

        it "filters nothing when no preset was asked for" $
            expandClassificationPreset [raw] Nothing `shouldBe` Right []

        -- An unknown name used to expand to no filters at all, which turned a
        -- request for one slice of the database into a request for all of it.
        it "refuses an unknown name instead of widening the query" $
            case expandClassificationPreset [raw] (Just "transformed") of
                Right filters -> expectationFailure ("expected a refusal, got " <> show filters)
                Left err -> do
                    err `shouldSatisfy` T.isInfixOf "transformed"
                    err `shouldSatisfy` T.isInfixOf "raw"

        it "says so when the instance carries no preset at all" $
            case expandClassificationPreset [] (Just "raw") of
                Right filters -> expectationFailure ("expected a refusal, got " <> show filters)
                Left err -> err `shouldSatisfy` T.isInfixOf "no classification presets"

    describe "HostingConfig" $ do
        -- The [hosting] fragment is the one interface the operator actually
        -- touches; a typo in a key name here would silently drop their words.
        let decodeHosting t = TOML.decode t :: Either TOML.TOMLError HostingConfig
        it "parses read_only_message" $
            case decodeHosting "read_only = true\nread_only_message = \"Ask the operator.\"\n" of
                Right hc -> hcReadOnlyMessage hc `shouldBe` "Ask the operator."
                Left e -> expectationFailure (show e)
        it "defaults read_only_message to unset when the key is absent" $
            case decodeHosting "read_only = true\n" of
                Right hc -> hcReadOnlyMessage hc `shouldBe` ""
                Left e -> expectationFailure (show e)

    describe "validateConfig" $ do
        let preset name =
                ClassificationPreset
                    { cpName = name
                    , cpLabel = name
                    , cpDescription = Nothing
                    , cpFilters = []
                    }
            decodeMethod t = TOML.decode t :: Either TOML.TOMLError MethodConfig

        -- Presets and methods are looked up by name, so a duplicate would
        -- silently shadow one of its bearers; startup refuses it instead.
        it "refuses two classification presets sharing a name" $
            case validateConfig defaultConfig{cfgClassificationPresets = [preset "raw", preset "raw"]} of
                Right _ -> expectationFailure "expected a refusal"
                Left err -> do
                    err `shouldSatisfy` T.isInfixOf "Duplicate classification preset"
                    err `shouldSatisfy` T.isInfixOf "raw"

        it "refuses two method collections sharing a name" $
            case decodeMethod "name = \"EF\"\npath = \"x.zip\"\n" of
                Left e -> expectationFailure (show e)
                Right mc -> case validateConfig defaultConfig{cfgMethods = [mc, mc]} of
                    Right _ -> expectationFailure "expected a refusal"
                    Left err -> do
                        err `shouldSatisfy` T.isInfixOf "Duplicate method collection"
                        err `shouldSatisfy` T.isInfixOf "EF"

        it "accepts distinct names" $
            case decodeMethod "name = \"EF\"\npath = \"x.zip\"\n" of
                Left e -> expectationFailure (show e)
                Right mc ->
                    validateConfig
                        defaultConfig
                            { cfgClassificationPresets = [preset "raw", preset "transformed"]
                            , cfgMethods = [mc]
                            }
                        `shouldSatisfy` isRight

    describe "MethodConfig global-methods" $ do
        let decodeMethod t = TOML.decode t :: Either TOML.TOMLError MethodConfig
        it "parses the global-methods list" $
            case decodeMethod "name = \"EF\"\npath = \"x.zip\"\nglobal-methods = [\"Land use\"]\n" of
                Right mc -> mcGlobalMethods mc `shouldBe` ["Land use"]
                Left e -> expectationFailure (show e)
        it "defaults global-methods to empty when the key is absent" $
            case decodeMethod "name = \"EF\"\npath = \"x.zip\"\n" of
                Right mc -> mcGlobalMethods mc `shouldBe` []
                Left e -> expectationFailure (show e)

    describe "MethodConfig patches" $ do
        let decodeMethod t = TOML.decode t :: Either TOML.TOMLError MethodConfig

        it "defaults patches to empty when the key is absent" $
            case decodeMethod "name = \"EF\"\npath = \"x.zip\"\n" of
                Right mc -> mcPatches mc `shouldBe` []
                Left e -> expectationFailure (show e)

        it "parses a scale patch with a category + flow-name-prefix selector" $
            case decodeMethod
                "name = \"EF\"\npath = \"x.zip\"\n\n\
                \[[patches]]\n\
                \description = \"uraniumFRU\"\n\
                \match = { category = \"Resource use, fossils\", flow-name-prefix = \"Uranium\" }\n\
                \scale = 0.6\n" of
                Right mc -> case mcPatches mc of
                    [patch] -> do
                        mpDescription patch `shouldBe` Just "uraniumFRU"
                        mpmCategory (mpMatch patch) `shouldBe` Just "Resource use, fossils"
                        mpmFlowNamePrefix (mpMatch patch) `shouldBe` Just "Uranium"
                        mpOp patch `shouldBe` ScaleBy 0.6
                    ps -> expectationFailure ("expected exactly one patch, got " <> show (length ps))
                Left e -> expectationFailure (show e)

        it "parses a set-value patch with a subcompartment-contains selector" $
            case decodeMethod
                "name = \"EF\"\npath = \"x.zip\"\n\n\
                \[[patches]]\n\
                \match = { subcompartment-contains = \"long-term\" }\n\
                \set-value = 0.0\n" of
                Right mc -> case mcPatches mc of
                    [patch] -> do
                        mpmSubcompartmentContains (mpMatch patch) `shouldBe` Just "long-term"
                        mpOp patch `shouldBe` SetValueTo 0.0
                    ps -> expectationFailure ("expected exactly one patch, got " <> show (length ps))
                Left e -> expectationFailure (show e)

        it "rejects a patch with both scale and set-value" $
            case decodeMethod
                "name = \"EF\"\npath = \"x.zip\"\n\n\
                \[[patches]]\n\
                \match = { flow-name = \"Uranium\" }\n\
                \scale = 0.6\n\
                \set-value = 0.0\n" of
                Left _ -> pure ()
                Right _ -> expectationFailure "expected a decode error for scale + set-value together"

        it "rejects a patch with neither scale nor set-value" $
            case decodeMethod
                "name = \"EF\"\npath = \"x.zip\"\n\n\
                \[[patches]]\n\
                \match = { flow-name = \"Uranium\" }\n" of
                Left _ -> pure ()
                Right _ -> expectationFailure "expected a decode error when neither scale nor set-value is set"

        it "rejects a patch whose selector matches every CF" $
            case decodeMethod
                "name = \"EF\"\npath = \"x.zip\"\n\n\
                \[[patches]]\n\
                \match = {}\n\
                \scale = 0.6\n" of
                Left _ -> pure ()
                Right _ -> expectationFailure "expected a decode error for an empty selector"

    let registry path uploaded = RefDataConfig{rdName = "flows", rdSource = FromFile path, rdActive = True, rdIsUploaded = uploaded, rdIsAuto = False, rdDescription = Nothing}
        reading path = defaultConfig{cfgFlowSynonyms = [registry path False]}

    describe "dataBundleDir" $ do
        it "is the directory of the flow registry the engine reads" $
            dataBundleDir (reading "/opt/volca-data/3/flows.csv") `shouldBe` Just "/opt/volca-data/3"

        it "skips an uploaded registry, which lives with the uploads, not the bundle" $
            dataBundleDir defaultConfig{cfgFlowSynonyms = [registry "uploads/mine.csv" True, registry "data/flows.csv" False]}
                `shouldBe` Just "data"

        it "is Nothing when the registry is the built-in one: no bundle on disk" $
            dataBundleDir defaultConfig `shouldBe` Nothing

    describe "readDataVersion" $ do
        it "reads the VERSION file beside the registry the engine reads" $ do
            expected <- T.strip <$> TIO.readFile "data/VERSION"
            v <- readDataVersion (reading "data/flows.csv")
            v `shouldBe` Just (DataVersion expected)

        it "is the built-in version when the registry is built in" $
            readDataVersion defaultConfig `shouldReturn` Just builtinDataVersion

        it "is Nothing when the registry's directory carries no VERSION" $
            readDataVersion (reading "test/flows.csv") `shouldReturn` Nothing

    describe "refDataDecoder" $ do
        let decodeUnits = TOML.decodeWith (getFieldWith (getArrayOf (refDataDecoder BuiltinUnits)) "units")

        it "reads a path as a file, named after it by default" $
            fmap (map (\r -> (rdName r, rdSource r))) (decodeUnits "[[units]]\npath = \"mine.csv\"\n")
                `shouldBe` Right [("mine.csv", FromFile "mine.csv")]

        it "reads a name without a path as the built-in table, which is how a file switches it off" $
            fmap (map (\r -> (rdSource r, rdActive r))) (decodeUnits "[[units]]\nname = \"Default units\"\nactive = false\n")
                `shouldBe` Right [(BuiltIn BuiltinUnits, False)]

        it "refuses a pathless entry that is not the built-in of its own table" $
            case decodeUnits "[[units]]\nname = \"Default flow synonyms\"\n" of
                Left err -> show err `shouldContain` "Default units"
                Right _ -> expectationFailure "expected a decode error naming the built-in table"

    describe "withBuiltins" $ do
        it "lists the built-in table of a kind the configuration says nothing about" $
            map rdSource (cfgUnits (withBuiltins defaultConfig{cfgUnits = []})) `shouldBe` [BuiltIn BuiltinUnits]

        it "lets a file of the same name stand in for the built-in" $ do
            let mine = (builtinEntry BuiltinUnits){rdSource = FromFile "mine.csv"}
            cfgUnits (withBuiltins defaultConfig{cfgUnits = [mine]}) `shouldBe` [mine]

        -- Tables of one kind are merged in name order, "Default ..." first,
        -- so adding the built-in beside an operator's own file would let it
        -- win on every key they share, without a word. Their list is theirs.
        it "leaves a kind the configuration lists alone, whatever the names" $ do
            let mine = mkRef "mine.csv"
            cfgUnits (withBuiltins defaultConfig{cfgUnits = [mine]}) `shouldBe` [mine]

        it "keeps the built-in beside the operator's own when named" $ do
            let mine = mkRef "mine.csv"
                both = [mine, builtinEntry BuiltinUnits]
            cfgUnits (withBuiltins defaultConfig{cfgUnits = both}) `shouldBe` both

        it "keeps a switched-off built-in listed, off" $ do
            let off = (builtinEntry BuiltinUnits){rdActive = False}
            map rdActive (cfgUnits (withBuiltins defaultConfig{cfgUnits = [off]})) `shouldBe` [False]

        it "is what the defaults are" $
            map (fmap rdName . listToMaybe) [cfgFlowSynonyms defaultConfig, cfgCompartmentMappings defaultConfig, cfgUnits defaultConfig, cfgEnergyDensities defaultConfig]
                `shouldBe` map (Just . builtinName) builtinTables

    describe "built-in methods" $ do
        let decodeMethods :: Text -> Either TOML.TOMLError [MethodConfig]
            decodeMethods = TOML.decodeWith (getFieldWith (getArrayOf TOML.tomlDecoder) "methods")
            originsAfterBuiltins :: Text -> Either TOML.TOMLError [(Text, MethodOrigin, Bool)]
            originsAfterBuiltins toml =
                map (\m -> (mcName m, mcOrigin m, mcActive m)) . cfgMethods . withBuiltins . (\ms -> defaultConfig{cfgMethods = ms})
                    <$> decodeMethods toml

        it "are listed, on, when the configuration names none" $
            map (\m -> (mcOrigin m, mcActive m)) (cfgMethods defaultConfig)
                `shouldBe` map (\b -> (MethodBuiltIn b, True)) builtinMethods

        -- Collections are never merged, so the built-in beside an operator's
        -- own changes none of their scores: unlike a table, it stays.
        it "stay beside a collection the configuration adds" $
            originsAfterBuiltins "[[methods]]\nname = \"EF\"\npath = \"ef.zip\"\n"
                `shouldBe` Right [("EF", MethodFromFile "ef.zip", True), ("plain-indicators", MethodBuiltIn BuiltinPlainIndicators, True)]

        it "are switched off by naming one without a path" $
            originsAfterBuiltins "[[methods]]\nname = \"plain-indicators\"\nactive = false\n"
                `shouldBe` Right [("plain-indicators", MethodBuiltIn BuiltinPlainIndicators, False)]

        it "are replaced by a file of the same name" $
            originsAfterBuiltins "[[methods]]\nname = \"plain-indicators\"\npath = \"mine.csv\"\n"
                `shouldBe` Right [("plain-indicators", MethodFromFile "mine.csv", True)]

        it "refuse a pathless entry that names none of them" $
            case decodeMethods "[[methods]]\nname = \"EF\"\n" of
                Left err -> show err `shouldContain` "plain-indicators"
                Right _ -> expectationFailure "expected a decode error naming the built-in collections"

        it "have no path to redirect or resolve" $
            map mcOrigin (cfgMethods (resolveConfigPaths (Just "/etc/volca/volca.toml") (applyDataDir (Just "/d") defaultConfig)))
                `shouldBe` map MethodBuiltIn builtinMethods

    describe "redirectIntoDataDir" $ do
        it "leaves paths unchanged when VOLCA_DATA_DIR is unset" $
            redirectIntoDataDir Nothing "data/flows.csv" `shouldBe` "data/flows.csv"

        it "redirects unix-style data/ prefix to the env-var dir" $
            redirectIntoDataDir (Just "/opt/volca-data/v1") "data/flows.csv"
                `shouldBe` "/opt/volca-data/v1/flows.csv"

        it "redirects windows-style data\\ prefix the same way" $
            redirectIntoDataDir (Just "/opt/volca-data/v1") "data\\flows.csv"
                `shouldBe` "/opt/volca-data/v1/flows.csv"

        it "leaves non-data paths alone (user databases must not be redirected)" $
            redirectIntoDataDir (Just "/opt/volca-data/v1") "DBs/agribalyse.7z"
                `shouldBe` "DBs/agribalyse.7z"

        it "leaves absolute paths alone even if they happen to start with 'data'" $
            redirectIntoDataDir (Just "/opt/volca-data/v1") "/etc/data/flows.csv"
                `shouldBe` "/etc/data/flows.csv"

    describe "applyDataDir" $ do
        let cfg =
                defaultConfig
                    { cfgGeographies = Just "data/geographies.csv"
                    , cfgChemSynonyms = Just "data/chem.csv"
                    , cfgSubstanceEdges = Just "data/edges.csv"
                    , cfgFlowSynonyms = [mkRef "data/flows.csv"]
                    , cfgCompartmentMappings = [mkRef "data/compartments.csv"]
                    , cfgUnits = [mkRef "data/units.csv"]
                    , cfgEnergyDensities = [mkRef "data/energy.csv"]
                    }

        it "rewrites every reference-data path when the env var is set" $ do
            let resolved = applyDataDir (Just "/d") cfg
            cfgGeographies resolved `shouldBe` Just "/d/geographies.csv"
            cfgChemSynonyms resolved `shouldBe` Just "/d/chem.csv"
            cfgSubstanceEdges resolved `shouldBe` Just "/d/edges.csv"
            map rdSource (cfgFlowSynonyms resolved) `shouldBe` [FromFile "/d/flows.csv"]
            map rdSource (cfgCompartmentMappings resolved) `shouldBe` [FromFile "/d/compartments.csv"]
            map rdSource (cfgUnits resolved) `shouldBe` [FromFile "/d/units.csv"]
            map rdSource (cfgEnergyDensities resolved) `shouldBe` [FromFile "/d/energy.csv"]

        -- Both rewrites walk one enumeration of the path-bearing fields, so
        -- what keeps them apart is the kind each field is tagged with. The
        -- fixture paths open with "data/" on purpose: that prefix is the only
        -- input redirectIntoDataDir acts on, so a database tagged as reference
        -- data would be visibly redirected here and nowhere else.
        it "leaves user content where the operator put it" $
            withParsedPaths "data/agb.CSV" "data/ef.zip" $ \db method -> do
                let userCfg = cfg{cfgDatabases = [db], cfgMethods = [method]}
                applyDataDir (Just "/d") userCfg
                    `shouldBe` (applyDataDir (Just "/d") cfg){cfgDatabases = [db], cfgMethods = [method]}

        it "is a no-op when the env var is unset" $
            applyDataDir Nothing cfg `shouldBe` cfg

        it "leaves a built-in table alone: it has no path to redirect" $
            map rdSource (cfgUnits (applyDataDir (Just "/d") defaultConfig)) `shouldBe` [BuiltIn BuiltinUnits]

    describe "resolveConfigPaths" $ do
        let withParsed = withParsedPaths "agb.CSV" "ef.zip"

        it "prefixes every relative path with the config file's directory" $
            withParsed $ \db method -> do
                let cfg =
                        defaultConfig
                            { cfgDatabases = [db]
                            , cfgMethods = [method]
                            , cfgFlowSynonyms = [mkRef "flows.csv"]
                            , cfgCompartmentMappings = [mkRef "compartments.csv"]
                            , cfgUnits = [mkRef "units.csv"]
                            , cfgEnergyDensities = [mkRef "energy.csv"]
                            , cfgGeographies = Just "geographies.csv"
                            , cfgChemSynonyms = Just "chem.csv"
                            , cfgSubstanceEdges = Just "edges.csv"
                            }
                    resolved = resolveConfigPaths (Just "/etc/volca/volca.toml") cfg
                -- A method path used to follow the process while the database
                -- path beside it followed the file. They move together now.
                -- Expected values go through 'normalise' too: on Windows the
                -- resolver emits backslashes.
                map dcPath (cfgDatabases resolved) `shouldBe` [normalise "/etc/volca/agb.CSV"]
                map mcOrigin (cfgMethods resolved) `shouldBe` [MethodFromFile (normalise "/etc/volca/ef.zip")]
                map rdSource (cfgFlowSynonyms resolved) `shouldBe` [FromFile (normalise "/etc/volca/flows.csv")]
                map rdSource (cfgCompartmentMappings resolved) `shouldBe` [FromFile (normalise "/etc/volca/compartments.csv")]
                map rdSource (cfgUnits resolved) `shouldBe` [FromFile (normalise "/etc/volca/units.csv")]
                map rdSource (cfgEnergyDensities resolved) `shouldBe` [FromFile (normalise "/etc/volca/energy.csv")]
                cfgGeographies resolved `shouldBe` Just (normalise "/etc/volca/geographies.csv")
                cfgChemSynonyms resolved `shouldBe` Just (normalise "/etc/volca/chem.csv")
                cfgSubstanceEdges resolved `shouldBe` Just (normalise "/etc/volca/edges.csv")

        it "leaves an absolute path alone" $
            withParsed $ \_ method -> do
                let cfg = defaultConfig{cfgMethods = [method{mcOrigin = MethodFromFile "/srv/methods/ef.zip"}]}
                map mcOrigin (cfgMethods (resolveConfigPaths (Just "/etc/volca/volca.toml") cfg))
                    `shouldBe` [MethodFromFile (normalise "/srv/methods/ef.zip")]

        it "falls back to the process directory when there is no config file" $
            withParsed $ \_ method -> do
                let cfg = defaultConfig{cfgMethods = [method]}
                map mcOrigin (cfgMethods (resolveConfigPaths Nothing cfg)) `shouldBe` [MethodFromFile "ef.zip"]

        it "keeps the shipped data bundle applyDataDir already pointed at" $ do
            -- applyDataDir runs first and turns "data/x" into an absolute path;
            -- resolving after it must not prefix that a second time.
            let cfg = defaultConfig{cfgFlowSynonyms = [mkRef "data/flows.csv"]}
                bundled = applyDataDir (Just "/opt/volca/data") cfg
            map rdSource (cfgFlowSynonyms (resolveConfigPaths (Just "/etc/volca/volca.toml") bundled))
                `shouldBe` [FromFile (normalise "/opt/volca/data/flows.csv")]

    describe "loadConfigOrDefault" $ do
        it "yields the validated defaults when no path is given" $ do
            result <- loadConfigOrDefault Nothing
            case result of
                Right cfg -> cfgDatabases cfg `shouldBe` []
                Left err -> expectationFailure (show err)

        it "still fails loudly on an explicit path that does not exist" $ do
            result <- loadConfigOrDefault (Just "/nonexistent/volca.toml")
            case result of
                Left err -> err `shouldSatisfy` ("Config file not found" `T.isPrefixOf`)
                Right _ -> expectationFailure "expected a missing explicit config to fail"

    describe "ScoringSetConfig labels" $ do
        let decodeSet :: Text -> Either TOML.TOMLError ScoringSetConfig
            decodeSet = TOML.decode

        it "accepts a label on a computed variable" $ do
            let toml =
                    "name = \"ECS\"\n\
                    \[computed]\n\
                    \etf = \"2 * etfo + etfi\"\n\
                    \[labels]\n\
                    \etf = \"Ecotoxicity, freshwater\"\n"
            fmap sscLabels (decodeSet toml)
                `shouldBe` Right (M.singleton "etf" "Ecotoxicity, freshwater")

        it "accepts a label on a primitive variable" $ do
            let toml =
                    "name = \"ECS\"\n\
                    \[variables]\n\
                    \cch = \"Climate change\"\n\
                    \[labels]\n\
                    \cch = \"Changement climatique\"\n"
            fmap sscLabels (decodeSet toml)
                `shouldBe` Right (M.singleton "cch" "Changement climatique")

        it "rejects a label whose key matches no scoring variable" $ do
            let toml =
                    "name = \"ECS\"\n\
                    \[computed]\n\
                    \etf = \"2 * etfo + etfi\"\n\
                    \[labels]\n\
                    \eft = \"Ecotoxicity, freshwater\"\n"
            case decodeSet toml of
                Right _ -> expectationFailure "orphan label key must be rejected"
                Left err -> show err `shouldContain` "eft"
