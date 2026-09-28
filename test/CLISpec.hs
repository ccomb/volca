{-# LANGUAGE OverloadedStrings #-}

module CLISpec (spec) where

import Options.Applicative (
    ParserResult (..),
    defaultPrefs,
    execParserPure,
    renderFailure,
 )
import System.Exit (ExitCode (..))
import Test.Hspec

import CLI.Client (ImpactTarget (..), MethodRow (..), contributionPath, resolveImpactTarget)
import CLI.Parser (cliParserInfo)
import CLI.Types
import Data.Either (isLeft)
import qualified Data.Text as T

-- Parse argv and return either the parsed config (Right) or the failure summary (Left).
runParse :: [String] -> Either String CLIConfig
runParse argv =
    case execParserPure defaultPrefs cliParserInfo argv of
        Success cfg -> Right cfg
        Failure f -> Left (fst (renderFailure f "volca"))
        CompletionInvoked _ -> Left "completion-invoked"

-- Convenience: extract the Command from a successful parse (or fail loudly).
parseCmd :: [String] -> IO Command
parseCmd argv = case runParse argv of
    Right cfg -> case command cfg of
        Just c -> pure c
        Nothing -> expectationFailure "Expected a command, got Nothing" >> error "unreachable"
    Left err -> expectationFailure ("Parse failed: " <> err) >> error "unreachable"

spec :: Spec
spec = do
    describe "--help on every subcommand" $ do
        -- A parser built without `helper` does not know --help, so it rejects
        -- it: usage on the error stream, non-zero exit, nothing for whoever
        -- was capturing the help. That is invisible from the terminal, where
        -- the usage still appears, which is how four documentation files came
        -- to be committed empty. So it is asked of every subcommand here.
        let answersHelp argv = case execParserPure defaultPrefs cliParserInfo (argv <> ["--help"]) of
                Failure f -> snd (renderFailure f "volca") == ExitSuccess
                Success _ -> False
                CompletionInvoked _ -> False
            subcommands =
                [ ["server"]
                , ["activity", "x"]
                , ["inventory", "x"]
                , ["tree", "x"]
                , ["supply-chain", "x"]
                , ["consumers", "x"]
                , ["path-to", "x", "y"]
                , ["contributing-flows", "x"]
                , ["contributing-activities", "x"]
                , ["explain-cf", "x"]
                , ["flow", "x"]
                , ["flow", "x", "activities"]
                , ["activities"]
                , ["flows"]
                , ["impacts", "x"]
                , ["debug-matrices"]
                , ["export-matrices"]
                , ["database"]
                , ["database", "list"]
                , ["database", "load", "x"]
                , ["database", "unload", "x"]
                , ["database", "upload", "f"]
                , ["database", "delete", "x"]
                , ["database", "delete-activities"]
                , ["database", "copy", "x"]
                , ["database", "relink", "x"]
                , ["database", "export", "x"]
                , ["database", "create-activities", "x"]
                , ["database", "replace-activity", "x"]
                , ["database", "edit-exchanges", "x"]
                , ["method"]
                , ["method", "list"]
                , ["method", "upload", "f"]
                , ["method", "delete", "x"]
                , ["method", "export", "x"]
                , ["methods"]
                , ["synonyms"]
                , ["compartment-mappings"]
                , ["units"]
                , ["flow-mapping"]
                , ["stop"]
                , ["repl"]
                , ["dump-openapi"]
                , ["dump-mcp-tools"]
                , ["dump-config-schema"]
                ]
        mapM_
            (\argv -> it (unwords argv <> " --help describes itself") $ answersHelp argv `shouldBe` True)
            subcommands

    describe "CLI.Types.parseOutputFormat" $ do
        let cases =
                [ ("json", Just JSON)
                , ("csv", Just CSV)
                , ("table", Just Table)
                , ("pretty", Just Pretty)
                , ("JSON", Just JSON) -- case-insensitive
                , ("Csv", Just CSV)
                , ("PRETTY", Just Pretty)
                , ("xml", Nothing) -- unknown format
                , ("", Nothing) -- empty string
                ]
        mapM_
            ( \(input, expected) ->
                it ("parses " <> show input <> " → " <> show expected) $
                    parseOutputFormat input `shouldBe` expected
            )
            cases

    describe "global options" $ do
        it "parses --config FILE" $ do
            case runParse ["--config", "volca.toml", "methods"] of
                Right cfg -> configFile (globalOptions cfg) `shouldBe` Just "volca.toml"
                Left err -> expectationFailure err

        it "parses --db NAME and --methods PATH" $ do
            case runParse ["--db", "ecoinvent", "--methods", "/m", "methods"] of
                Right cfg -> do
                    dbName (globalOptions cfg) `shouldBe` Just "ecoinvent"
                    methodsDir (globalOptions cfg) `shouldBe` Just "/m"
                Left err -> expectationFailure err

        it "parses --format pretty" $ do
            case runParse ["--format", "pretty", "methods"] of
                Right cfg -> format (globalOptions cfg) `shouldBe` Just Pretty
                Left err -> expectationFailure err

        it "rejects an unknown --format value with a non-zero exit" $ do
            case runParse ["--format", "yaml", "methods"] of
                Left _ -> pure ()
                Right _ -> expectationFailure "Expected parse failure on --format yaml"

        it "defaults noCache to False when --no-cache is absent" $ do
            case runParse ["methods"] of
                Right cfg -> noCache (globalOptions cfg) `shouldBe` False
                Left err -> expectationFailure err

        it "sets noCache to True when --no-cache is given" $ do
            case runParse ["--no-cache", "methods"] of
                Right cfg -> noCache (globalOptions cfg) `shouldBe` True
                Left err -> expectationFailure err

    describe "no command → load-only mode" $ do
        it "accepts an invocation with only global options" $ do
            case runParse ["--config", "volca.toml"] of
                Right cfg -> command cfg `shouldBe` Nothing
                Left err -> expectationFailure err

    describe "listing commands" $ do
        let listingCases =
                [ (["methods"], Methods)
                , (["synonyms"], Synonyms)
                , (["compartment-mappings"], CompartmentMappings)
                , (["units"], Units)
                , (["stop"], Stop)
                , (["repl"], Repl)
                ]
        mapM_
            ( \(argv, expected) ->
                it ("parses `" <> unwords argv <> "` → " <> show expected) $ do
                    cmd <- parseCmd argv
                    cmd `shouldBe` expected
            )
            listingCases

    describe "resource subcommands" $ do
        it "parses `database` with no subcommand → DbList" $ do
            cmd <- parseCmd ["database"]
            cmd `shouldBe` Database DbList

        it "parses `database list`" $ do
            cmd <- parseCmd ["database", "list"]
            cmd `shouldBe` Database DbList

        it "parses `database delete NAME`" $ do
            cmd <- parseCmd ["database", "delete", "ecoinvent"]
            cmd `shouldBe` Database (DbDelete "ecoinvent")

        it "parses `database edit-exchanges DB --process-id PID --from FILE`" $ do
            cmd <- parseCmd ["database", "edit-exchanges", "mine", "--process-id", "a_b", "--from", "edits.json"]
            cmd `shouldBe` Database (DbEditExchanges (DbActivityArgs "mine" "a_b" "edits.json"))

        it "parses `database upload FILE --name NAME`" $ do
            cmd <- parseCmd ["database", "upload", "db.7z", "--name", "My DB"]
            case cmd of
                Database (DbUpload args) -> do
                    uaFile args `shouldBe` "db.7z"
                    uaName args `shouldBe` "My DB"
                    uaDescription args `shouldBe` Nothing
                _ -> expectationFailure ("Unexpected command: " <> show cmd)

        it "parses `database upload FILE --name N --description D`" $ do
            cmd <- parseCmd ["database", "upload", "db.7z", "--name", "N", "--description", "D"]
            case cmd of
                Database (DbUpload args) -> uaDescription args `shouldBe` Just "D"
                _ -> expectationFailure ("Unexpected command: " <> show cmd)

        it "parses `method` with no subcommand → McList" $ do
            cmd <- parseCmd ["method"]
            cmd `shouldBe` Method McList

    describe "server command" $ do
        it "parses `server` with defaults" $ do
            cmd <- parseCmd ["server"]
            case cmd of
                Server opts -> do
                    serverPort opts `shouldBe` Nothing
                    serverIdleTimeout opts `shouldBe` 0
                    serverTreeDepth opts `shouldBe` 2
                    serverDesktopMode opts `shouldBe` False
                _ -> expectationFailure "Expected Server command"

        it "parses `server --port 9000 --idle-timeout 600 --tree-depth 5 --desktop`" $ do
            cmd <- parseCmd ["server", "--port", "9000", "--idle-timeout", "600", "--tree-depth", "5", "--desktop"]
            case cmd of
                Server opts -> do
                    serverPort opts `shouldBe` Just 9000
                    serverIdleTimeout opts `shouldBe` 600
                    serverTreeDepth opts `shouldBe` 5
                    serverDesktopMode opts `shouldBe` True
                _ -> expectationFailure "Expected Server command"

        it "parses `server --load db1,db2,db3` as a list" $ do
            cmd <- parseCmd ["server", "--load", "db1,db2,db3"]
            case cmd of
                Server opts -> serverLoadDbs opts `shouldBe` Just ["db1", "db2", "db3"]
                _ -> expectationFailure "Expected Server command"

    describe "resource queries" $ do
        it "parses `activity UUID`" $ do
            cmd <- parseCmd ["activity", "abc-123"]
            cmd `shouldBe` Activity "abc-123"

        it "parses `inventory UUID`" $ do
            cmd <- parseCmd ["inventory", "abc-123"]
            cmd `shouldBe` Inventory "abc-123"

        it "parses `flow FLOW_ID activities`" $ do
            cmd <- parseCmd ["flow", "flow-1", "activities"]
            cmd `shouldBe` Flow "flow-1" (Just FlowActivities)

        it "parses `impacts UUID --method MID`" $ do
            cmd <- parseCmd ["impacts", "abc-123", "--method", "method-uuid"]
            case cmd of
                Impacts uuid opts -> do
                    uuid `shouldBe` "abc-123"
                    lciaMethod opts `shouldBe` "method-uuid"
                _ -> expectationFailure ("Unexpected command: " <> show cmd)

        it "parses `supply-chain` with its filters and threshold" $ do
            cmd <- parseCmd ["supply-chain", "p", "--geo", "FR", "--max-depth", "2", "--min-quantity", "0.5"]
            cmd
                `shouldBe` SupplyChain
                    "p"
                    SupplyChainOptions
                        { scReach = ReachOptions Nothing (Just "FR") Nothing (Just 2) Nothing Nothing
                        , scMinQuantity = Just 0.5
                        }

        it "parses `consumers` and refuses a threshold it has no use for" $ do
            cmd <- parseCmd ["consumers", "p", "--name", "tomato"]
            cmd `shouldBe` Consumers "p" (ReachOptions (Just "tomato") Nothing Nothing Nothing Nothing Nothing)
            runParse ["consumers", "p", "--min-quantity", "1"] `shouldSatisfy` isLeft

        it "parses `contributing-flows` and asks for the method's breakdown" $ do
            cmd <- parseCmd ["contributing-flows", "p", "--method", "Climate change", "--limit", "3", "--exclude-long-term"]
            let opts = ContributionOptions (LCIAOptions "Climate change" Nothing) (Just 3) ExcludeLongTerm
            cmd `shouldBe` Contributing ContributingFlows "p" opts
            contributionPath "db" "p" ContributingFlows opts (MethodRow "m1" "Climate change" "EF 3.1")
                `shouldBe` "/api/v1/db/db/activity/p/contributing-flows/EF%203.1/m1?limit=3&exclude-long-term=true"

        it "parses `explain-cf FLOW_ID --method M [--collection C]`" $ do
            parseCmd ["explain-cf", "f", "--method", "m"] `shouldReturn` ExplainCF "f" (LCIAOptions "m" Nothing)
            parseCmd ["explain-cf", "f", "--method", "m", "--collection", "c"] `shouldReturn` ExplainCF "f" (LCIAOptions "m" (Just "c"))

        it "parses `path-to PROCESS_ID NAME`" $
            parseCmd ["path-to", "p", "electricity"] `shouldReturn` PathTo "p" (NamePart "electricity")

        it "reads what `flow-mapping` lists" $ do
            let parsed argv = parseCmd ("flow-mapping" : "m" : argv)
                listing view = FlowMapping (MappingOptions "m" view Nothing)
            parsed [] `shouldReturn` listing MappingSummary
            parsed ["--matched"] `shouldReturn` listing MatchedFlows
            parsed ["--uncharacterized"] `shouldReturn` listing UncharacterizedFlows
            parsed ["--collection", "c"] `shouldReturn` FlowMapping (MappingOptions "m" MappingSummary (Just "c"))

    describe "rejection" $ do
        it "rejects `flow-mapping` asked for two lists at once" $
            runParse ["flow-mapping", "m", "--matched", "--uncharacterized"] `shouldSatisfy` isLeft

        it "rejects an unknown subcommand" $ do
            case runParse ["floop"] of
                Left _ -> pure ()
                Right _ -> expectationFailure "Expected parse failure on `floop`"

        it "rejects `database upload` without a FILE positional" $ do
            case runParse ["database", "upload", "--name", "X"] of
                Left _ -> pure ()
                Right _ -> expectationFailure "Expected parse failure (missing FILE)"

        it "rejects `database upload FILE` without --name" $ do
            case runParse ["database", "upload", "db.7z"] of
                Left _ -> pure ()
                Right _ -> expectationFailure "Expected parse failure (missing --name)"

        it "rejects `impacts UUID` without --method" $ do
            case runParse ["impacts", "abc"] of
                Left _ -> pure ()
                Right _ -> expectationFailure "Expected parse failure (missing --method)"

    describe "what impacts --method names" $ do
        let land = MethodRow "31aa" "Land occupied" "plain-indicators"
            water = MethodRow "6f11" "Water used" "plain-indicators"
            climate = MethodRow "c1c1" "Climate change" "EF-3.1"
            rows = [land, water, climate]

        it "takes a method by its UUID" $
            resolveImpactTarget Nothing "6f11" rows `shouldBe` Right (OneMethod water)

        it "takes a method by its name, whatever the case" $
            resolveImpactTarget Nothing "climate CHANGE" rows `shouldBe` Right (OneMethod climate)

        it "takes a collection by its name, to score every method in it" $
            resolveImpactTarget Nothing "EF-3.1" rows `shouldBe` Right (WholeCollection "EF-3.1")

        it "refuses a name two collections carry, naming both" $
            case resolveImpactTarget Nothing "Climate change" (rows ++ [MethodRow "c2c2" "Climate change" "EF-3.0"]) of
                Left err -> do
                    err `shouldSatisfy` T.isInfixOf "EF-3.0"
                    err `shouldSatisfy` T.isInfixOf "EF-3.1"
                    err `shouldSatisfy` T.isInfixOf "--collection"
                Right t -> expectationFailure ("expected a refusal, got " <> show t)

        it "refuses a name shared by a method and a collection" $
            resolveImpactTarget Nothing "EF-3.1" (rows ++ [MethodRow "e5e5" "EF-3.1" "custom"]) `shouldSatisfy` isLeft

        it "lists the collections when nothing matches" $
            resolveImpactTarget Nothing "nope" rows `shouldBe` Left "No loaded method or collection is called \"nope\". Collections: plain-indicators, EF-3.1"

        it "says no collection is loaded rather than listing none" $
            resolveImpactTarget Nothing "Climate change" [] `shouldBe` Left "No method collection is loaded."

        it "takes the one of two same-named methods --collection names" $
            resolveImpactTarget (Just "ef-3.0") "Climate change" (rows ++ [MethodRow "c2c2" "Climate change" "EF-3.0"])
                `shouldBe` Right (OneMethod (MethodRow "c2c2" "Climate change" "EF-3.0"))

        it "refuses a --collection that is not loaded, listing those that are" $
            resolveImpactTarget (Just "EF-9") "Climate change" rows
                `shouldBe` Left "No loaded collection is called \"EF-9\". Collections: plain-indicators, EF-3.1"
