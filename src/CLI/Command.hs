{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

{- | The commands that load a database in this process instead of asking a
server: they write matrix files beside the caller, which a server has no way
to hand back. Every other command goes through "CLI.Client".
-}
module CLI.Command (
    localCommand,
    requireDatabase,
) where

import CLI.Types (Command (..), DebugMatricesOptions (..))
import Control.Concurrent.STM (readTVarIO)
import qualified Data.Map as M
import Data.Text (Text)
import qualified Data.Text as T
import Database.Manager (DatabaseManager (..), LoadedDatabase (..))
import Progress
import qualified Service
import System.Exit (exitFailure)
import Types (Database)

{- | What a command does with a database loaded here, or 'Nothing' when it
runs elsewhere: on a server, or in @main@ itself for @server@, @repl@, @stop@
and the hidden dumps, which @main@ matches before asking this. Every
constructor is named, so a new command has to be placed on one side or the
other.
-}
localCommand :: Command -> Maybe (Database -> IO ())
localCommand = \case
    DebugMatrices uuid opts -> Just (\db -> debugMatrices db uuid opts)
    ExportMatrices outputDir -> Just (`exportMatrices` outputDir)
    Server _ -> Nothing
    Activity _ -> Nothing
    Flow _ _ -> Nothing
    Inventory _ -> Nothing
    SearchActivities _ -> Nothing
    SearchFlows _ -> Nothing
    Impacts _ _ -> Nothing
    Database _ -> Nothing
    Method _ -> Nothing
    Methods -> Nothing
    Synonyms -> Nothing
    CompartmentMappings -> Nothing
    Units -> Nothing
    FlowMapping _ -> Nothing
    QualityReport _ -> Nothing
    ComputedQualityReport _ -> Nothing
    Stop -> Nothing
    Repl -> Nothing
    Dump _ -> Nothing

-- | Look up a database from the manager by name, or use the single loaded one
requireDatabase :: DatabaseManager -> Maybe Text -> IO Database
requireDatabase manager mName = do
    loadedDbs <- readTVarIO (dmLoadedDbs manager)
    case mName of
        Just name ->
            case M.lookup name loadedDbs of
                Just ld -> return (ldDatabase ld)
                Nothing -> do
                    let available = map T.unpack (M.keys loadedDbs)
                    reportError $ "Database '" ++ T.unpack name ++ "' not found. Available: " ++ unwords available
                    exitFailure
        Nothing ->
            case M.elems loadedDbs of
                [ld] -> return (ldDatabase ld)
                [] -> do
                    reportError "No databases loaded"
                    exitFailure
                _ -> do
                    let available = map T.unpack (M.keys loadedDbs)
                    reportError $ "Multiple databases loaded, use --db to select one: " ++ unwords available
                    exitFailure

debugMatrices :: Database -> Text -> DebugMatricesOptions -> IO ()
debugMatrices database uuid opts = do
    reportProgress Info $ "Extracting matrix debug data for activity: " ++ T.unpack uuid
    reportProgress Info $ "Output base: " ++ debugOutput opts

    case debugFlowFilter opts of
        Just flowFilter -> reportProgress Info $ "Flow filter: " ++ T.unpack flowFilter
        Nothing -> reportProgress Info "No flow filter specified (all biosphere flows)"

    result <- Service.exportMatrixDebugData database uuid opts
    case result of
        Left err -> do
            reportError $ "Error: " ++ show err
            exitFailure
        Right _ -> do
            reportProgress Info "Matrix debug export completed"
            reportProgress Info $ "Supply chain data: " ++ debugOutput opts ++ "_supply_chain.csv"
            reportProgress Info $ "Biosphere matrix: " ++ debugOutput opts ++ "_biosphere_matrix.csv"

exportMatrices :: Database -> FilePath -> IO ()
exportMatrices database outputDir = do
    reportProgress Info $ "Exporting matrices to: " ++ outputDir
    Service.exportUniversalMatrixFormat outputDir database
    reportProgress Info "Matrix export completed"
    reportProgress Info "  - ie_index.csv (activity index)"
    reportProgress Info "  - ee_index.csv (biosphere flow index)"
    reportProgress Info "  - A_public.csv (technosphere matrix)"
    reportProgress Info "  - B_public.csv (biosphere matrix)"
