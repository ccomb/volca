{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

{- |
Module      : API.DatabaseHandlers
Description : Database management API handlers

Extracted from API.Routes to reduce module size and improve organization.
Contains handlers for database listing, loading, unloading, uploading, and deletion.
-}
module API.DatabaseHandlers
    ( -- * Handlers
      getDatabases
    , loadDatabaseHandler
    , unloadDatabaseHandler
    , deleteDatabaseHandler
    , uploadDatabaseHandler
      -- * Setup Page Handlers
    , getDatabaseSetupHandler
    , addDependencyHandler
    , removeDependencyHandler
    , setDataPathHandler
    , finalizeDatabaseHandler
      -- * Helpers
    , convertDbStatus
    , convertLoadedDbToStatus
    ) where

import Control.Exception (SomeException, try)
import Control.Monad.IO.Class (liftIO)
import Data.List (isPrefixOf)
import Data.Text (Text)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base64 as B64
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Map as M
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Servant (Handler, throwError, errBody, err400, err404, err500)
import System.FilePath ((</>))

import qualified Config
import Config (DatabaseConfig(..))
import Data.Aeson (Value)
import qualified Data.Aeson as A
import qualified Data.Aeson.KeyMap as KM
import Database.Manager
    ( DatabaseManager
    , DatabaseStatus(..)
    , DatabaseSetupInfo(..)
    , SetupError(..)
    , DepLoadResult(..)
    , LoadedDatabase(..)
    , addDatabase
    , addDependencyToStaged
    , finalizeDatabase
    , getDatabaseSetupInfo
    , listDatabases
    , loadDatabase
    , removeDatabase
    , removeDependencyFromStaged
    , setDataPath
    , unloadDatabase
    )
import API.Types
    ( ActivateResponse(..)
    , LoadDatabaseResponse(..)
    , DatabaseListResponse(..)
    , DatabaseStatusAPI(..)
    , UploadRequest(..)
    , UploadResponse(..)
    )
import Database.Upload
    ( DatabaseFormat(..)
    , UploadData(..)
    , UploadResult(..)
    , handleUpload
    )
import qualified Database.UploadedDatabase as UploadedDB

-- | List all databases
getDatabases :: DatabaseManager -> Handler DatabaseListResponse
getDatabases dbManager = do
    dbStatuses <- liftIO $ listDatabases dbManager
    let statusList = map convertDbStatus dbStatuses
    return $ DatabaseListResponse statusList

-- | Load a database on demand
loadDatabaseHandler :: DatabaseManager -> Text -> Handler LoadDatabaseResponse
loadDatabaseHandler dbManager dbName = do
    eitherResult <- liftIO $ try $ loadDatabase dbManager dbName
    case eitherResult of
        Left (ex :: SomeException) ->
            return $ LoadFailed ("Server exception: " <> T.pack (show ex))
        Right (Left err) -> return $ LoadFailed err
        Right (Right (loadedDb, depResults)) -> do
            let config = ldConfig loadedDb
                status = makeStatusFromConfig config
            return $ LoadSucceeded status depResults

-- | Unload a database from memory
unloadDatabaseHandler :: DatabaseManager -> Text -> Handler ActivateResponse
unloadDatabaseHandler dbManager dbName = do
    result <- liftIO $ unloadDatabase dbManager dbName
    case result of
        Left err -> return $ ActivateResponse False err Nothing
        Right () -> return $ ActivateResponse True ("Unloaded database: " <> dbName) Nothing

-- | Delete an uploaded database (move to trash)
deleteDatabaseHandler :: DatabaseManager -> Text -> Handler ActivateResponse
deleteDatabaseHandler dbManager dbName = do
    result <- liftIO $ removeDatabase dbManager dbName
    case result of
        Left err -> return $ ActivateResponse False err Nothing
        Right () -> return $ ActivateResponse True ("Deleted database: " <> dbName) Nothing

-- | Upload a new database
uploadDatabaseHandler :: DatabaseManager -> UploadRequest -> Handler UploadResponse
uploadDatabaseHandler dbManager req = do
    -- Decode base64 ZIP data
    let zipDataResult = B64.decode $ T.encodeUtf8 $ urFileData req
    case zipDataResult of
        Left err -> return $ UploadResponse False ("Invalid base64 data: " <> T.pack err) Nothing Nothing
        Right zipBytes -> do
            let uploadData = UploadData
                    { udName = urName req
                    , udDescription = urDescription req
                    , udZipData = BSL.fromStrict zipBytes
                    }
            -- Handle the upload (extract, detect format)
            result <- liftIO $ handleUpload uploadData (\_ -> return ())

            case result of
                Left err ->
                    return $ UploadResponse False err Nothing Nothing
                Right uploadResult -> do
                    -- Get upload directory path
                    uploadsDir <- liftIO $ UploadedDB.getUploadsDir
                    let uploadDir = uploadsDir </> T.unpack (urSlug uploadResult)

                    -- Create meta.toml for self-describing upload
                    let meta = UploadedDB.UploadMeta
                            { UploadedDB.umVersion = 1
                            , UploadedDB.umDisplayName = urName req
                            , UploadedDB.umDescription = urDescription req
                            , UploadedDB.umFormat = urFormat uploadResult  -- Types are now unified
                            , UploadedDB.umDataPath = makeRelative uploadDir (urPath uploadResult)
                            }
                    liftIO $ UploadedDB.writeUploadMeta uploadDir meta

                    -- Create database config for in-memory manager
                    let dbConfig = DatabaseConfig
                            { dcName = urSlug uploadResult
                            , dcDisplayName = urName req
                            , dcPath = urPath uploadResult
                            , dcDescription = urDescription req
                            , dcLoad = False  -- Don't auto-load
                            , dcDefault = False
                            , dcDepends = []
                            , dcLocationAliases = M.empty
                            , dcFormat = Just (urFormat uploadResult)
                            , dcIsUploaded = True  -- Freshly uploaded database
                            }

                    -- Add to manager
                    liftIO $ addDatabase dbManager dbConfig

                    return $ UploadResponse True
                        "Database uploaded successfully"
                        (Just $ urSlug uploadResult)
                        (Just $ formatToText $ urFormat uploadResult)

-- | Convert DatabaseManager.DatabaseStatus to API.DatabaseStatusAPI
convertDbStatus :: DatabaseStatus -> DatabaseStatusAPI
convertDbStatus ds = DatabaseStatusAPI
    { dsaName = dsName ds
    , dsaDisplayName = dsDisplayName ds
    , dsaDescription = dsDescription ds
    , dsaLoadAtStartup = dsLoadAtStartup ds
    , dsaLoaded = dsLoaded ds
    , dsaCached = dsCached ds
    , dsaIsUploaded = dsIsUploaded ds
    , dsaPath = dsPath ds
    , dsaFormat = formatToDisplayText <$> dsFormat ds
    }
  where
    formatToDisplayText EcoSpold2 = "EcoSpold 2"
    formatToDisplayText EcoSpold1 = "EcoSpold 1"
    formatToDisplayText SimaProCSV = "SimaPro CSV"
    formatToDisplayText UnknownFormat = ""

-- | Convert LoadedDatabase to DatabaseStatusAPI
convertLoadedDbToStatus :: LoadedDatabase -> DatabaseStatusAPI
convertLoadedDbToStatus loaded =
    let config = ldConfig loaded
    in makeStatusFromConfig config

-- | Create DatabaseStatusAPI from config (loaded database)
makeStatusFromConfig :: DatabaseConfig -> DatabaseStatusAPI
makeStatusFromConfig config = DatabaseStatusAPI
    { dsaName = Config.dcName config
    , dsaDisplayName = Config.dcDisplayName config
    , dsaDescription = Config.dcDescription config
    , dsaLoadAtStartup = Config.dcLoad config
    , dsaLoaded = True
    , dsaCached = True
    , dsaIsUploaded = Config.dcIsUploaded config
    , dsaPath = T.pack (Config.dcPath config)
    , dsaFormat = formatToDisplayText <$> Config.dcFormat config
    }
  where
    formatToDisplayText EcoSpold2 = "EcoSpold 2"
    formatToDisplayText EcoSpold1 = "EcoSpold 1"
    formatToDisplayText SimaProCSV = "SimaPro CSV"
    formatToDisplayText UnknownFormat = ""

-- uploadFormatToMeta removed - types are now unified (UploadedDB re-exports from Upload)

-- | Make a path relative to a base directory
makeRelative :: FilePath -> FilePath -> FilePath
makeRelative base path
    | base `isPrefixOf` path = drop (length base + 1) path  -- +1 for separator
    | otherwise = path

-- | Convert DatabaseFormat to Text
formatToText :: DatabaseFormat -> Text
formatToText SimaProCSV = "simapro-csv"
formatToText EcoSpold1 = "ecospold1"
formatToText EcoSpold2 = "ecospold2"
formatToText UnknownFormat = "unknown"

--------------------------------------------------------------------------------
-- Setup Page Handlers
--------------------------------------------------------------------------------

-- | Get database setup info
-- Returns completeness, missing suppliers, and dependency suggestions
getDatabaseSetupHandler :: DatabaseManager -> Text -> Handler DatabaseSetupInfo
getDatabaseSetupHandler dbManager dbName = do
    result <- liftIO $ getDatabaseSetupInfo dbManager dbName
    case result of
        Left (SetupNotFound msg) -> throwError $ err404 { errBody = BSL.fromStrict $ T.encodeUtf8 msg }
        Left (SetupFailed msg)   -> throwError $ err500 { errBody = BSL.fromStrict $ T.encodeUtf8 msg }
        Right setupInfo -> return setupInfo

-- | Add a dependency to a staged database
-- Runs cross-DB linking and returns updated setup info
addDependencyHandler :: DatabaseManager -> Text -> Text -> Handler DatabaseSetupInfo
addDependencyHandler dbManager dbName depName = do
    result <- liftIO $ addDependencyToStaged dbManager dbName depName
    case result of
        Left err -> throwError $ err400 { errBody = BSL.fromStrict $ T.encodeUtf8 err }
        Right setupInfo -> return setupInfo

-- | Remove a dependency from a staged database
-- Re-runs cross-DB linking and returns updated setup info
removeDependencyHandler :: DatabaseManager -> Text -> Text -> Handler DatabaseSetupInfo
removeDependencyHandler dbManager dbName depName = do
    result <- liftIO $ removeDependencyFromStaged dbManager dbName depName
    case result of
        Left err -> throwError $ err400 { errBody = BSL.fromStrict $ T.encodeUtf8 err }
        Right setupInfo -> return setupInfo

-- | Change the data path for an uploaded (staged) database
setDataPathHandler :: DatabaseManager -> Text -> Value -> Handler DatabaseSetupInfo
setDataPathHandler dbManager dbName body = do
    -- Extract "path" from JSON body
    let mPath = case body of
            A.Object obj -> case KM.lookup "path" obj of
                Just (A.String p) -> Just p
                _ -> Nothing
            _ -> Nothing
    case mPath of
        Nothing -> throwError $ err400 { errBody = "Missing \"path\" field in request body" }
        Just newPath -> do
            result <- liftIO $ setDataPath dbManager dbName newPath
            case result of
                Left err -> throwError $ err400 { errBody = BSL.fromStrict $ T.encodeUtf8 err }
                Right setupInfo -> return setupInfo

-- | Finalize a staged database
-- Builds matrices and makes it ready for queries
finalizeDatabaseHandler :: DatabaseManager -> Text -> Handler ActivateResponse
finalizeDatabaseHandler dbManager dbName = do
    eitherResult <- liftIO $ try $ finalizeDatabase dbManager dbName
    case eitherResult of
        Left (ex :: SomeException) ->
            return $ ActivateResponse False ("Server exception: " <> T.pack (show ex)) Nothing
        Right (Left err) -> return $ ActivateResponse False err Nothing
        Right (Right loaded) -> do
            let config = ldConfig loaded
                status = makeStatusFromConfig config
            return $ ActivateResponse True ("Finalized database: " <> Config.dcDisplayName config) (Just status)
