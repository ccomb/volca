{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

{- |
Module      : API.DatabaseHandlers
Description : Database management API handlers

Extracted from API.Routes to reduce module size and improve organization.
Contains handlers for database listing, loading, unloading, uploading, and deletion.
-}
module API.DatabaseHandlers (
    -- * Handlers
    getDatabases,
    loadDatabaseHandler,
    unloadDatabaseHandler,
    relinkDatabaseHandler,
    deleteDatabaseHandler,
    uploadDatabaseHandler,
    uploadMethodHandler,
    deleteMethodHandler,

    -- * Setup Page Handlers
    getDatabaseSetupHandler,
    addDependencyHandler,
    removeDependencyHandler,
    setDataPathHandler,
    finalizeDatabaseHandler,

    -- * Reference data handlers
    RefDataKind (..),
    listRefData,
    loadRefData,
    unloadRefData,
    deleteRefData,
    uploadRefData,
    getFlowSynonymGroupsHandler,
    downloadRefDataHandler,

    -- * Helpers
    convertDbStatus,
    simpleAction,
) where

import Control.Exception (SomeException, try)
import Control.Monad.IO.Class (liftIO)
import qualified Data.ByteString.Base64 as B64
import qualified Data.ByteString.Lazy as BSL
import Data.List (isPrefixOf)
import qualified Data.Map as M
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.IO as T
import Servant (Handler, Header, Headers, addHeader, err400, err404, err500, errBody, throwError)
import qualified System.Directory
import System.FilePath ((</>))

-- Flow synonyms

-- Compartment mappings

-- Unit definitions

import API.Types (
    ActivateResponse (..),
    BinaryContent (..),
    DatabaseListResponse (..),
    DatabaseStatusAPI (..),
    LoadDatabaseResponse (..),
    RefDataListResponse (..),
    RefDataStatusAPI (..),
    RelinkResponse (..),
    SynonymGroupsResponse (..),
    UploadRequest (..),
    UploadResponse (..),
 )
import Config (DatabaseConfig (..), MethodConfig (..), RefDataConfig (..))
import Control.Concurrent.STM (readTVarIO)
import Data.Aeson (Value, toJSON)
import qualified Data.Aeson as A
import qualified Data.Aeson.KeyMap as KM
import qualified Data.Vector as V
import Database.Manager (
    DatabaseLoadStatus (..),
    DatabaseManager (..),
    DatabaseSetupInfo (..),
    DatabaseStatus (..),
    LoadedDatabase (..),
    RefDataStatus (..),
    RelinkResult (..),
    SetupError (..),
    addCompartmentMappings,
    addDatabase,
    addDependencyToStaged,
    addFlowSynonyms,
    addMethodCollection,
    addUnitDefs,
    finalizeDatabase,
    getDatabaseSetupInfo,
    getFlowSynonymGroups,
    listCompartmentMappings,
    listDatabases,
    listFlowSynonyms,
    listUnitDefs,
    loadCompartmentMappings,
    loadDatabase,
    loadFlowSynonyms,
    loadUnitDefs,
    relinkDatabase,
    removeCompartmentMappings,
    removeDatabase,
    removeDependencyFromStaged,
    removeFlowSynonyms,
    removeMethodCollection,
    removeUnitDefs,
    setDataPath,
    unloadCompartmentMappings,
    unloadDatabase,
    unloadFlowSynonyms,
    unloadUnitDefs,
 )
import Database.Upload (
    DatabaseFormat (..),
    UploadData (..),
    UploadResult (..),
    findMethodDirectory,
    handleUpload,
 )
import qualified Database.UploadedDatabase as UploadedDB
import Types (Database (..), unresolvedCount)

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
            let status = makeStatusFromLoadedDb loadedDb
            return $ LoadSucceeded status depResults

-- | Unload a database from memory
unloadDatabaseHandler :: DatabaseManager -> Text -> Handler ActivateResponse
unloadDatabaseHandler dbManager dbName =
    simpleAction (unloadDatabase dbManager dbName) ("Unloaded database: " <> dbName)

{- | Re-run cross-DB linking for a loaded database against the currently-loaded
dependency databases. Lets the user recover from loads that happened in a
suboptimal order without reloading the whole database.
-}
relinkDatabaseHandler :: DatabaseManager -> Text -> Handler RelinkResponse
relinkDatabaseHandler dbManager dbName = do
    res <- liftIO $ relinkDatabase dbManager dbName
    case res of
        Left err -> throwError err404{errBody = BSL.fromStrict $ T.encodeUtf8 err}
        Right r ->
            return
                RelinkResponse
                    { rrDbName = rresDbName r
                    , rrUnresolvedBefore = rresUnresolvedBefore r
                    , rrUnresolvedAfter = rresUnresolvedAfter r
                    , rrCrossDBLinks = rresCrossDBLinks r
                    , rrDependsOn = rresDepsLoaded r
                    }

-- | Delete an uploaded database (move to trash)
deleteDatabaseHandler :: DatabaseManager -> Text -> Handler ActivateResponse
deleteDatabaseHandler dbManager dbName =
    simpleAction (removeDatabase dbManager dbName) ("Deleted database: " <> dbName)

-- | Upload a new database
uploadDatabaseHandler :: DatabaseManager -> UploadRequest -> Handler UploadResponse
uploadDatabaseHandler dbManager req = do
    -- Decode base64 ZIP data
    let zipDataResult = B64.decode $ T.encodeUtf8 $ urFileData req
    case zipDataResult of
        Left err -> return $ UploadResponse False ("Invalid base64 data: " <> T.pack err) Nothing Nothing
        Right zipBytes -> do
            let uploadData =
                    UploadData
                        { udName = urName req
                        , udDescription = urDescription req
                        , udZipData = BSL.fromStrict zipBytes
                        }
            -- Handle the upload (extract, detect format)
            uploadsDir <- liftIO UploadedDB.getDatabaseUploadsDir
            result <- liftIO $ handleUpload uploadsDir uploadData (\_ -> return ())

            case result of
                Left err ->
                    return $ UploadResponse False err Nothing Nothing
                Right uploadResult -> do
                    let uploadDir = uploadsDir </> T.unpack (urSlug uploadResult)

                    -- Create meta.toml for self-describing upload
                    let meta =
                            UploadedDB.UploadMeta
                                { UploadedDB.umVersion = 1
                                , UploadedDB.umDisplayName = urName req
                                , UploadedDB.umDescription = urDescription req
                                , UploadedDB.umFormat = urFormat uploadResult -- Types are now unified
                                , UploadedDB.umDataPath = makeRelative uploadDir (urPath uploadResult)
                                }
                    liftIO $ UploadedDB.writeUploadMeta uploadDir meta

                    -- Create database config for in-memory manager
                    let dbConfig =
                            DatabaseConfig
                                { dcName = urSlug uploadResult
                                , dcDisplayName = urName req
                                , dcPath = urPath uploadResult
                                , dcDescription = urDescription req
                                , dcLoad = False -- Don't auto-load
                                , dcDefault = False
                                , dcDepends = []
                                , dcLocationAliases = M.empty
                                , dcFormat = Just (urFormat uploadResult)
                                , dcIsUploaded = True -- Freshly uploaded database
                                }

                    -- Add to manager
                    liftIO $ addDatabase dbManager dbConfig

                    return $
                        UploadResponse
                            True
                            "Database uploaded successfully"
                            (Just $ urSlug uploadResult)
                            (Just $ formatToText $ urFormat uploadResult)

-- | Convert DatabaseManager.DatabaseStatus to API.DatabaseStatusAPI
convertDbStatus :: DatabaseStatus -> DatabaseStatusAPI
convertDbStatus ds =
    DatabaseStatusAPI
        { dsaName = dsName ds
        , dsaDisplayName = dsDisplayName ds
        , dsaDescription = dsDescription ds
        , dsaLoadAtStartup = dsLoadAtStartup ds
        , dsaStatus = statusToText (dsStatus ds)
        , dsaIsUploaded = dsIsUploaded ds
        , dsaPath = dsPath ds
        , dsaFormat = formatDisplayText <$> dsFormat ds
        , dsaActivityCount = dsActivityCount ds
        }
  where
    statusToText Unloaded = "unloaded"
    statusToText PartiallyLinked = "partially_linked"
    statusToText Loaded = "loaded"

-- | Create DatabaseStatusAPI from a loaded database (derives status from linking stats)
makeStatusFromLoadedDb :: LoadedDatabase -> DatabaseStatusAPI
makeStatusFromLoadedDb loaded =
    let config = ldConfig loaded
        db = ldDatabase loaded
        status =
            if unresolvedCount (dbLinkingStats db) > 0
                then "partially_linked"
                else "loaded"
     in DatabaseStatusAPI
            { dsaName = dcName config
            , dsaDisplayName = dcDisplayName config
            , dsaDescription = dcDescription config
            , dsaLoadAtStartup = dcLoad config
            , dsaStatus = status
            , dsaIsUploaded = dcIsUploaded config
            , dsaPath = T.pack (dcPath config)
            , dsaFormat = formatDisplayText <$> dcFormat config
            , dsaActivityCount = V.length (dbActivities db)
            }

-- uploadFormatToMeta removed - types are now unified (UploadedDB re-exports from Upload)

-- | Make a path relative to a base directory
makeRelative :: FilePath -> FilePath -> FilePath
makeRelative base path
    | base `isPrefixOf` path = drop (length base + 1) path -- +1 for separator
    | otherwise = path

-- | Convert DatabaseFormat to display text (uses ToJSON instance: "EcoSpold 2", etc.)
formatDisplayText :: DatabaseFormat -> Text
formatDisplayText fmt = case toJSON fmt of
    A.String t -> t
    _ -> ""

-- | Convert DatabaseFormat to API slug text
formatToText :: DatabaseFormat -> Text
formatToText SimaProCSV = "simapro-csv"
formatToText EcoSpold1 = "ecospold1"
formatToText EcoSpold2 = "ecospold2"
formatToText ILCDProcess = "ilcd"
formatToText UnknownFormat = "unknown"

--------------------------------------------------------------------------------
-- Setup Page Handlers
--------------------------------------------------------------------------------

{- | Get database setup info
Returns completeness, missing suppliers, and dependency suggestions
-}
getDatabaseSetupHandler :: DatabaseManager -> Text -> Handler DatabaseSetupInfo
getDatabaseSetupHandler dbManager dbName = do
    eitherResult <- liftIO $ try $ getDatabaseSetupInfo dbManager dbName
    case eitherResult of
        Left (ex :: SomeException) ->
            throwError $ err500{errBody = BSL.fromStrict $ T.encodeUtf8 $ "Setup failed: " <> T.pack (show ex)}
        Right (Left (SetupNotFound msg)) -> throwError $ err404{errBody = BSL.fromStrict $ T.encodeUtf8 msg}
        Right (Left (SetupFailed msg)) -> throwError $ err500{errBody = BSL.fromStrict $ T.encodeUtf8 msg}
        Right (Right setupInfo) -> return setupInfo

{- | Add a dependency to a staged database
Runs cross-DB linking and returns updated setup info
-}
addDependencyHandler :: DatabaseManager -> Text -> Text -> Handler DatabaseSetupInfo
addDependencyHandler dbManager dbName depName = do
    result <- liftIO $ addDependencyToStaged dbManager dbName depName
    case result of
        Left err -> throwError $ err400{errBody = BSL.fromStrict $ T.encodeUtf8 err}
        Right setupInfo -> return setupInfo

{- | Remove a dependency from a staged database
Re-runs cross-DB linking and returns updated setup info
-}
removeDependencyHandler :: DatabaseManager -> Text -> Text -> Handler DatabaseSetupInfo
removeDependencyHandler dbManager dbName depName = do
    result <- liftIO $ removeDependencyFromStaged dbManager dbName depName
    case result of
        Left err -> throwError $ err400{errBody = BSL.fromStrict $ T.encodeUtf8 err}
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
        Nothing -> throwError $ err400{errBody = "Missing \"path\" field in request body"}
        Just newPath -> do
            result <- liftIO $ setDataPath dbManager dbName newPath
            case result of
                Left err -> throwError $ err400{errBody = BSL.fromStrict $ T.encodeUtf8 err}
                Right setupInfo -> return setupInfo

{- | Finalize a staged database
Builds matrices and makes it ready for queries
-}
finalizeDatabaseHandler :: DatabaseManager -> Text -> Handler ActivateResponse
finalizeDatabaseHandler dbManager dbName = do
    eitherResult <- liftIO $ try $ finalizeDatabase dbManager dbName
    case eitherResult of
        Left (ex :: SomeException) ->
            return $ ActivateResponse False ("Server exception: " <> T.pack (show ex)) Nothing
        Right (Left err) -> return $ ActivateResponse False err Nothing
        Right (Right loaded) -> do
            let status = makeStatusFromLoadedDb loaded
            return $ ActivateResponse True ("Finalized database: " <> dcDisplayName (ldConfig loaded)) (Just status)

{- | Upload a new method collection
Same flow as database upload but creates MethodConfig entry
-}
uploadMethodHandler :: DatabaseManager -> UploadRequest -> Handler UploadResponse
uploadMethodHandler dbManager req = do
    let zipDataResult = B64.decode $ T.encodeUtf8 $ urFileData req
    case zipDataResult of
        Left err -> return $ UploadResponse False ("Invalid base64 data: " <> T.pack err) Nothing Nothing
        Right zipBytes -> do
            let uploadData =
                    UploadData
                        { udName = urName req
                        , udDescription = urDescription req
                        , udZipData = BSL.fromStrict zipBytes
                        }
            uploadsDir <- liftIO UploadedDB.getMethodUploadsDir
            result <- liftIO $ handleUpload uploadsDir uploadData (\_ -> return ())
            case result of
                Left err ->
                    return $ UploadResponse False err Nothing Nothing
                Right uploadResult -> do
                    let uploadDir = uploadsDir </> T.unpack (urSlug uploadResult)

                    -- Find the actual method XML directory (e.g. ILCD/lciamethods/)
                    methodDir <- liftIO $ findMethodDirectory uploadDir

                    -- Create meta.toml (store path relative to upload dir)
                    let meta =
                            UploadedDB.UploadMeta
                                { UploadedDB.umVersion = 1
                                , UploadedDB.umDisplayName = urName req
                                , UploadedDB.umDescription = urDescription req
                                , UploadedDB.umFormat = urFormat uploadResult
                                , UploadedDB.umDataPath = makeRelative uploadDir methodDir
                                }
                    liftIO $ UploadedDB.writeUploadMeta uploadDir meta

                    -- Create MethodConfig and add to manager
                    let mc =
                            MethodConfig
                                { mcName = urName req
                                , mcPath = methodDir
                                , mcActive = False
                                , mcIsUploaded = True
                                , mcDescription = urDescription req
                                , mcFormat = Just $ formatToText $ urFormat uploadResult
                                , mcScoringSets = []
                                }
                    liftIO $ addMethodCollection dbManager mc

                    return $
                        UploadResponse
                            True
                            "Method uploaded successfully"
                            (Just $ urSlug uploadResult)
                            (Just "ILCD")

-- | Delete an uploaded method collection
deleteMethodHandler :: DatabaseManager -> Text -> Handler ActivateResponse
deleteMethodHandler dbManager name =
    simpleAction (removeMethodCollection dbManager name) ("Deleted method: " <> name)

-- | Common pattern: run an IO action that returns Either Text (), map to ActivateResponse
simpleAction :: IO (Either Text ()) -> Text -> Handler ActivateResponse
simpleAction action successMsg = do
    result <- liftIO action
    return $ case result of
        Left err -> ActivateResponse False err Nothing
        Right () -> ActivateResponse True successMsg Nothing

--------------------------------------------------------------------------------
-- Reference Data Handlers (flow synonyms, compartment mappings, units)
--------------------------------------------------------------------------------

-- | Which kind of reference data we're operating on
data RefDataKind = FlowSynonyms | CompartmentMappings | UnitDefs

-- | Dispatch to the right Manager functions based on kind
rdOps ::
    RefDataKind ->
    ( DatabaseManager -> IO [RefDataStatus]
    , DatabaseManager -> Text -> IO (Either Text ())
    , DatabaseManager -> Text -> IO (Either Text ())
    , DatabaseManager -> RefDataConfig -> IO ()
    , DatabaseManager -> Text -> IO (Either Text ())
    , Text -- upload subdir
    )
rdOps FlowSynonyms =
    (listFlowSynonyms, loadFlowSynonyms, unloadFlowSynonyms, addFlowSynonyms, removeFlowSynonyms, "flow-synonyms")
rdOps CompartmentMappings =
    (listCompartmentMappings, loadCompartmentMappings, unloadCompartmentMappings, addCompartmentMappings, removeCompartmentMappings, "compartment-mappings")
rdOps UnitDefs =
    (listUnitDefs, loadUnitDefs, unloadUnitDefs, addUnitDefs, removeUnitDefs, "units")

convertRefDataStatus :: RefDataStatus -> RefDataStatusAPI
convertRefDataStatus s =
    RefDataStatusAPI
        { rdaName = rdsName s
        , rdaDisplayName = rdsDisplayName s
        , rdaDescription = rdsDescription s
        , rdaStatus = case rdsStatus s of Loaded -> "loaded"; _ -> "unloaded"
        , rdaIsUploaded = rdsIsUploaded s
        , rdaIsAuto = rdsIsAuto s
        , rdaEntryCount = rdsEntryCount s
        }

listRefData :: RefDataKind -> DatabaseManager -> Handler RefDataListResponse
listRefData kind mgr = do
    let (listFn, _, _, _, _, _) = rdOps kind
    statuses <- liftIO $ listFn mgr
    return $ RefDataListResponse (map convertRefDataStatus statuses)

loadRefData :: RefDataKind -> DatabaseManager -> Text -> Handler ActivateResponse
loadRefData kind mgr name = do
    let (_, loadFn, _, _, _, _) = rdOps kind
    simpleAction (loadFn mgr name) ("Loaded: " <> name)

unloadRefData :: RefDataKind -> DatabaseManager -> Text -> Handler ActivateResponse
unloadRefData kind mgr name = do
    let (_, _, unloadFn, _, _, _) = rdOps kind
    simpleAction (unloadFn mgr name) ("Unloaded: " <> name)

deleteRefData :: RefDataKind -> DatabaseManager -> Text -> Handler ActivateResponse
deleteRefData kind mgr name = do
    let (_, _, _, _, removeFn, _) = rdOps kind
    simpleAction (removeFn mgr name) ("Deleted: " <> name)

uploadRefData :: RefDataKind -> DatabaseManager -> UploadRequest -> Handler UploadResponse
uploadRefData kind mgr req = do
    let (_, _, _, addFn, _, subdir) = rdOps kind
    let csvDataResult = B64.decode $ T.encodeUtf8 $ urFileData req
    case csvDataResult of
        Left err -> return $ UploadResponse False ("Invalid base64 data: " <> T.pack err) Nothing Nothing
        Right csvBytes -> do
            baseDir <- liftIO UploadedDB.getDataDir
            let slug = T.toLower $ T.intercalate "-" $ T.words $ urName req
                uploadDir = baseDir </> "uploads" </> T.unpack subdir </> T.unpack slug
                csvPath = uploadDir </> "data.csv"
            liftIO $ do
                System.Directory.createDirectoryIfMissing True uploadDir
                BSL.writeFile csvPath (BSL.fromStrict csvBytes)
                let metaContent =
                        T.intercalate
                            "\n"
                            [ "[meta]"
                            , "version = 1"
                            , "displayName = " <> quote (urName req)
                            , maybe "" (\d -> "description = " <> quote d) (urDescription req)
                            , ""
                            ]
                T.writeFile (uploadDir </> "meta.toml") metaContent
            let rd =
                    RefDataConfig
                        { rdName = urName req
                        , rdPath = csvPath
                        , rdActive = False
                        , rdIsUploaded = True
                        , rdIsAuto = False
                        , rdDescription = urDescription req
                        }
            liftIO $ addFn mgr rd
            return $ UploadResponse True "Uploaded successfully" (Just slug) Nothing
  where
    quote t = "\"" <> T.replace "\"" "\\\"" t <> "\""

getFlowSynonymGroupsHandler :: DatabaseManager -> Text -> Handler SynonymGroupsResponse
getFlowSynonymGroupsHandler mgr name = do
    result <- liftIO $ getFlowSynonymGroups mgr name
    case result of
        Left err -> throwError $ err404{errBody = BSL.fromStrict $ T.encodeUtf8 err}
        Right groups -> return $ SynonymGroupsResponse groups

downloadRefDataHandler :: RefDataKind -> DatabaseManager -> Text -> Handler (Headers '[Header "Content-Disposition" Text] BinaryContent)
downloadRefDataHandler kind mgr name = do
    let tvar = case kind of
            FlowSynonyms -> dmAvailableFlowSyns mgr
            CompartmentMappings -> dmAvailableCompMaps mgr
            UnitDefs -> dmAvailableUnitDefs mgr
    available <- liftIO $ readTVarIO tvar
    case M.lookup name available of
        Nothing -> throwError $ err404{errBody = "Not found"}
        Just rd -> do
            let csvPath = rdPath rd
            exists <- liftIO $ System.Directory.doesFileExist csvPath
            if not exists
                then throwError $ err404{errBody = "CSV file not found"}
                else do
                    content <- liftIO $ BSL.readFile csvPath
                    let disposition = "attachment; filename=\"" <> name <> ".csv\""
                    return $ addHeader disposition (BinaryContent content)
