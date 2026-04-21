{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module ConfigWriter (
    -- * File locking
    withConfigLock,

    -- * Config modification
    addDatabaseToConfig,
    removeDatabaseFromConfig,
    updateDatabaseLoadFlag,
) where

import Control.Exception (SomeException, try)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.Directory (doesFileExist, renameFile)
import System.FileLock (SharedExclusive (..), withFileLock)
import System.FilePath ((<.>))

import Config (Config (..), DatabaseConfig (..), loadConfigFile)

-- | Execute an action while holding an exclusive lock on the config file
withConfigLock :: FilePath -> IO a -> IO a
withConfigLock configPath action = do
    let lockPath = configPath <.> "lock"
    withFileLock lockPath Exclusive $ \_ -> action

{- | Add a new database to the config file
Appends a new [[databases]] section at the end of the file
Returns error if a database with the same name already exists
-}
addDatabaseToConfig :: FilePath -> DatabaseConfig -> IO (Either Text ())
addDatabaseToConfig configPath dbConfig = withConfigLock configPath $ do
    exists <- doesFileExist configPath
    if not exists
        then return $ Left $ "Config file not found: " <> T.pack configPath
        else do
            -- First check if database with this name already exists
            configResult <- loadConfigFile configPath
            case configResult of
                Left err -> return $ Left $ "Failed to parse config: " <> err
                Right config -> do
                    let existingNames = map dcName (cfgDatabases config)
                    if dcName dbConfig `elem` existingNames
                        then return $ Left $ "Database already exists: " <> dcName dbConfig
                        else do
                            result <- try $ do
                                content <- TIO.readFile configPath
                                let newSection = formatDatabaseSection dbConfig
                                    newContent = content <> "\n" <> newSection
                                atomicWriteFile configPath newContent
                            case result of
                                Left (e :: SomeException) ->
                                    return $ Left $ "Failed to write config: " <> T.pack (show e)
                                Right () ->
                                    return $ Right ()

{- | Remove a database from the config file by name
Rewrites the entire config file, preserving order but removing the target
-}
removeDatabaseFromConfig :: FilePath -> Text -> IO (Either Text ())
removeDatabaseFromConfig configPath dbName = withConfigLock configPath $ do
    exists <- doesFileExist configPath
    if not exists
        then return $ Left $ "Config file not found: " <> T.pack configPath
        else do
            -- Parse existing config
            configResult <- loadConfigFile configPath
            case configResult of
                Left err -> return $ Left err
                Right config -> do
                    let databases = cfgDatabases config
                        remaining = filter (\db -> dcName db /= dbName) databases
                    if length remaining == length databases
                        then return $ Left $ "Database not found: " <> dbName
                        else do
                            -- Read the raw file to preserve non-database sections
                            content <- TIO.readFile configPath
                            let newContent = rewriteConfigWithDatabases content remaining
                            result <- try $ atomicWriteFile configPath newContent
                            case result of
                                Left (e :: SomeException) ->
                                    return $ Left $ "Failed to write config: " <> T.pack (show e)
                                Right () ->
                                    return $ Right ()

-- | Update the load flag for a database
updateDatabaseLoadFlag :: FilePath -> Text -> Bool -> IO (Either Text ())
updateDatabaseLoadFlag configPath dbName newLoadValue = withConfigLock configPath $ do
    exists <- doesFileExist configPath
    if not exists
        then return $ Left $ "Config file not found: " <> T.pack configPath
        else do
            content <- TIO.readFile configPath
            let updatedContent = updateLoadInText content dbName newLoadValue
            result <- try $ atomicWriteFile configPath updatedContent
            case result of
                Left (e :: SomeException) ->
                    return $ Left $ "Failed to write config: " <> T.pack (show e)
                Right () ->
                    return $ Right ()

-- | Format a DatabaseConfig as a TOML [[databases]] section
formatDatabaseSection :: DatabaseConfig -> Text
formatDatabaseSection DatabaseConfig{..} =
    T.unlines $
        [ "# Uploaded database"
        , "[[databases]]"
        , "name = " <> quote dcName
        , "displayName = " <> quote dcDisplayName
        , "path = " <> quote (T.pack dcPath)
        ]
            ++ maybeField "description" dcDescription
            ++ [ "load = " <> boolToText dcLoad
               , "default = " <> boolToText dcDefault
               ]
            ++ dependsField dcDepends
  where
    quote t = "\"" <> escapeToml t <> "\""
    boolToText True = "true"
    boolToText False = "false"
    maybeField name = maybe [] (\v -> [name <> " = " <> quote v])
    dependsField [] = []
    dependsField ds = ["depends = [" <> T.intercalate ", " (map quote ds) <> "]"]

-- | Escape special characters in TOML strings
escapeToml :: Text -> Text
escapeToml = T.concatMap escape
  where
    escape '"' = "\\\""
    escape '\\' = "\\\\"
    escape '\n' = "\\n"
    escape '\r' = "\\r"
    escape '\t' = "\\t"
    escape c = T.singleton c

{- | Rewrite config file with new database list
Preserves [server] and [[methods]] sections, replaces all [[databases]]
-}
rewriteConfigWithDatabases :: Text -> [DatabaseConfig] -> Text
rewriteConfigWithDatabases originalContent databases =
    let lines' = T.lines originalContent
        -- Extract sections before databases
        (beforeDbs, afterDbsStart) = break isDatabaseHeader lines'
        -- Skip all database sections
        afterDbs = skipDatabaseSections afterDbsStart
        -- Generate new database sections
        dbSections = T.intercalate "\n" $ map formatDatabaseSection databases
        -- Combine: before + new databases + after (methods, etc.)
        newLines = beforeDbs ++ [dbSections] ++ afterDbs
     in T.unlines newLines
  where
    isDatabaseHeader line = T.strip line == "[[databases]]"

    -- Skip lines until we hit a non-database section or EOF
    skipDatabaseSections [] = []
    skipDatabaseSections (l : ls)
        | isDatabaseHeader l = skipDatabaseSections (dropDatabaseContent ls)
        | isOtherSection l = l : ls
        | otherwise = skipDatabaseSections ls

    -- Drop content belonging to current database until next section
    dropDatabaseContent [] = []
    dropDatabaseContent (l : ls)
        | isDatabaseHeader l = l : ls -- Start of next database
        | isOtherSection l = l : ls -- Start of other section
        | otherwise = dropDatabaseContent ls

    isOtherSection line =
        let stripped = T.strip line
         in (T.isPrefixOf "[[" stripped && not (T.isPrefixOf "[[databases]]" stripped))
                || (T.isPrefixOf "[" stripped && not (T.isPrefixOf "[[" stripped))

{- | Update load flag in raw TOML text for a specific database
Simple text-based replacement within the correct database section
-}
updateLoadInText :: Text -> Text -> Bool -> Text
updateLoadInText content dbName newLoad =
    let lines' = T.lines content
     in T.unlines $ updateInSection lines' False
  where
    updateInSection [] _ = []
    updateInSection (l : ls) inTargetDb
        | T.strip l == "[[databases]]" =
            -- Starting a new database section, check if it's our target
            l : updateInSection ls (isTargetDb ls)
        | inTargetDb && isLoadLine l =
            -- Replace load/active line
            replaceLoadValue l newLoad : updateInSection ls inTargetDb
        | T.isPrefixOf "[[" (T.strip l) =
            -- Entering a different section
            l : updateInSection ls False
        | otherwise =
            l : updateInSection ls inTargetDb

    isTargetDb [] = False
    isTargetDb (l : ls)
        | T.isPrefixOf "name" (T.strip l) =
            T.isInfixOf ("\"" <> dbName <> "\"") l
        | T.strip l == "[[databases]]" = False
        | T.isPrefixOf "[[" (T.strip l) = False
        | otherwise = isTargetDb ls

    isLoadLine l =
        let stripped = T.strip l
         in T.isPrefixOf "load" stripped || T.isPrefixOf "active" stripped

    replaceLoadValue l newVal =
        let stripped = T.strip l
            indent = T.takeWhile (== ' ') l
            key = if T.isPrefixOf "active" stripped then "active" else "load"
            val = if newVal then "true" else "false"
         in indent <> key <> " = " <> val

-- | Write content to a temp file and atomically rename
atomicWriteFile :: FilePath -> Text -> IO ()
atomicWriteFile targetPath content = do
    -- Write to temp file in same directory (for atomic rename)
    let tempPath = targetPath <.> "tmp"
    TIO.writeFile tempPath content
    -- Atomic rename
    renameFile tempPath targetPath
