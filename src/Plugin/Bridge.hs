{-# LANGUAGE OverloadedStrings #-}

{- | External Plugin Bridge

Communicates with external plugin executables via JSON on stdin/stdout.
Each request is a single JSON line; each response is a single JSON line.
Plugins are spawned per-call (stateless). Future: persistent connections.
-}
module Plugin.Bridge (
    callPlugin,
    callPluginBatch,
) where

import Data.Aeson (Value, eitherDecode', encode)
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Lazy.Char8 as BSL8
import Data.Text (Text)
import qualified Data.Text as T
import System.Exit (ExitCode (..))
import System.Process (readProcessWithExitCode)

{- | Call an external plugin with a single JSON request.
Spawns the plugin executable, sends JSON on stdin, reads JSON from stdout.
-}
callPlugin :: FilePath -> Value -> IO (Either Text Value)
callPlugin pluginPath request = do
    let input = BSL8.unpack (encode request)
    result <- readProcessWithExitCode pluginPath [] input
    case result of
        (ExitSuccess, stdout, _stderr) ->
            case eitherDecode' (BSL8.pack stdout) of
                Right val -> pure (Right val)
                Left err -> pure (Left $ "Plugin JSON parse error: " <> T.pack err)
        (ExitFailure code, _stdout, stderr) ->
            pure $
                Left $
                    "Plugin "
                        <> T.pack pluginPath
                        <> " exited with code "
                        <> T.pack (show code)
                        <> ": "
                        <> T.pack stderr

{- | Call an external plugin with a batch of JSON requests (JSON Lines).
Sends all requests as newline-delimited JSON, reads newline-delimited responses.
-}
callPluginBatch :: FilePath -> [Value] -> IO (Either Text [Value])
callPluginBatch pluginPath requests = do
    let input = BSL8.unpack $ BSL.intercalate "\n" (map encode requests)
    result <- readProcessWithExitCode pluginPath [] input
    case result of
        (ExitSuccess, stdout, _stderr) ->
            let lines' = filter (not . BSL.null) $ BSL8.split '\n' (BSL8.pack stdout)
             in case parseLines lines' of
                    Right vals -> pure (Right vals)
                    Left err -> pure (Left $ "Plugin JSON parse error: " <> T.pack err)
        (ExitFailure code, _stdout, stderr) ->
            pure $
                Left $
                    "Plugin "
                        <> T.pack pluginPath
                        <> " exited with code "
                        <> T.pack (show code)
                        <> ": "
                        <> T.pack stderr

-- | traverse for lists using mapM from Prelude
parseLines :: [BSL.ByteString] -> Either String [Value]
parseLines = mapM eitherDecode'
