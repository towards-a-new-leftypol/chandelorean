{-# LANGUAGE RecordWildCards #-}

module Main (main) where

import System.Exit (exitFailure)
import qualified Data.ByteString.Lazy as B
import System.Console.CmdArgs (cmdArgs, Data, Typeable)
import Data.Aeson (decode)
import Control.Concurrent.Async (mapConcurrently)

import Common.Server.ConsumerSettings
import Lib
    ( processBoards
    , toClientSettings
    , httpFileGetters
    )
import Sync

newtype CliArgs = CliArgs
  { settingsFile :: String
  } deriving (Show, Data, Typeable)

getSettings :: IO ConsumerJSONSettings
getSettings = do
    cliArgs <- cmdArgs $ CliArgs "consumer_settings.json"

    let filePath = settingsFile cliArgs
    if null filePath
    then do
        putStrLn "Error: No JSON settings file provided."
        exitFailure
    else do
        putStrLn $ "Loading settings from: " ++ filePath
        content <- B.readFile filePath
        case decode content :: Maybe ConsumerJSONSettings of
            Nothing -> do
                putStrLn "Error: Invalid JSON format."
                exitFailure
            Just settings -> return settings


processWebsite :: ConsumerJSONSettings -> JSONSiteSettings -> IO ()
processWebsite settings site_settings = do
    let client_settings = toClientSettings settings site_settings
    processBoards client_settings (httpFileGetters client_settings) (boards site_settings)
    return ()

main :: IO ()
main = do
    settings <- getSettings
    print settings

    _ <- if http_fill_all settings
    then do
        putStrLn "Starting web backfill"
        mapConcurrently (processWebsite settings) (websites settings)
    else return []

    if http_sync_continously settings
    then syncWebsites settings
    else return ()

    putStrLn "Done"

