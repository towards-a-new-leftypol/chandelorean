module Main (main) where

import System.Exit (exitFailure)
import qualified Data.ByteString.Lazy as B
import System.Console.CmdArgs (cmdArgs, Data, Typeable)
import Data.Aeson (decode)
import Control.Concurrent.Async (mapConcurrently_)
import Control.Monad (when)

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

    when (http_fill_all settings) $ do
        putStrLn "Starting web backfill"
        mapConcurrently_ (processWebsite settings) (websites settings)
        putStrLn "Finished web backfill"

    when (http_sync_continously settings) $ do
        putStrLn "Starting web sync loop"
        syncWebsites settings

    putStrLn "Done. Quitting."

