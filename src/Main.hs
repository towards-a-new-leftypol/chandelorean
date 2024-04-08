{-# LANGUAGE RecordWildCards #-}

module Main (main) where

import System.Exit (exitFailure)
import qualified Data.ByteString.Lazy as B
import System.Console.CmdArgs (cmdArgs, Data, Typeable)
import Data.Aeson (decode)
import System.FilePath ((</>))
import Control.Concurrent.Async (mapConcurrently)
import Data.Aeson (FromJSON)

import qualified SitesType as Sites
import Common.Server.ConsumerSettings
import Common.Server.JSONSettings as J
import Lib
    ( processBoards
    , FileGetters (..)
    )
import qualified Network.DataClient as Client

newtype CliArgs = CliArgs
  { settingsFile :: String
  } deriving (Show, Data, Typeable)

toClientSettings :: ConsumerJSONSettings -> JSONSiteSettings -> J.JSONSettings
toClientSettings ConsumerJSONSettings {..} JSONSiteSettings {..} =
    J.JSONSettings
    { J.postgrest_url = postgrest_url
    , J.jwt = jwt
    , J.backup_read_root = undefined
    , J.media_root_path = media_root_path
    , J.site_name = name
    , J.site_url = root_url
    }

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

httpFileGetters :: JSONSettings -> FileGetters
httpFileGetters _ = FileGetters
    { getJSONCatalog = httpGetJSON
    , getJSONPosts = httpGetJSON
    }

httpGetJSON :: (FromJSON a) => Sites.Site -> String -> IO (Either String a)
httpGetJSON site path = (Client.getJSON $ Sites.url site </> path)
    >>= getErrMsg
    where
        getErrMsg :: Either Client.HttpError a -> IO (Either String a)
        getErrMsg (Left err) = return $ Left $ show err
        getErrMsg (Right x) = return $ Right x

processWebsite :: ConsumerJSONSettings -> JSONSiteSettings -> IO ()
processWebsite settings site_settings = do
    let client_settings = toClientSettings settings site_settings
    processBoards client_settings (httpFileGetters client_settings) (boards site_settings)
    return ()

main :: IO ()
main = do
    putStrLn "Starting channel web synchronization."

    settings <- getSettings
    print settings

    _ <- mapConcurrently (processWebsite settings) (websites settings)

    putStrLn "Done."
