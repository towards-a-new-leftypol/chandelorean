{-# LANGUAGE RecordWildCards #-}

module Main (main) where

import System.Exit (exitFailure)
import qualified Data.ByteString.Lazy as B
import System.Console.CmdArgs (cmdArgs, Data, Typeable)
import Data.Aeson (decode)
import System.FilePath ((</>))
import Control.Concurrent.Async (mapConcurrently)
import Data.Aeson (FromJSON)
import System.Directory (createDirectoryIfMissing, removeFile)

import qualified SitesType as Sites
import Common.Server.ConsumerSettings
import Common.Server.JSONSettings as J
import Lib
    ( processBoards
    , FileGetters (..)
    )
import qualified Network.DataClient as Client
import qualified Common.AttachmentType as At

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


-- Move a file by reading, writing, and then deleting the original
moveFile :: FilePath -> FilePath -> IO ()
moveFile src dst =
    B.readFile src >>= B.writeFile dst >> removeFile src


httpFileGetters :: JSONSettings -> FileGetters
httpFileGetters settings = FileGetters
    { getJSONCatalog = httpGetJSON
    , getJSONPosts = httpGetJSON
    , addPathPrefix = ((++) $ site_url settings)
      -- attachmentPaths here actually doesn't get the paths of the attachment,
      -- it downloads them into a temporary file and gets that path of that.
    , attachmentPaths = \paths -> do
        filepath <- Client.getFile (At.file_path paths)
        m_thumbpath <- case At.thumbnail_path paths of
            Nothing -> return Nothing
            Just thumbpath -> Client.getFile thumbpath

        return $ filepath >>= \fp ->
            case m_thumbpath of
                Nothing -> return (At.Paths fp Nothing)
                tp -> return (At.Paths fp tp)

    , copyOrMove = \common_dest (src, dest) (m_thumb_src, thumb_dest) -> do
        putStrLn $ "Copy Or Move (Move) src: " ++ src ++ " dest: " ++ dest
        createDirectoryIfMissing True common_dest
        moveFile src dest

        case m_thumb_src of
          Nothing -> return ()
          Just thumb_src -> moveFile thumb_src thumb_dest
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
