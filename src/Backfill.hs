-- {-# LANGUAGE DeriveDataTypeable #-}
-- {-# LANGUAGE DeriveGeneric #-}
-- {-# LANGUAGE OverloadedStrings #-}

module Main where

import System.Exit
import Control.Monad (filterM)
import Data.Aeson (decode)
import qualified Data.ByteString.Lazy as B
import System.Console.CmdArgs
import System.Directory (listDirectory, doesFileExist)
import System.FilePath ((</>))

import JSONParsing
import Types
import qualified DataClient as Client

data SettingsCLI = SettingsCLI
  { jsonFile :: FilePath
  } deriving (Show, Data, Typeable)

settingsCLI :: SettingsCLI
settingsCLI = SettingsCLI
    { jsonFile = def &= args &= typ "settings-jsonfile-path"
    } &= summary "Backfill v0.0.1"


listCatalogDirectories :: JSONSettings -> IO [FilePath]
listCatalogDirectories settings = do
    dirs <- listDirectory (backup_read_root settings)
    filterM hasCatalog dirs
  where
    hasCatalog dir = do
      let catalogPath = (backup_read_root settings) </> dir </> "catalog.json"
      doesFileExist catalogPath


ensureSiteExists :: JSONSettings -> IO ()
ensureSiteExists settings = do
    sitesResult <- Client.getAllSites settings

    case sitesResult of
        Right siteList ->
            if any (\site -> Client.name site == site_name settings) siteList
                then putStrLn $ site_name settings ++ " already exists!"
                else do
                    putStrLn "leftychan.net does not exist. Creating..."
                    postResult <- Client.postSite settings
                    case postResult of
                        Right _ -> putStrLn $ "Successfully created " ++ site_name settings ++ "."
                        Left err -> do
                            putStrLn $ "Failed to create leftychan.net. Error: " ++ show err
                            exitFailure
        Left err -> do
            putStrLn $ "Error fetching sites: " ++ show err
            exitFailure


processBackupDirectory :: JSONSettings -> IO ()
processBackupDirectory settings = do
    putStrLn "JSON successfully read!"
    print settings  -- print the decoded JSON settings
    ensureSiteExists settings
    dirs <- listCatalogDirectories settings
    _ <- Client.getWebsiteBoards settings
    putStrLn "Boards fetched!"
    mapM_ putStrLn dirs
    mapM_ processDir dirs
  where
    backupDir :: FilePath
    backupDir = backup_read_root settings

    processDir dir = do
        let catalogPath = backupDir </> dir </> "catalog.json"
        putStrLn $ "catalog file path: " ++ catalogPath

        result <- parseJSONFile catalogPath

        case result of
            Right catalogs ->
                mapM_ (mapM_ (print . no) . threads) catalogs
            Left errMsg    ->
                putStrLn $ "Failed to parse the JSON file in directory: "
                    ++ dir ++ ". Error: " ++ errMsg

main :: IO ()
main = do
    settingsValue <- cmdArgs settingsCLI
    let filePath = jsonFile settingsValue
    if null filePath
    then do
        putStrLn "Error: No JSON settings file provided."
        exitFailure
    else do
        putStrLn $ "Loading settings from: " ++ filePath
        content <- B.readFile filePath
        case decode content :: Maybe JSONSettings of
            Nothing -> do
                putStrLn "Error: Invalid JSON format."
                exitFailure
            Just settings -> processBackupDirectory settings
