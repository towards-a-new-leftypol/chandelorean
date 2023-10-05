{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import System.Exit
import Control.Monad (filterM)
import Data.Aeson (FromJSON, decode)
import qualified Data.ByteString.Lazy as B
import System.Console.CmdArgs
import GHC.Generics
import System.Directory (listDirectory, doesFileExist)
import System.FilePath ((</>))

import JSONParsing

data SettingsCLI = SettingsCLI
  { jsonFile :: FilePath
  } deriving (Show, Data, Typeable)

data JSONSettings = JSONSettings
  { postgrest_url :: String
  , jwt :: String
  , backup_read_root :: FilePath
  } deriving (Show, Generic)

instance FromJSON JSONSettings

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


processBackupDirectory :: JSONSettings -> IO ()
processBackupDirectory settings = do
    putStrLn "JSON successfully read!"
    print settings  -- print the decoded JSON settings
    dirs <- listCatalogDirectories settings
    mapM_ print dirs
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
