{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import System.Exit
import Data.Aeson (FromJSON, decode)
import qualified Data.ByteString.Lazy as B
import System.Console.CmdArgs
import GHC.Generics
import System.Directory (listDirectory)

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


-- Function to list all files and directories inside the backup_read_root
listBackupContents :: JSONSettings -> IO ()
listBackupContents settings =
   listDirectory (backup_read_root settings) >>= mapM_ print

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
            Just settings -> do
                putStrLn "JSON successfully read!"
                print settings  -- print the decoded JSON settings
                listBackupContents settings
