module Main where

import System.Exit
import qualified Data.ByteString.Lazy as B
import Data.Aeson (decode)
import System.Console.CmdArgs

import Common.Server.JSONSettings
import Lib

main :: IO ()
main = do
    settingsValue <- cmdArgs $ SettingsCLI "backfill_settings.json"
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
                processBackupDirectory settings
                putStrLn "Done!"
