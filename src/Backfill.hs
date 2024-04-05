module Main where

import System.Exit
import qualified Data.ByteString.Lazy as B
import Data.Aeson (decode)
import System.Console.CmdArgs

import Common.Server.JSONSettings
import Lib

-- TODO: detect saged threads by reading the bump time from the thread and comparing
--  that time to the timestamp of the most recent post. If the post is newer
--      - then the thread is being saged. Reasons it can be saged:
--          - it's saged by a mod
--          - the post has sage in the email field
--          - the thread is full.
--
-- Better to support all those flags via the api: saged, locked, cyclical?, sticky
--      - deleted could be there too
--      - edited could be there too

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
