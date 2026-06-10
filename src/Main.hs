module Main (main) where

import CliSettings
import Sync

main :: IO ()
main = do
    settings <- getSettings
    print settings

    putStrLn "Starting web sync loop"
    syncWebsites settings

    putStrLn "Done. Quitting."

