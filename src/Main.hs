module Main (main) where

import System.IO

import CliSettings
import Sync

main :: IO ()
main = do
    hSetBuffering stdout LineBuffering
    hSetBuffering stderr LineBuffering

    settings <- getSettings
    print settings

    putStrLn "Starting web sync loop"
    syncWebsites settings

    putStrLn "Done. Quitting."

