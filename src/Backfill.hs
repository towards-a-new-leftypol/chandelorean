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
import Data.List (find)
import qualified Data.Set as Set

import JSONParsing
import Types
import qualified DataClient as Client
import qualified SitesType as Sites
import qualified BoardsType as Boards

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


ensureSiteExists :: JSONSettings -> IO (Int)
ensureSiteExists settings = do
    sitesResult <- Client.getAllSites settings

    case sitesResult of
        Right siteList ->
            case find (\site -> Sites.name site == site_name settings) siteList of
            Just site -> do
                putStrLn $ site_name settings ++ " already exists!"
                return $ Sites.site_id site
            Nothing -> do
                putStrLn $ site_name settings ++ " does not exist. Creating..."
                postResult <- Client.postSite settings

                case postResult of
                    Right (site:_) -> do
                        putStrLn $ "Successfully created " ++ site_name settings ++ ". " ++ show site
                        return $ Sites.site_id site
                    Right [] -> do
                        putStrLn $ "Did not get new site id back from postgrest"
                        exitFailure
                    Left err -> do
                        putStrLn $ "Failed to create " ++ site_name settings
                            ++ " Error: " ++ show err
                        exitFailure

        Left err -> do
            putStrLn $ "Error fetching sites: " ++ show err
            exitFailure


createArchivesForNewBoards :: JSONSettings -> [ String ] -> [ String ] -> Int -> IO [ Boards.Board ]
createArchivesForNewBoards settings dirs archived_boards siteid = do
    let dirsSet = Set.fromList dirs
    let archivedBoardsSet = Set.fromList archived_boards

    -- Find boards that are in dirs but not in archived_boards
    let boardsToArchive = dirsSet `Set.difference` archivedBoardsSet

    putStrLn "Creating boards:"
    mapM_ putStrLn boardsToArchive

    post_result <- Client.postBoards settings (Set.toList boardsToArchive) siteid

    case post_result of
        Left err -> do
            putStrLn $ "Error posting boards: " ++ show err
            exitFailure
        Right boards -> do
            putStrLn "Created the following boards:"
            mapM_ putStrLn (map Boards.pathpart boards)
            return boards

processBoard :: JSONSettings -> Boards.Board -> IO ()
processBoard settings board = do
    let catalogPath = backupDir </> (Boards.pathpart board) </> "catalog.json"
    putStrLn $ "catalog file path: " ++ catalogPath

    result <- parseJSONFile catalogPath

    case result of
        Right catalogs -> do
            let threads_on_board = concatMap threads catalogs
            -- catalogs can be turned into [ Thread ]
            -- ensureThreads :: ( Board, [ Thread ] ) -> IO ()
            mapM_ (print . no) threads_on_board
        Left errMsg    ->
            putStrLn $ "Failed to parse the JSON file in directory: "
                ++ (Boards.pathpart board) ++ ". Error: " ++ errMsg

  where
    backupDir :: FilePath
    backupDir = backup_read_root settings


processBackupDirectory :: JSONSettings -> IO ()
processBackupDirectory settings = do
    putStrLn "JSON successfully read!"
    print settings  -- print the decoded JSON settings
    site_id_ <- ensureSiteExists settings
    dirs <- listCatalogDirectories settings
    boards_result <- Client.getSiteBoards settings site_id_
    putStrLn "Boards fetched!"

    case boards_result of
        Left err -> do
            putStrLn $ "Error fetching boards: " ++ show err
            exitFailure
        Right archived_boards -> do
            let boardnames = map Boards.pathpart archived_boards
            created_boards <- createArchivesForNewBoards settings dirs boardnames site_id_
            let boards :: [ Boards.Board ] = archived_boards ++ created_boards
            mapM_ (processBoard settings) boards

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
