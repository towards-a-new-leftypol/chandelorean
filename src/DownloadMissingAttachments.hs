module Main where

import System.FilePath ((</>), (<.>), takeDirectory)
import Data.Text (unpack)
import System.Directory (doesFileExist, createDirectoryIfMissing)
import Control.Exception (try, SomeException)
import System.Exit (exitFailure)

import CliSettings
import Network.DataClient (getAllAttachmentsPaged, HttpError (..), getFile)
import Sync
    (consumerSettingsToPartialJSONSettings
    )
import qualified Common.Network.SiteType as Site
import qualified Common.Network.BoardType as Board
import qualified Common.Network.ThreadType as Thread
import qualified Common.Network.PostType as Post
import qualified Common.AttachmentType as Att
import qualified Common.Server.JSONSettings as Sett
import Lib (moveFile)

type Url = String

constructAttachmentResourceLocator :: Sett.JSONSettings -> Site.Site -> [(FilePath, Url)]
constructAttachmentResourceLocator settings site =
    [ ( fileDirRoot </> fileBasename
      , fileUrlRoot </> "src" </> fileBasename
      )
    , ( fileDirRoot </> thumbnailFilename
      , fileUrlRoot </> "thumb" </> thumbnailBasename
      )
    ]
    where
        board      = head $ Site.boards site
        thread     = head $ Board.threads board
        post       = head $ Thread.posts thread
        attachment = head $ Post.attachments post

        fileDirRoot :: FilePath
        fileDirRoot
            =   Sett.media_root_path settings
            </> unpack (Site.name site)
            </> unpack (Board.pathpart board)
            </> show (Thread.board_thread_id thread)

        fileBasename :: String
        fileBasename
            = unpack (Att.board_filename attachment)
            <.> (maybe "jpg" unpack $ Att.file_extension attachment)

        thumbnailBasename :: String
        thumbnailBasename
            = unpack (Att.board_filename attachment)
            <.> (maybe "png" unpack $ Att.thumb_extension attachment)

        thumbnailFilename = "thumbnail_" <> thumbnailBasename

        fileUrlRoot :: Url
        fileUrlRoot = unpack (Site.url site) </> unpack (Board.pathpart board)


printHttpError :: HttpError -> IO ()
printHttpError err = putStrLn $ "HTTP Error occurred: " ++ show err


processResource :: (FilePath, Url) -> IO ()
processResource (filepath, url) = do
    exists <- doesFileExist filepath
    if exists
        then putStrLn $ filepath ++ " OK"
        else do
            putStrLn $ filepath ++ " Missing"
            putStrLn $ "Downloading: " ++ url
            downloadResult <- getFile url
            case downloadResult of
                Left err -> printHttpError err
                Right tmpPath -> do
                    putStrLn $ "Downloaded to temp: " ++ tmpPath ++ ", moving to " ++ filepath
                    -- Ensure the destination directory exists before moving
                    createDirectoryIfMissing True (takeDirectory filepath)

                    -- Attempt to move the file, catching any exceptions
                    moveResult <- try (moveFile tmpPath filepath) :: IO (Either SomeException ())
                    case moveResult of
                        Left e -> do
                            putStrLn $ "FATAL: Failed to move file to archive destination: " ++ show e
                            exitFailure
                        Right () -> putStrLn $ "Successfully moved to " ++ filepath
--
-- | Generates a list of (limit, offset) pairs for pagination.
-- Example: paginate 10 25 -> [(10, 0), (10, 10), (5, 20)]
paginate :: Int -> Int -> [(Int, Int)]
paginate pageSize_ total
  | pageSize_ <= 0 = []
  | otherwise     = go 0
  where
    go offset
      | offset >= total = []
      | otherwise = 
          let limit = min pageSize_ (total - offset)
          in (limit, offset) : go (offset + pageSize_)


pageSize :: Int
pageSize = 100

attachmentsCount :: Int 
attachmentsCount = 20000

main :: IO ()
main = do
    settings <- getSettings
    print settings

    let json_settings = consumerSettingsToPartialJSONSettings settings
    putStrLn "Reading attachments from db..."

    (flip mapM_) (paginate pageSize attachmentsCount) $ \(limit, offset) -> do
        attachmentsResult <- getAllAttachmentsPaged json_settings limit offset

        case attachmentsResult of
            Left err -> printHttpError err
            Right sites ->
                let resourceLocators = concatMap (constructAttachmentResourceLocator json_settings) sites
                in
                    mapM_ processResource resourceLocators
