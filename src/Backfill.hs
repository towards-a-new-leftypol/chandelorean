{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant bracket" #-}
{-# HLINT ignore "Use fromMaybe" #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import System.Exit
import Data.Int (Int64)
import Control.Monad (filterM)
import Data.Aeson (decode)
import qualified Data.ByteString.Lazy as B
import System.Console.CmdArgs
import System.Directory (listDirectory, doesFileExist)
import System.FilePath ((</>))
import Data.List (find)
import qualified Data.Set as Set
import Data.Set (Set)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Data.Time.Clock (UTCTime)
import qualified Data.Map as Map
import Data.Maybe (fromJust)
import Data.Text (Text, unpack)
import Data.Text.Encoding (decodeUtf8)
import Network.Mime (defaultMimeLookup, defaultMimeType)

import JSONParsing
import JSONSettings
import qualified JSONCommonTypes as JS
import qualified JSONPost   as JSONPosts
import qualified DataClient as Client
import qualified SitesType  as Sites
import qualified BoardsType as Boards
import qualified ThreadType as Threads
import qualified AttachmentType as At
import qualified Common.PostsType as Posts
import qualified Hash as Hash

newtype SettingsCLI = SettingsCLI
  { jsonFile :: FilePath
  } deriving (Show, Data, Typeable)


listCatalogDirectories :: JSONSettings -> IO [ FilePath ]
listCatalogDirectories settings = do
    allDirs <- listDirectory (backup_read_root settings)
    let filteredDirs = filter (`notElem` excludedDirs) allDirs
    filterM hasCatalog filteredDirs

  where
    excludedDirs = ["sfw", "alt", "overboard"]

    hasCatalog dir = do
      let catalogPath = backup_read_root settings </> dir </> "catalog.json"
      doesFileExist catalogPath


ensureSiteExists :: JSONSettings -> IO Int
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
                        putStrLn "Did not get new site id back from postgrest"
                        exitFailure
                    Left err -> do
                        putStrLn $ "Failed to create " ++ site_name settings
                            ++ " Error: " ++ show err
                        exitFailure

        Left err -> do
            putStrLn $ "Error fetching sites: " ++ show err
            exitFailure


createArchivesForNewBoards
    :: JSONSettings ->
    Set String ->
    [ String ] ->
    Int ->
    IO [ Boards.Board ]
createArchivesForNewBoards settings dirsSet archived_boards siteid = do
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
            mapM_ (putStrLn . Boards.pathpart) boards
            return boards


apiThreadToArchiveThread :: Int -> Thread -> Threads.Thread
apiThreadToArchiveThread board_id_ json_thread =
    Threads.Thread
    { Threads.thread_id       = undefined
    , Threads.board_thread_id = no json_thread
    , Threads.creation_time   = epochToUTCTime $ fromIntegral (time json_thread)
    , Threads.board_id        = board_id_
    }

epochToUTCTime :: Int -> UTCTime
epochToUTCTime = posixSecondsToUTCTime . realToFrac


createArchivesForNewThreads
    :: JSONSettings
    -> [ Thread ]
    -> [ Threads.Thread ]
    -> Boards.Board
    -> IO [ Threads.Thread ]
createArchivesForNewThreads settings all_threads archived_threads board = do
    putStrLn $ "Creating " ++ show (length threads_to_create) ++ " threads."
    threads_result <- Client.postThreads settings (map (apiThreadToArchiveThread board_id) threads_to_create)

    case threads_result of
        Left err -> do
            putStrLn $ "Error creating threads: " ++ show err
            exitFailure
        Right new_threads -> return new_threads

    where
        board_id :: Int = Boards.board_id board

        archived_board_thread_ids :: Set.Set Int
        archived_board_thread_ids =
            Set.fromList $ map Threads.board_thread_id archived_threads

        threads_to_create :: [ Thread ]
        threads_to_create =
            filter
                ((`Set.notMember` archived_board_thread_ids) . no)
                all_threads


ensureThreads :: JSONSettings -> Boards.Board -> [ Thread ] -> IO [ Threads.Thread ]
ensureThreads settings board all_threads = do
    threads_result <- Client.getThreads settings (Boards.board_id board) (map no all_threads)

    case threads_result of
        Left err -> do
            putStrLn $ "Error fetching threads: " ++ show err
            exitFailure
        Right archived_threads -> do
            putStrLn $ show (length archived_threads) ++ " threads already exist."
            new_threads <- createArchivesForNewThreads settings all_threads archived_threads board
            return $ archived_threads ++ new_threads


readPosts
    :: JSONSettings
    -> Boards.Board
    -> Threads.Thread
    -> IO (Threads.Thread, [ JSONPosts.Post ])
readPosts settings board thread = do
    result <- parsePosts thread_filename

    case result of
        Left err -> do
            putStrLn $ "Failed to parse the JSON file " ++ thread_filename ++ " error: " ++ err
            return (thread, [])
        Right posts_wrapper -> return (thread, JSONPosts.posts posts_wrapper)

    where
        thread_filename :: FilePath
        thread_filename = backupDir </> "res" </> (show (Threads.board_thread_id thread) ++ ".json")

        backupDir :: FilePath
        backupDir = backup_read_root settings </> Boards.pathpart board


-- Convert Post to DbPost
apiPostToArchivePost :: Threads.Thread -> JSONPosts.Post -> Posts.Post
apiPostToArchivePost thread post =
    Posts.Post
    { Posts.post_id         = Nothing
    , Posts.board_post_id   = JSONPosts.no post
    , Posts.creation_time   = posixSecondsToUTCTime (realToFrac $ JSONPosts.time post)
    , Posts.body            = JSONPosts.com post
    , Posts.name            = JSONPosts.name post
    , Posts.subject         = JSONPosts.sub post
    , Posts.email           = JSONPosts.email post
    , Posts.thread_id       = Threads.thread_id thread
    }

-- | A version of 'concatMap' that works with a monadic predicate.
-- Stolen from package extra Control.Monad.Extra
concatMapM :: Monad m => (a -> m [b]) -> [a] -> m [b]
{-# INLINE concatMapM #-}
concatMapM op = foldr f (pure [])
    where f x xs = do
            x_ <- op x

            if null x_
            then xs
            else do
                xs_ <- xs
                pure $ x_ ++ xs_


setPostIdInPosts :: [(JSONPosts.Post, Posts.Post)] -> [ Client.PostId ] -> [(JSONPosts.Post, Posts.Post)]
setPostIdInPosts post_pairs ids = map f ids
    where
        post_map :: Map.Map (Int64, Int64) (JSONPosts.Post, Posts.Post)
        post_map = Map.fromList (map (\(i, j) -> ((Posts.thread_id j, Posts.board_post_id j), (i, j))) post_pairs)

        f :: Client.PostId -> (JSONPosts.Post, Posts.Post)
        f (Client.PostId { Client.post_id = asdf1, Client.thread_id = asdf2, Client.board_post_id = asdf3 }) =
            (\(i, j) -> (i, j { Posts.post_id = Just asdf1 })) (post_map Map.! (asdf2, asdf3))


fileToAttachment :: Posts.Post -> JS.File -> At.Attachment
fileToAttachment post file =
    At.Attachment
        { At.attachment_id = Nothing
        , At.mimetype = maybe "undefined/undefined" id (JS.mime file)
        , At.creation_time = Posts.creation_time post
        , At.sha256_hash = undefined
        , At.phash = undefined
        , At.illegal = False
        , At.post_id = fromJust $ Posts.post_id post
        , At.resolution = dim
        }

    where
      dim = (JS.w file) >>= \w ->
        ((JS.h file) >>= \h ->
          Just $ At.Dimension w h)


getMimeType :: Text -> Text
getMimeType ext = decodeUtf8 $ defaultMimeLookup ext


processFiles :: JSONSettings -> [(JSONPosts.Post, Posts.Post)] -> IO ()
processFiles settings post_pairs = do -- perfect just means that our posts have ids, they're already inserted into the db
    existing_attachments_result <- Client.getAttachments settings (map (fromJust . Posts.post_id . snd) post_pairs)

    case existing_attachments_result of
        Left err -> do
            putStrLn $ "Error fetching boards: " ++ show err
            exitFailure
        Right existing_attachments -> do
            let map_existing :: Map.Map Int64 [ At.Attachment ] =
                    foldl (insertRecord At.post_id) Map.empty existing_attachments

            -- have things like sha256 already
            -- how do we know that a `elem` attachments_on_board and a `elem` existing_attachments
                    -- can group existing_attachments into `map_existing :: Map post_id -> Set Attachment`
                    -- same with attachments_on_board into `map_should_exist :: Map post_id -> Set Attachment`
                    --
                    -- then run through the keys and compare size of the sets
            let attachments_on_board :: [(At.Paths, At.Attachment)] =
                    concatMap parseAttachments post_pairs

            let map_should_exist :: Map.Map Int64 [(At.Paths, At.Attachment)] =
                    foldl (insertRecord (At.post_id . snd)) Map.empty attachments_on_board

            let to_insert_map = Map.filterWithKey (compareAttMaps map_existing) map_should_exist

            let to_insert = foldr (++) [] $ Map.elems to_insert_map

            have_hash <- mapM computeAttachmentHash to_insert

            let existing_hashes :: Set.Set Text =
                    Set.fromList $ map At.sha256_hash existing_attachments

            let to_insert_ = filter ((`Set.notMember` existing_hashes) . At.sha256_hash) have_hash

            -- TODO: Concat all values in to_insert_map and
            -- go ahead and compute sha256 and phashes on them.
            -- ensure that the sha256 in to_insert_map is not in map_existing

            return ()

    where
        computeAttachmentHash :: (At.Paths, At.Attachment) -> IO At.Attachment
        computeAttachmentHash (p, q) = do
            let f = backup_read_root settings <> "/" <> unpack (At.file_path p)
            sha256_sum <- Hash.computeSHA256 f

            return q { At.sha256_hash = sha256_sum }

        computeAttachmentPhash :: (At.Paths, At.Attachment) -> IO At.Attachment
        computeAttachmentPhash = undefined

        parseLegacyPaths :: JSONPosts.Post -> Maybe At.Paths
        parseLegacyPaths post = do
            tim <- JSONPosts.tim post

            ext <- JSONPosts.ext post

            let board = JSONPosts.board post

            let file_path = board <> "/src/" <> tim <> ext
            let thumbnail_path = board <> "/thumb/" <> tim <> ext

            Just $ At.Paths file_path thumbnail_path


        parseAttachments :: (JSONPosts.Post, Posts.Post) -> [(At.Paths, At.Attachment)]
        parseAttachments (p, q) =
            case JSONPosts.files p of
                Just files -> map (\x -> (At.Paths (JS.file_path x) (JS.thumb_path x), fileToAttachment q x)) files
                Nothing ->
                    case parseLegacyPaths p of
                        Nothing -> []
                        Just paths ->
                            let
                                dim = (JSONPosts.w p) >>= \w -> ((JSONPosts.h p) >>= \h -> Just $ At.Dimension w h)
                                mime = maybe (decodeUtf8 defaultMimeType) getMimeType $ JSONPosts.ext p
                            in
                                ( paths
                                , At.Attachment
                                    { At.attachment_id = Nothing
                                    , At.mimetype = mime
                                    , At.creation_time = Posts.creation_time q
                                    , At.sha256_hash = undefined
                                    , At.phash = undefined
                                    , At.illegal = False
                                    , At.post_id = fromJust $ Posts.post_id q
                                    , At.resolution = dim
                                    }
                                ) : []

        insertRecord
            :: Ord a
            => (b -> a)
            -> Map.Map a [b]
            -> b
            -> Map.Map a [b]
        insertRecord getKey accMap x =
            let pid = getKey x
                l = Map.findWithDefault [] pid accMap
            in Map.insert pid (x : l) accMap

        compareAttMaps
            :: Map.Map Int64 [ At.Attachment ]
            -> Int64
            -> [(At.Paths, At.Attachment)]
            -> Bool
        compareAttMaps existing k v
            = (maybe (-1) length (Map.lookup k existing)) /= length v

processBoard :: JSONSettings -> Boards.Board -> IO ()
processBoard settings board = do
    let catalogPath = backupDir </> "catalog.json"
    putStrLn $ "catalog file path: " ++ catalogPath

    result <- parseJSONCatalog catalogPath

    case result of
        Right catalogs -> do
            let threads_on_board = concatMap ((maybe [] id) . threads) catalogs

            all_threads_for_board :: [ Threads.Thread ] <- ensureThreads settings board threads_on_board

            all_posts_on_board :: [(Threads.Thread, [ JSONPosts.Post ])] <- mapM (readPosts settings board) all_threads_for_board

            let post_pairs :: [ (JSONPosts.Post, Posts.Post) ] = concatMap
                    ( \(t, posts) -> map (\p -> (p, apiPostToArchivePost t p)) posts )
                    all_posts_on_board

            posts_result <- Client.postPosts settings (map snd post_pairs)

            case posts_result of
                Left err -> print err
                Right (new_ids :: [ Client.PostId ]) -> do
                    putStrLn "Sum of post_ids:"
                    print $ sum $ map Client.post_id new_ids
                    putStrLn "Sum of board_post_ids:"
                    print $ sum $ map Client.board_post_id new_ids

                    let perfect_post_pairs = setPostIdInPosts post_pairs new_ids

                    processFiles settings perfect_post_pairs

        Left errMsg    ->
            putStrLn $ "Failed to parse the JSON file in directory: "
                ++ (Boards.pathpart board) ++ ". Error: " ++ errMsg

    where
        backupDir :: FilePath
        backupDir = backup_read_root settings </> (Boards.pathpart board)


processBackupDirectory :: JSONSettings -> IO ()
processBackupDirectory settings = do
    putStrLn "JSON successfully read!"
    print settings  -- print the decoded JSON settings
    site_id_ <- ensureSiteExists settings
    dirs <- listCatalogDirectories settings
    let dirsSet = Set.fromList dirs
    boards_result <- Client.getSiteBoards settings site_id_
    putStrLn "Boards fetched!"

    case boards_result of
        Left err -> do
            putStrLn $ "Error fetching boards: " ++ show err
            exitFailure
        Right archived_boards -> do
            let boardnames = map Boards.pathpart archived_boards
            created_boards <- createArchivesForNewBoards settings dirsSet boardnames site_id_
            let boards :: [ Boards.Board ] = archived_boards ++ created_boards
            let boards_we_have_data_for = filter (\board -> Set.member (Boards.pathpart board) dirsSet) boards
            mapM_ (processBoard settings) boards_we_have_data_for


-- TODO: detect saged threads by reading the bump time from the thread and comparing
--  that time to the timestamp of the most recent post. If the post is newer
--      - then the thread is being saged. Reasons it can be saged:
--          - it's saged by a mod
--          - the post has sage in the email field
--          - the thread is full.


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
            Just settings -> processBackupDirectory settings
