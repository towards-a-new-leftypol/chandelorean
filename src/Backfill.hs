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
import System.Directory
    ( listDirectory
    , doesFileExist
    , copyFile
    , createDirectoryIfMissing
    )
import System.FilePath ((</>), (<.>), takeExtension)
import Data.List (find, isSuffixOf, foldl')
import qualified Data.Set as Set
import Data.Set (Set)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Data.Time.Clock (UTCTime)
import qualified Data.Map as Map
import Data.Maybe (fromJust)
import Data.Text (Text, unpack)
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8)
import Network.Mime (defaultMimeLookup)
import PerceptualHash (fileHash)

import JSONParsing
import JSONSettings
import qualified JSONCommonTypes as JS
import qualified JSONPost   as JSONPosts
import qualified DataClient as Client
import qualified SitesType  as Sites
import qualified BoardsType as Boards
import qualified ThreadType as Threads
import qualified Common.AttachmentType as At
import qualified Common.PostsType as Posts
import qualified Hash as Hash
import qualified Data.WordUtil as Words

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


ensureSiteExists :: JSONSettings -> IO Sites.Site
ensureSiteExists settings = do
    sitesResult <- Client.getAllSites settings

    case sitesResult of
        Right siteList ->
            case find (\site -> Sites.name site == site_name settings) siteList of
            Just site -> do
                putStrLn $ site_name settings ++ " already exists!"
                return site
            Nothing -> do
                putStrLn $ site_name settings ++ " does not exist. Creating..."
                postResult <- Client.postSite settings

                case postResult of
                    Right (site:_) -> do
                        putStrLn $ "Successfully created " ++ site_name settings ++ ". " ++ show site
                        return site
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


setPostIdInPosts
    :: [(Sites.Site, Boards.Board, Threads.Thread, JSONPosts.Post, Posts.Post)]
    -> [ Client.PostId ]
    -> [(Sites.Site, Boards.Board, Threads.Thread, JSONPosts.Post, Posts.Post)]
setPostIdInPosts tuples ids = map f ids
    where
        post_map :: Map.Map (Int64, Int64) (Sites.Site, Boards.Board, Threads.Thread, JSONPosts.Post, Posts.Post)
        post_map = Map.fromList (map (\(a, b, c, i, j) -> ((Posts.thread_id j, Posts.board_post_id j), (a, b, c, i, j))) tuples)

        f :: Client.PostId -> (Sites.Site, Boards.Board, Threads.Thread, JSONPosts.Post, Posts.Post)
        f (Client.PostId { Client.post_id = asdf1, Client.thread_id = asdf2, Client.board_post_id = asdf3 }) =
            (\(a, b, c, i, j) -> (a, b, c, i, j { Posts.post_id = Just asdf1 })) (post_map Map.! (asdf2, asdf3))


fileToAttachment :: Posts.Post -> JS.File -> At.Attachment
fileToAttachment post file =
    At.Attachment
        { At.mimetype = maybe guessed_mime id (JS.mime file)
        , At.creation_time = Posts.creation_time post
        , At.sha256_hash = undefined
        , At.phash = Nothing
        , At.illegal = False
        , At.post_id = fromJust $ Posts.post_id post
        , At.resolution = dim
        , At.file_extension = Just extension
        , At.thumb_extension = Just thumb_extension
        , At.original_filename = Just $ JS.filename file <> "." <> extension
        , At.file_size_bytes = JS.fsize file
        , At.board_filename = JS.id file
        , At.spoiler = maybe False id $ JS.spoiler file
        }

    where
      extension = JS.ext file

      thumb_extension = T.pack $ drop 1 $ takeExtension $ unpack $ JS.thumb_path file

      guessed_mime = getMimeType extension

      dim = (JS.w file) >>= \w ->
        ((JS.h file) >>= \h ->
          Just $ At.Dimension w h)


getMimeType :: Text -> Text
getMimeType ext = decodeUtf8 $ defaultMimeLookup ext


phash_mimetypes :: Set.Set Text
phash_mimetypes = Set.fromList
    [ "image/jpeg"
    , "image/png"
    , "image/gif"
    ]


copyFiles :: JSONSettings -> (Sites.Site, Boards.Board, Threads.Thread, Posts.Post, At.Paths, At.Attachment) -> IO ()
copyFiles settings (site, board, thread, _, path, attachment) = do
    destination_exists <- doesFileExist dest

    if not destination_exists
    then do
        src_exists <- doesFileExist src

        createDirectoryIfMissing True common_dest

        if src_exists
        then putStrLn ("Copying " ++ src) >> copyFile src dest
        else return ()

        thumb_exists <- doesFileExist thumb_src

        if thumb_exists
        then putStrLn ("Copying " ++ thumb_src) >> copyFile thumb_src thumb_dest
        else return ()

    else return ()

    -- src = (At.file_path | At.thumb_path)
    -- dest = <media_root>/<website_name>/<boardpart>/<board_thread_id>/<sha>.<ext>

    where
        src :: FilePath
        src = At.file_path path

        thumb_src :: FilePath
        thumb_src = At.thumbnail_path path

        dest :: FilePath
        dest = common_dest
          </> (unpack $ At.board_filename attachment)
          <.> (unpack $ fromJust $ At.file_extension attachment)

        thumb_dest :: FilePath
        thumb_dest = common_dest
            </> "thumbnail_" <> (unpack $ At.board_filename attachment)
            <.> (unpack $ fromJust $ At.thumb_extension attachment)

        common_dest :: FilePath
        common_dest
            = (JSONSettings.media_root_path settings)
            </> Sites.name site
            </> Boards.pathpart board
            </> (show $ Threads.board_thread_id thread)


processFiles :: JSONSettings -> [(Sites.Site, Boards.Board, Threads.Thread, JSONPosts.Post, Posts.Post)] -> IO ()
processFiles settings tuples = do -- perfect just means that our posts have ids, they're already inserted into the db
    let ps = map (\(_, _, _, _, x) -> x) tuples

    existing_attachments_result <- Client.getAttachments settings (map (fromJust . Posts.post_id) ps)

    case existing_attachments_result of
        Left err -> do
            putStrLn $ "Error fetching attachments: " ++ show err
            exitFailure
        Right existing_attachments -> do
            let map_existing :: Map.Map (Int64, Text) [ At.Attachment ] =
                    foldl'
                        (insertRecord (\a -> (At.post_id a, At.board_filename a)))
                        Map.empty
                        existing_attachments

            let attachments_on_board :: [(Sites.Site, Boards.Board, Threads.Thread, Posts.Post, At.Paths, At.Attachment)] =
                    concatMap parseAttachments tuples
            -- attachments_on_board are the only files that can be copied into the archive dir right now
            -- since that's where we have the src filename. except here the Attachment doesn't have a sha hash yet
            -- so we can't build the destination filename.

            let map_should_exist :: Map.Map (Int64, Text) [(Sites.Site, Boards.Board, Threads.Thread, Posts.Post, At.Paths, At.Attachment)] =
                    foldl'
                        (insertRecord (\(_, _, _, _, _, a) -> (At.post_id a, At.board_filename a)))
                        Map.empty
                        attachments_on_board

            let to_insert_map =
                    Map.filterWithKey
                        (\k _ -> not $ k `Map.member` map_existing)
                        map_should_exist

            let to_insert = foldr (++) [] $ Map.elems to_insert_map

            to_insert_exist <- filterM attachmentFileExists to_insert

            with_hashes <- mapM computeAttachmentHash to_insert_exist

            attachments_result <- Client.postAttachments settings with_hashes

            case attachments_result of
                Left err -> do
                    putStrLn $ "Error posting attachments: " ++ show err
                    exitFailure

                Right saved -> do
                    putStrLn $ "Saved " ++ (show $ length saved) ++ " attachments!"
                    mapM_ (copyFiles settings) attachments_on_board

    where
        attachmentFileExists :: (Sites.Site, Boards.Board, Threads.Thread, Posts.Post, At.Paths, At.Attachment) -> IO Bool
        attachmentFileExists (_, _, _, _, p, _) = doesFileExist (At.file_path p)

        computeAttachmentHash :: (Sites.Site, Boards.Board, Threads.Thread, Posts.Post, At.Paths, At.Attachment) -> IO At.Attachment
        computeAttachmentHash (_, _, _, _, p, q) = do
            let f = At.file_path p

            putStrLn $ "Reading " ++ f
            -- putStrLn $ show p
            -- putStrLn $ show (q { At.sha256_hash = "undefined" })

            sha256_sum <- Hash.computeSHA256 f

            putStrLn $ "SHA-256: " ++ unpack sha256_sum

            phash :: Maybe Int64 <-
                case (At.mimetype q) `Set.member` phash_mimetypes of
                    True -> do
                        either_phash <- fileHash f
                        case either_phash of
                            Left err_str -> do
                                putStrLn $ "Failed to compute phash for file " ++ (unpack sha256_sum) ++ " " ++ f ++ " " ++ err_str
                                return Nothing
                            Right phash_w -> do
                                let phash_i = Words.wordToSignedInt64 phash_w

                                if phash_i == 0 then do
                                    putStrLn $ "phash is 0 for file " ++ (unpack sha256_sum) ++ " " ++ f
                                    return Nothing
                                else do
                                    putStrLn $ "phash: " ++ show phash_w
                                    return $ Just $ Words.wordToSignedInt64 phash_w

                    False -> return Nothing


            return q
                { At.sha256_hash = sha256_sum
                , At.phash = phash
                }

        parseLegacyPaths :: JSONPosts.Post -> Maybe (At.Paths, At.Attachment)
        parseLegacyPaths post = do
            tim <- JSONPosts.tim post
            ext <- JSONPosts.ext post
            filename <- JSONPosts.filename post
            size <- JSONPosts.fsize post
            spoiler <- JSONPosts.fsize post

            let
                board = JSONPosts.board post
                file_path = withPathPrefix $ board <> "/src/" <> tim <> ext
                thumbnail_path = withPathPrefix $ board <> "/thumb/" <> tim <> ext

                p = At.Paths file_path thumbnail_path

                mime = getMimeType ext

                attachment = At.Attachment
                    { At.mimetype = mime
                    , At.creation_time = undefined
                    , At.sha256_hash = undefined
                    , At.phash = Nothing
                    , At.illegal = False
                    , At.post_id = undefined
                    , At.resolution = undefined
                    , At.file_extension = Just $ T.drop 1 ext
                    , At.thumb_extension = Just $ "png"
                    , At.original_filename = Just $ filename <> ext
                    , At.file_size_bytes = size
                    , At.board_filename = tim
                    , At.spoiler = spoiler > 0
                    }

            return (p, attachment)


        notDeleted :: (a, b, c, d, At.Paths, At.Attachment) -> Bool
        notDeleted (_, _, _, _, p, _) = not $ "deleted" `isSuffixOf` (At.file_path p)


        withPathPrefix :: Text -> FilePath
        withPathPrefix = ((<>) $ backup_read_root settings) . unpack

        parseAttachments
            :: (Sites.Site, Boards.Board, Threads.Thread, JSONPosts.Post, Posts.Post)
            -> [(Sites.Site, Boards.Board, Threads.Thread, Posts.Post, At.Paths, At.Attachment)]
        parseAttachments (site, board, thread, p, q) = filter notDeleted $
            case JSONPosts.files p of
                Just files -> map (\x ->
                    ( site
                    , board
                    , thread
                    , q
                    , At.Paths (withPathPrefix $ JS.file_path x) (withPathPrefix $ JS.thumb_path x)
                    , fileToAttachment q x)
                    ) files
                Nothing ->
                    case parseLegacyPaths p of
                        Nothing -> []
                        Just (paths, a) ->
                            let
                                dim = (JSONPosts.w p) >>= \w -> ((JSONPosts.h p) >>= \h -> Just $ At.Dimension w h)
                            in
                                ( site
                                , board
                                , thread
                                , q
                                , paths
                                , a
                                    { At.creation_time = Posts.creation_time q
                                    , At.resolution = dim
                                    , At.post_id = fromJust $ Posts.post_id q
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


processBoard :: JSONSettings -> Sites.Site -> Boards.Board -> IO ()
processBoard settings site board = do
    let catalogPath = backupDir </> "catalog.json"
    putStrLn $ "catalog file path: " ++ catalogPath

    result <- parseJSONCatalog catalogPath

    case result of
        Right catalogs -> do
            let threads_on_board = concatMap ((maybe [] id) . threads) catalogs

            all_threads_for_board :: [ Threads.Thread ] <- ensureThreads settings board threads_on_board

            all_posts_on_board :: [(Threads.Thread, [ JSONPosts.Post ])] <- mapM (readPosts settings board) all_threads_for_board

            let tuples :: [(Sites.Site, Boards.Board, Threads.Thread, JSONPosts.Post, Posts.Post)] = concatMap
                    (\(t, posts) -> map (\p -> (site, board, t, p, apiPostToArchivePost t p)) posts)
                    all_posts_on_board

            posts_result <- Client.postPosts settings (map (\(_, _, _, _, d) -> d) tuples)

            case posts_result of
                Left err -> print err
                Right (new_ids :: [ Client.PostId ]) -> do
                    putStrLn "Sum of post_ids:"
                    print $ sum $ map Client.post_id new_ids
                    putStrLn "Sum of board_post_ids:"
                    print $ sum $ map Client.board_post_id new_ids

                    let perfect_post_pairs = setPostIdInPosts tuples new_ids

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
    site :: Sites.Site <- ensureSiteExists settings
    dirs <- listCatalogDirectories settings
    let dirsSet = Set.fromList dirs
    let site_id_ = Sites.site_id site
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
            mapM_ (processBoard settings site) boards_we_have_data_for


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
