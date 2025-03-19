{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant bracket" #-}
{-# HLINT ignore "Use fromMaybe" #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# HLINT ignore "Use when" #-}
{-# HLINT ignore "Use tuple-section" #-}
{-# HLINT ignore "Use zipWith" #-}
{-# HLINT ignore "Use <&>" #-}
{-# HLINT ignore "Use if" #-}
{-# HLINT ignore "Move brackets to avoid $" #-}
{-# HLINT ignore "Eta reduce" #-}

module Lib
    ( toClientSettings
    , createArchivesForNewBoards
    , ensureSiteExists
    , httpFileGetters
    , processFiles
    , processBoards
    , processBackupDirectory
    , SettingsCLI (..)
    , epochToUTCTime
    , apiThreadToArchiveThread
    , localIndexFoldf
    , addPostsToTuples
    , Details
    , parseAttachments
    , insertRecord
    , computeAttachmentHash
    , copyOrMoveFiles
    , moveAttachmentAndThumb
    , makeThreadAttachmentFsPath
    ) where

import System.Exit
import Data.Int (Int64)
import Control.Monad (filterM)
import System.Console.CmdArgs hiding (name)
import System.Directory
    ( listDirectory
    , doesFileExist
    , copyFile
    , createDirectoryIfMissing
    , removeFile
    )
import System.FilePath ((</>), (<.>), takeExtension)
import Data.List (find, isSuffixOf, foldl', sortBy)
import Data.Ord (comparing)
import qualified Data.Set as Set
import Data.Set (Set)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Data.Time.Clock (UTCTime)
import qualified Data.Map as Map
import Data.Maybe (fromJust, catMaybes)
import Data.Text (Text, unpack, toLower)
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8)
import Network.Mime (defaultMimeLookup)
import PerceptualHash (fileHash)
import Control.Exception.Safe (tryAny, tryAsync, SomeException, displayException)
import qualified Data.ByteString.Lazy as B
import Data.Aeson (FromJSON)

import JSONParsing
import qualified JSONCommonTypes as JS
import qualified JSONPost
import qualified Network.DataClient as Client
import qualified SitesType  as Sites
import qualified BoardsType as Boards
import qualified ThreadType as Threads
import qualified Common.AttachmentType as At
import qualified Common.PostsType as Posts
import qualified Hash
import qualified Data.WordUtil as Words
import qualified Common.Server.JSONSettings as J
import Common.Network.HttpClient (HttpError)
import qualified Common.Server.ConsumerSettings as CS

newtype SettingsCLI = SettingsCLI
  { jsonFile :: FilePath
  } deriving (Show, Data, Typeable)


-- Move a file by reading, writing, and then deleting the original
moveFile :: FilePath -> FilePath -> IO ()
moveFile src dst =
    B.readFile src >>= B.writeFile dst >> removeFile src


listCatalogDirectories :: J.JSONSettings -> IO [ FilePath ]
listCatalogDirectories settings = do
    allDirs <- listDirectory (J.backup_read_root settings)
    let filteredDirs = filter (`notElem` excludedDirs) allDirs
    filterM hasCatalog filteredDirs

  where
    excludedDirs = ["sfw", "alt", "overboard"]

    hasCatalog dir = do
      let catalogPath = J.backup_read_root settings </> dir </> "catalog.json"
      doesFileExist catalogPath


ensureSiteExists :: J.JSONSettings -> Either HttpError [ Sites.Site ] -> IO Sites.Site
ensureSiteExists settings sitesResult = do
    case sitesResult of
        Right siteList ->
            case find (\site -> Sites.name site == J.site_name settings) siteList of
            Just site -> do
                putStrLn $ J.site_name settings ++ " already exists!"
                return site
            Nothing -> do
                putStrLn $ J.site_name settings ++ " does not exist. Creating..."
                postResult <- Client.postSite settings

                case postResult of
                    Right (site:_) -> do
                        putStrLn $ "Successfully created " ++ J.site_name settings ++ ". " ++ show site
                        return site
                    Right [] -> do
                        putStrLn "Did not get new site id back from postgrest"
                        exitFailure
                    Left err -> do
                        putStrLn $ "Failed to create " ++ J.site_name settings
                            ++ " Error: " ++ show err
                        exitFailure

        Left err -> do
            putStrLn $ "Error fetching sites: " ++ show err
            exitFailure


createArchivesForNewBoards
    :: J.JSONSettings
    -> Set String
    -> [ String ]
    -> Int
    -> IO [ Boards.Board ]
createArchivesForNewBoards settings dirsSet archived_boards siteid = do
    let archivedBoardsSet = Set.fromList archived_boards

    -- Find boards that are in dirs but not in archived_boards
    let boardsToArchive = dirsSet `Set.difference` archivedBoardsSet

    putStrLn $ "Creating " ++ (show $ length boardsToArchive) ++ " boards:"
    mapM_ putStrLn boardsToArchive

    if Set.null boardsToArchive
    then return []
    else do
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
    :: J.JSONSettings
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

        archived_board_thread_ids :: Set.Set Int64
        archived_board_thread_ids =
            Set.fromList $ map Threads.board_thread_id archived_threads

        threads_to_create :: [ Thread ]
        threads_to_create =
            filter
                ((`Set.notMember` archived_board_thread_ids) . no)
                all_threads


ensureThreads :: J.JSONSettings -> Boards.Board -> [ Thread ] -> IO [ Threads.Thread ]
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
    :: FileGetters
    -> Sites.Site
    -> Boards.Board
    -> Threads.Thread
    -> IO (Threads.Thread, [ JSONPost.Post ])
readPosts FileGetters {..} site board thread = do
    result <- getJSONPosts site relative_path

    case result of
        Left err -> do
            putStrLn $ "Failed to parse the JSON file " ++ relative_path ++ " error: " ++ err
            putStrLn $ "Site: " ++ show site
            return (thread, [])
        Right posts_wrapper -> return (thread, JSONPost.posts posts_wrapper)

    where
        relative_path :: FilePath
        relative_path = Boards.pathpart board </> "res" </> (show (Threads.board_thread_id thread) ++ ".json")


apiPostToPostKey :: Threads.Thread -> JSONPost.Post -> Client.PostId
apiPostToPostKey thread post =
    Client.PostId
        { Client.thread_id = (Threads.thread_id thread)
        , Client.board_post_id = (JSONPost.no post)
        }


postHasAttachments :: JSONPost.Post -> Bool
postHasAttachments JSONPost.Post { JSONPost.files = Just _ } = True
postHasAttachments JSONPost.Post { JSONPost.filename = Just _ } = True
postHasAttachments _ = False


-- Convert Post to DbPost
apiPostToArchivePost :: Int -> Threads.Thread -> JSONPost.Post -> Posts.Post
apiPostToArchivePost local_idx thread post =
    Posts.Post
    { post_id         = Nothing
    , board_post_id   = JSONPost.no post
    , creation_time   = posixSecondsToUTCTime (realToFrac $ JSONPost.time post)
    , body            = JSONPost.com post
    , name            = JSONPost.name post
    , subject         = JSONPost.sub post
    , email           = JSONPost.email post
    , thread_id       = Threads.thread_id thread
    , embed           = JSONPost.embed post
    , local_idx       = local_idx
      -- TODO:
      -- rename this to "attachment_not_considered
      -- default this to true
      -- restore database from earlier point
      -- check what happens when we abort during the attachment download phase
      -- add flags
    , is_missing_attachments = postHasAttachments post -- initially posts with attachments aren't complete, keep the db state consistent.
    , sage            = emailToSage $ JSONPost.email post
    }

    where
        emailToSage :: Maybe Text -> Bool
        emailToSage Nothing  = False
        emailToSage (Just t) = toLower t == "sage"


addPostsToTuples
    :: [(Sites.Site, Boards.Board, Threads.Thread, JSONPost.Post)]
    -> [ Posts.Post ]
    -> [(Sites.Site, Boards.Board, Threads.Thread, JSONPost.Post, Posts.Post)]
addPostsToTuples tuples posts = map f posts
    where
        post_map :: Map.Map (Int64, Int64) (Sites.Site, Boards.Board, Threads.Thread, JSONPost.Post)
        post_map = Map.fromList (map (\(a, b, c, d) -> ((Threads.thread_id c, JSONPost.no d), (a, b, c, d))) tuples)

        f :: Posts.Post -> (Sites.Site, Boards.Board, Threads.Thread, JSONPost.Post, Posts.Post)
        f new_post =
            (\(a, b, c, d) -> (a, b, c, d, new_post))
            (post_map Map.! (Posts.thread_id new_post, Posts.board_post_id new_post))


fileToAttachment :: Int -> Posts.Post -> JS.File -> At.Attachment
fileToAttachment i post file =
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
        , At.attachment_idx = i
        }

    where
      extension = T.filter (/= '.') $ JS.ext file

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


makeThreadAttachmentFsPath
    :: J.JSONSettings
    -> Sites.Site
    -> Boards.Board
    -> Int64
    -> FilePath
makeThreadAttachmentFsPath settings site board thread_id
    = (J.media_root_path settings)
    </> Sites.name site
    </> Boards.pathpart board
    </> (show thread_id)

copyOrMoveFiles
    :: J.JSONSettings
    -> (String -> (String, String) -> (Maybe String, String) -> IO ())
    -> Details
    -> IO ()
copyOrMoveFiles settings copyOrMove (site, board, thread, _, path, attachment) = do
    copyOrMove common_dest (src, dest) (thumb_src, thumb_dest)

    -- src = (At.file_path | At.thumb_path)
    -- dest = <media_root>/<website_name>/<boardpart>/<board_thread_id>/<sha>.<ext>

    where
        src :: FilePath
        src = At.file_path path

        thumb_src :: Maybe FilePath
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
        common_dest = makeThreadAttachmentFsPath settings site board (Threads.board_thread_id thread)


type Details = (Sites.Site, Boards.Board, Threads.Thread, Posts.Post, At.Paths, At.Attachment)


parseAttachments
    :: String
    -> (Sites.Site, Boards.Board, Threads.Thread, JSONPost.Post, Posts.Post)
    -> [ Details ]
parseAttachments path_prefix (site, board, thread, p, q) = filter notDeleted $
    case JSONPost.files p of
        Just files -> map
            (\(i, x) ->
                ( site
                , board
                , thread
                , q
                , At.Paths (path_prefix ++ (unpack $ JS.file_path x)) (Just $ path_prefix ++ (unpack $ JS.thumb_path x))
                , fileToAttachment i q x
                )
            ) (zip [1..] files)
        Nothing ->
            case parseLegacyPaths board p path_prefix of
                Nothing -> []
                Just (paths, a) ->
                    let
                        dim = (JSONPost.w p) >>= \w -> ((JSONPost.h p) >>= \h -> Just $ At.Dimension w h)
                    in
                        [( site
                        , board
                        , thread
                        , q
                        , paths
                        , a
                            { At.creation_time = Posts.creation_time q
                            , At.resolution = dim
                            , At.post_id = fromJust $ Posts.post_id q
                            }
                        )]

    where
        notDeleted :: (a, b, c, d, At.Paths, At.Attachment) -> Bool
        notDeleted (_, _, _, _, paths, _) = not $ "deleted" `isSuffixOf` (At.file_path paths)


parseLegacyPaths :: Boards.Board -> JSONPost.Post -> String -> Maybe (At.Paths, At.Attachment)
parseLegacyPaths board post path_prefix = do
    tim <- JSONPost.tim post
    ext <- JSONPost.ext post
    filename <- JSONPost.filename post
    size <- JSONPost.fsize post
    spoiler <- JSONPost.fsize post

    let
        board_pathpart = T.pack $ Boards.pathpart board
        file_path = path_prefix </> (T.unpack $ board_pathpart <> "/src/" <> tim <> ext)
        thumb_extension = "png"
        thumbnail_path = path_prefix </> (T.unpack $ board_pathpart <> "/thumb/" <> tim <> "." <> thumb_extension)

        p = At.Paths file_path (Just thumbnail_path)

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
            , At.thumb_extension = Just thumb_extension
            , At.original_filename = Just $ filename <> ext
            , At.file_size_bytes = size
            , At.board_filename = tim
            , At.spoiler = spoiler > 0
            , At.attachment_idx = 1
            }

    return (p, attachment)


computeAttachmentHash :: Details -> IO At.Attachment
computeAttachmentHash (_, _, _, _, p, q) = do
    let f = At.file_path p

    putStrLn $ "Reading " ++ f

    sha256_sum <- Hash.computeSHA256 f

    putStrLn $ "SHA-256: " ++ unpack sha256_sum

    phash :: Maybe Int64 <-
        case (At.mimetype q) `Set.member` phash_mimetypes of
            True -> do
                putStrLn $ "Running tryAny $ fileHash f " ++ f
                either_exception <- tryAny $ fileHash f
                putStrLn $ "Done tryAny $ fileHash f " ++ f

                case either_exception of
                    Left (err :: SomeException) -> do
                        putStrLn $ "Error while computing the perceptual hash of file " ++ f ++ " " ++ displayException err
                        return Nothing
                    Right either_phash ->
                        case either_phash of
                            Left err_str -> do
                                putStrLn $ "Failed to compute phash for file " ++ (unpack sha256_sum) ++ " " ++ f ++ " " ++ err_str
                                return Nothing
                            Right phash_w -> do
                                result <- tryAsync $ do
                                    let phash_i = Words.wordToSignedInt64 phash_w

                                    if phash_i == 0 then do
                                        putStrLn $ "phash is 0 for file " ++ (unpack sha256_sum) ++ " " ++ f
                                        return Nothing
                                    else do
                                        putStrLn $ "phash: " ++ show phash_w
                                        return $ Just $ Words.wordToSignedInt64 phash_w

                                case result of
                                    Left (err2 :: SomeException) -> do
                                        putStrLn $ "Error handling phash result! " ++ displayException err2
                                        return Nothing

                                    Right w -> return w

            False -> return Nothing


    return q
        { At.sha256_hash = sha256_sum
        , At.phash = phash
        }


processFiles
    :: J.JSONSettings
    -> FileGetters
    -> [(Sites.Site, Boards.Board, Threads.Thread, JSONPost.Post, Posts.Post)]
    -> IO ()
processFiles settings fgs tuples = do -- perfect just means that our posts have ids, they're already inserted into the db
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

            let attachments_on_board :: [ Details ] =
                    concatMap (parseAttachments path_prefix) tuples
            -- attachments_on_board are the only files that can be copied into the archive dir right now
            -- since that's where we have the src filename. except here the Attachment doesn't have a sha hash yet
            -- so we can't build the destination filename.

            let map_should_exist :: Map.Map (Int64, Text) [ Details ] =
                    foldl'
                        (insertRecord (\(_, _, _, _, _, a) -> (At.post_id a, At.board_filename a)))
                        Map.empty
                        attachments_on_board

            let to_insert_map =
                    Map.filterWithKey
                        (\k _ -> not $ k `Map.member` map_existing)
                        map_should_exist

            let to_insert = concat $ Map.elems to_insert_map

            to_insert_ <- mapM ensureAttachmentExists to_insert

            let to_insert_exist = catMaybes to_insert_

            with_hashes <- mapM computeAttachmentHash to_insert_exist

            attachments_result <- Client.postAttachments settings with_hashes

            case attachments_result of
                Left err -> do
                    putStrLn $ "Error posting attachments: " ++ show err
                    exitFailure

                Right saved -> do
                    putStrLn $ "Saved " ++ (show $ length saved) ++ " attachments!"
                    mapM_ (copyOrMoveFiles settings (copyOrMove fgs)) to_insert_exist

    where
        ensureAttachmentExists :: Details -> IO (Maybe Details)
        ensureAttachmentExists (a, b, c, d, p, f) =
            (attachmentPaths fgs) p >>=
                return . (maybe Nothing (\x -> Just (a, b, c, d, x, f)))

        path_prefix :: String
        path_prefix = (addPathPrefix fgs) ""


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


localIndexFoldf
    :: ([Posts.Post], Map.Map Int64 Int)
    -> (Threads.Thread, JSONPost.Post, Client.PostId)
    -> ([Posts.Post], Map.Map Int64 Int)
localIndexFoldf (posts, idx_map) (t, p, c) =
    case Map.lookup thread_id idx_map of
        Nothing -> (post 1       : posts, Map.insert thread_id 1       idx_map)
        Just i  -> (post (i + 1) : posts, Map.insert thread_id (i + 1) idx_map)

    where
        post :: Int -> Posts.Post
        post i = apiPostToArchivePost i t p

        thread_id = Client.thread_id c


createNewPosts
    :: J.JSONSettings
    -> [ (Threads.Thread, JSONPost.Post, Client.PostId) ]
    -> IO [ Posts.Post ]
createNewPosts settings tuples = do
    existing_post_results <- Client.getPosts settings $ map (\(_, _, c) -> c) tuples
    existing_posts <- either handleError return existing_post_results

    thread_max_local_idx_result <- Client.getThreadMaxLocalIdx settings thread_ids
    thread_max_local_idxs <- either handleError return thread_max_local_idx_result

    let existing_set :: Set (Int64, Int64) = Set.fromList (map (\x -> (Posts.thread_id x, Posts.board_post_id x)) existing_posts)

    let to_insert_list :: [ (Threads.Thread, JSONPost.Post, Client.PostId) ] =
            sortBy (comparing $ \(_, _, p) -> Client.board_post_id p) $
                newPosts tuples existing_set

    -- Map of thread_id to the largest local_idx value (which would be the number of the last post in the thread)
    let local_idx :: Map.Map Int64 Int = Map.fromList thread_max_local_idxs

    let insert_posts :: [ Posts.Post ] = fst $ foldl' localIndexFoldf ([], local_idx) to_insert_list

    -- posts to insert are the posts that are not in existing_posts
    -- so we create a Set (thread_id, board_post_id) ✓
    -- then check every tuples against the set and the ones not in the set get added to a to_insert_list ✓
    -- also for every tuples we need to compute a local_idx
    -- so we create a Map index_map from thread_id to local_idx ✓
    --      - for existing_posts
    --      - need to compare posts already in the map with another post and keep the max local_idx ✓
    -- to get the new local_idx, we must order the to_insert_list by board_post_id, and look up each entry ✓

    print insert_posts
    posts_result <- Client.postPosts settings insert_posts
    new_posts <- either handleError return posts_result
    return $ existing_posts ++ new_posts

    where
        handleError err = print err >> exitFailure

        thread_ids :: [ Int64 ]
        thread_ids = Set.elems $ Set.fromList $ map (\(t, _, _) -> Threads.thread_id t) tuples

        newPosts :: [(Threads.Thread, JSONPost.Post, Client.PostId)] -> Set (Int64, Int64) -> [(Threads.Thread, JSONPost.Post, Client.PostId)]
        newPosts ts existing_set = filter (\(_, _, c) -> Set.notMember (Client.thread_id c, Client.board_post_id c) existing_set) ts


data FileGetters = FileGetters
    { getJSONCatalog :: Sites.Site -> String -> IO (Either String [ Catalog ])
    , getJSONPosts :: Sites.Site -> String -> IO (Either String JSONPost.PostWrapper)
    , addPathPrefix :: String -> String
    , attachmentPaths :: At.Paths -> IO (Maybe At.Paths)
    , copyOrMove :: String -> (String, String) -> (Maybe String, String) -> IO ()
    }


localFileGetters :: J.JSONSettings -> FileGetters
localFileGetters settings = FileGetters
    { getJSONCatalog = const $ parseJSONCatalog . withRoot
    , getJSONPosts = const $ parsePosts . withRoot
    , addPathPrefix = ((++) $ J.backup_read_root settings)
    , attachmentPaths = \p -> do
        exists <- doesFileExist (At.file_path p)
        if exists then return (Just p) else return Nothing
    , copyOrMove = \common_dest (src, dest) (m_thumb_src, thumb_dest) -> do
        destination_exists <- doesFileExist dest

        if not destination_exists
        then do
            src_exists <- doesFileExist src

            createDirectoryIfMissing True common_dest

            if src_exists
            then putStrLn ("Copying " ++ src) >> copyFile src dest
            else return ()

            case m_thumb_src of
                Nothing -> return ()
                Just thumb_src -> do
                    thumb_exists <- doesFileExist thumb_src

                    if thumb_exists
                    then putStrLn ("Copying " ++ thumb_src) >> copyFile thumb_src thumb_dest
                    else return ()

        else return ()
    }

    where
        withRoot = (J.backup_read_root settings </>)


-- This one is not designed to run concurrently
processBoard :: J.JSONSettings -> FileGetters -> Sites.Site -> Boards.Board -> IO ()
processBoard settings fgs@FileGetters {..} site board = do
    let catalogPath = Boards.pathpart board </> "catalog.json"
    putStrLn $ "catalog file path: " ++ catalogPath

    result <- getJSONCatalog site catalogPath

    case result of
        Right (catalogs :: [ Catalog ]) -> do
            let threads_on_board = concatMap ((maybe [] id) . threads) catalogs

            all_threads_for_board :: [ Threads.Thread ] <- ensureThreads settings board threads_on_board

            all_posts_on_board :: [(Threads.Thread, [ JSONPost.Post ])] <- mapM (readPosts fgs site board) all_threads_for_board

            let tuples :: [(Sites.Site, Boards.Board, Threads.Thread, JSONPost.Post)] = concatMap
                    (\(t, posts) -> map (\p -> (site, board, t, p)) posts)
                    all_posts_on_board

            posts_result :: [ Posts.Post ] <- createNewPosts settings (map (\(_, _, c, d) -> (c, d, apiPostToPostKey c d)) tuples)

            putStrLn "Sum of post_ids:"
            print $ sum $ map (fromJust . Posts.post_id) posts_result
            putStrLn "Sum of board_post_ids:"
            print $ sum $ map Posts.board_post_id posts_result

            let perfect_post_pairs = addPostsToTuples tuples posts_result

            processFiles settings fgs perfect_post_pairs

        Left errMsg    ->
            putStrLn $ "Failed to parse the JSON file in directory: "
                ++ (Boards.pathpart board) ++ ". Error: " ++ errMsg


getBoards :: J.JSONSettings -> [ FilePath ] -> IO (Sites.Site, [ Boards.Board ])
getBoards settings board_names = do
    sitesResult <- Client.getAllSites settings
    site :: Sites.Site <- ensureSiteExists settings sitesResult

    let boardsSet = Set.fromList board_names
    let site_id_ = Sites.site_id site
    boards_result <- Client.getSiteBoards settings site_id_
    putStrLn "Boards fetched!"

    case boards_result of
        Left err -> do
            putStrLn $ "Error fetching boards: " ++ show err
            exitFailure
        Right archived_boards -> do
            let boardnames = map Boards.pathpart archived_boards
            created_boards <- createArchivesForNewBoards settings boardsSet boardnames site_id_
            let boards :: [ Boards.Board ] = archived_boards ++ created_boards
            let boards_we_have_data_for = filter (\board -> Set.member (Boards.pathpart board) boardsSet) boards
            return (site, boards_we_have_data_for)


processBoards :: J.JSONSettings -> FileGetters -> [ FilePath ] -> IO ()
processBoards settings fgs board_names =
    getBoards settings board_names >>= \(site, boards) ->
        mapM_ (processBoard settings fgs site) boards


processBackupDirectory :: J.JSONSettings -> IO ()
processBackupDirectory settings = do
    putStrLn "JSON successfully read!"
    print settings  -- print the decoded JSON settings
    boards <- listCatalogDirectories settings
    processBoards settings (localFileGetters settings) boards


toClientSettings :: CS.ConsumerJSONSettings -> CS.JSONSiteSettings -> J.JSONSettings
toClientSettings CS.ConsumerJSONSettings {..} CS.JSONSiteSettings {..} =
    J.JSONSettings
    { J.postgrest_url = postgrest_url
    , J.jwt = jwt
    , J.backup_read_root = undefined
    , J.media_root_path = media_root_path
    , J.site_name = name
    , J.site_url = root_url
    }


httpGetJSON :: (FromJSON a) => Sites.Site -> String -> IO (Either String a)
httpGetJSON site path = (Client.getJSON $ Sites.url site </> path)
    >>= getErrMsg
    where
        getErrMsg :: Either Client.HttpError a -> IO (Either String a)
        getErrMsg (Left err) = return $ Left $ show err
        getErrMsg (Right x) = return $ Right x

httpFileGetters :: J.JSONSettings -> FileGetters
httpFileGetters settings = FileGetters
    { getJSONCatalog = httpGetJSON
    , getJSONPosts = httpGetJSON
    , addPathPrefix = ((++) $ J.site_url settings)
      -- attachmentPaths here actually doesn't get the paths of the attachment,
      -- it downloads them into a temporary file and gets that path of that.
    , attachmentPaths = \paths -> do
        filepath <- Client.getFile (At.file_path paths)

        m_thumbpath <- case At.thumbnail_path paths of
            Nothing -> return $ Left undefined
            Just thumbpath -> Client.getFile thumbpath

        case filepath of
            Left err -> do
                print err
                return Nothing

            Right p ->
                case m_thumbpath of
                    Left _ -> do
                        return $ Just $ At.Paths p Nothing
                    Right tp -> return $ Just $ At.Paths p $ Just tp


    , copyOrMove = moveAttachmentAndThumb
    }


moveAttachmentAndThumb :: String -> (String, String) -> (Maybe String, String) -> IO ()
moveAttachmentAndThumb common_dest (src, dest) (m_thumb_src, thumb_dest) = do
    putStrLn $ "Copy Or Move (Move) src: " ++ src ++ " dest: " ++ dest
    createDirectoryIfMissing True common_dest
    moveFile src dest

    case m_thumb_src of
      Nothing -> return ()
      Just thumb_src -> moveFile thumb_src thumb_dest
