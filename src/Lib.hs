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
    , SettingsCLI (..)
    , epochToUTCTime
    , apiThreadToArchiveThread
    , Details
    , parseAttachments
    , computeAttachmentHash
    , copyOrMoveFiles
    , moveAttachmentAndThumb
    , makeThreadAttachmentFsPath
    , moveFile
    , apiPostToArchivePost
    ) where

import System.Exit
import Data.Int (Int64)
import System.Console.CmdArgs hiding (name)
import System.Directory
    ( createDirectoryIfMissing
    , removeFile
    )
import System.FilePath ((</>), (<.>), takeExtension)
import Data.List (find, isSuffixOf)
import qualified Data.Set as Set
import Data.Set (Set)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Data.Time.Clock (UTCTime)
import Data.Maybe (fromJust)
import Data.Text (Text, unpack, toLower)
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8)
import Network.Mime (defaultMimeLookup)
import PerceptualHash (fileHash)
import Control.Exception.Safe (tryAsync, SomeException, displayException)
import qualified Data.ByteString.Lazy as B

import Network.Api.JSONParsing
import qualified Network.Api.JSONCommonTypes as JS
import qualified Network.Api.JSONPost as JSONPost
import qualified Network.Api.JSONExtraFile as EF
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
import qualified CliSettings as CS
import qualified Common.Network.SiteType as Site

newtype SettingsCLI = SettingsCLI
  { jsonFile :: FilePath
  } deriving (Show, Data, Typeable)


-- Move a file by reading, writing, and then deleting the original
moveFile :: FilePath -> FilePath -> IO ()
moveFile src dst =
    B.readFile src >>= B.writeFile dst >> removeFile src


siteFromSite :: Sites.Site -> Site.Site
siteFromSite s = Site.Site
    { Site.site_id = Sites.site_id s
    , Site.name = T.pack $ Sites.name s
    , Site.url = T.pack $ Sites.url s
    , Site.boards = []
    }


ensureSiteExists :: J.JSONSettings -> Either HttpError [ Site.Site ] -> IO Site.Site
ensureSiteExists settings sitesResult = do
    case sitesResult of
        Right siteList ->
            case find (\site -> unpack (Site.name site) == J.site_name settings) siteList of
            Just site -> do
                putStrLn $ J.site_name settings ++ " already exists!"
                return site
            Nothing -> do
                putStrLn $ J.site_name settings ++ " does not exist. Creating..."
                postResult <- Client.postSite settings

                case postResult of
                    Right (site:_) -> do
                        putStrLn $ "Successfully created " ++ J.site_name settings ++ ". " ++ show site
                        return $ siteFromSite site
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


-- Convert Post to DbPost
apiPostToArchivePost :: Int -> Threads.Thread -> JSONPost.Post -> Posts.Post
apiPostToArchivePost local_idx thread post =
    Posts.Post
    { post_id         = Nothing
    , board_post_id   = JSONPost.no post
    , creation_time   = epochToUTCTime $ JSONPost.time post
    , body            = sanitize <$> JSONPost.com post
    , name            = sanitize <$> JSONPost.name post
    , subject         = sanitize <$> JSONPost.sub post
    , email           = sanitize <$> JSONPost.email post
    , thread_id       = Threads.thread_id thread
    , embed           = sanitize <$> JSONPost.embed post
    , local_idx       = local_idx
    , attachment_not_considered = True
    , sage            = emailToSage $ JSONPost.email post
    }

    where
        emailToSage :: Maybe Text -> Bool
        emailToSage Nothing  = False
        emailToSage (Just t) = toLower t == "sage"

        sanitize :: Text -> Text
        sanitize = T.filter (/= '\x00')


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

      dim = do
          w <- JS.w file
          h <- JS.h file
          return $ At.Dimension w h


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
    -> Site.Site
    -> Boards.Board
    -> Int64
    -> FilePath
makeThreadAttachmentFsPath settings site board thread_id
    = (J.media_root_path settings)
    </> unpack (Site.name site)
    </> Boards.pathpart board
    </> (show thread_id)


copyOrMoveFiles
    :: J.JSONSettings
    -> (String -> (String, String) -> (Maybe String, String) -> IO ())
    -> Details
    -> IO ()
copyOrMoveFiles settings copyOrMove (site, board, thread, _, Just (path, attachment)) = do
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

copyOrMoveFiles _ _ _ = return ()


type Details = (Site.Site, Boards.Board, Threads.Thread, Posts.Post, Maybe (At.Paths, At.Attachment))


parseAttachments
    :: String
    -> (Site.Site, Boards.Board, Threads.Thread, JSONPost.Post, Posts.Post)
    -> [ Details ]
parseAttachments path_prefix (site, board, thread, p, q) = filter notDeleted $
    case JSONPost.files p of
        Just files -> map
            (\(i, x) ->
                ( site
                , board
                , thread
                , q
                , Just
                    ( At.Paths (path_prefix ++ (unpack $ JS.file_path x)) (Just $ path_prefix ++ (unpack $ JS.thumb_path x))
                    , fileToAttachment i q x
                    )
                )
            ) (zip [1..] files)
        Nothing ->
            case parseLegacyPaths board p path_prefix of
                Nothing -> [(site, board, thread, q, Nothing)]
                Just (paths, a) ->
                    let dim = do
                            w <- JSONPost.w p
                            h <- JSONPost.h p
                            return $ At.Dimension w h
                    in
                        ( site
                        , board
                        , thread
                        , q
                        , Just
                            ( paths
                            , a
                                { At.creation_time = Posts.creation_time q
                                , At.resolution = dim
                                , At.post_id = fromJust $ Posts.post_id q
                                }
                            )
                        ) : (
                                (map $ (\(x, y) -> (site, board, thread, q, Just (x, y))) . (parseExtraFiles board q p path_prefix))
                                (zip [2..] $ maybe [] id $ JSONPost.extra_files p)
                            )

    where
        notDeleted :: Lib.Details -> Bool
        notDeleted (_, _, _, _, Just (paths, _)) = not $ "deleted" `isSuffixOf` (At.file_path paths)
        notDeleted _ = True


parseLegacyPaths :: Boards.Board -> JSONPost.Post -> String -> Maybe (At.Paths, At.Attachment)
parseLegacyPaths board post path_prefix = do
    tim <- JSONPost.tim post
    ext <- JSONPost.ext post
    filename <- JSONPost.filename post
    size <- JSONPost.fsize post

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
            , At.spoiler = maybe False (> 0) $ JSONPost.spoiler post
            , At.attachment_idx = 1
            }

    return (p, attachment)


parseExtraFiles :: Boards.Board -> Posts.Post -> JSONPost.Post -> String -> (Int, EF.ExtraFile) -> (At.Paths, At.Attachment)
parseExtraFiles board post json_post path_prefix (idx, extra_file) =
    let
        tim      = EF.tim extra_file
        ext      = EF.ext extra_file
        filename = EF.filename extra_file
        size     = EF.fsize extra_file

        board_pathpart = T.pack $ Boards.pathpart board
        file_path = path_prefix </> (T.unpack $ board_pathpart <> "/src/" <> tim <> ext)
        thumb_extension = "png"
        thumbnail_path = path_prefix </> (T.unpack $ board_pathpart <> "/thumb/" <> tim <> "." <> thumb_extension)

        p = At.Paths file_path (Just thumbnail_path)

        mime = getMimeType ext

        dim = do
            w <- EF.w extra_file
            h <- EF.h extra_file
            return $ At.Dimension w h

        attachment = At.Attachment
            { At.mimetype = mime
            , At.creation_time = Posts.creation_time post
            , At.sha256_hash = undefined
            , At.phash = Nothing
            , At.illegal = False
            , At.post_id = fromJust $ Posts.post_id post
            , At.resolution = dim
            , At.file_extension = Just $ T.drop 1 ext
            , At.thumb_extension = Just thumb_extension
            , At.original_filename = Just $ filename <> ext
            , At.file_size_bytes = size
            , At.board_filename = tim
            , At.spoiler = maybe False (> 0) $ JSONPost.spoiler json_post
            , At.attachment_idx = idx
            }

    in
        (p, attachment)


computeAttachmentHash :: At.Paths -> At.Attachment -> IO At.Attachment
computeAttachmentHash p q = do
    let f = At.file_path p

    putStrLn $ "Reading " ++ f

    sha256_sum <- Hash.computeSHA256 f

    putStrLn $ "SHA-256: " ++ unpack sha256_sum

    phash :: Maybe Int64 <-
        case (At.mimetype q) `Set.member` phash_mimetypes of
            True -> do
                putStrLn $ "Running fileHash f " ++ f
                either_phash <- fileHash f
                putStrLn $ "Done fileHash f " ++ f

                case either_phash of
                    Left _ -> do
                        -- there was a bug in evauating the error value here, program would freeze. so no error message from fileHash for us.
                        putStrLn $ "Failed to compute phash for file " ++ (unpack sha256_sum)
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


moveAttachmentAndThumb
    :: String
    -> (String, String)
    -> (Maybe String, String)
    -> IO ()
moveAttachmentAndThumb common_dest (src, dest) (m_thumb_src, thumb_dest) = do
    putStrLn $ "Copy Or Move (Move) src: " ++ src ++ " dest: " ++ dest
    createDirectoryIfMissing True common_dest
    moveFile src dest

    case m_thumb_src of
      Nothing -> return ()
      Just thumb_src -> moveFile thumb_src thumb_dest
