{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Network.DataClient
  ( HttpError(..)
  , PostId (..)
  , get
  , getSiteBoards
  , getAllSites
  , postSite
  , post
  , postBoards
  , getThreads
  , getThreadMaxLocalIdx
  , postThreads
  , postPosts
  , getAttachments
  , postAttachments
  , getJSON
  , getFile
  , getLatestPostsPerBoard
  , deleteThreads
  , updatePostAttachmentNotConsidered
  , get_ -- from Common.Network.HttpClient
  , getAllAttachmentsPaged
  , getPostIdsByBoardIds
  , eitherDecodeResponse
  ) where

import Control.Monad (forM)
import Data.Int (Int64)
import Data.Either (lefts, rights)
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Lazy.Char8 as LC8
import Data.List (intercalate)
import Data.Aeson
  ( eitherDecode
  , ToJSON
  , FromJSON
  , (.=)
  , object
  , encode
  , Value
  )
import GHC.Generics
import System.IO.Temp (openBinaryTempFile, getCanonicalTemporaryDirectory)
import System.IO (hClose)

import qualified Common.Server.JSONSettings as T
import qualified SitesType as Sites
import qualified BoardsType as Boards
import qualified ThreadType as Threads
import qualified Common.AttachmentType as Attachments
import qualified Common.PostsType  as Posts
import Common.Network.HttpClient
import qualified Network.DataClientTypes as T
import qualified Network.GetLatestPostsPerBoardResponse as GLPPBR
import qualified Common.Network.SiteType as Site
import Common.Parsing.FlexibleJsonResponseParser as Flx


data PostId = PostId { board_post_id :: Int64 }
    deriving (Show, Generic, ToJSON, FromJSON)

getSiteBoards :: T.JSONSettings -> Int -> IO (Either HttpError [ Boards.Board ])
getSiteBoards settings site_id_ = eitherDecodeResponse <$>
    get settings path
    where
        path = "/boards?site_id=eq." ++ show site_id_


postSite :: T.JSONSettings -> IO (Either HttpError [ Sites.Site ])
postSite settings =
    eitherDecodeResponse <$> post settings "/sites" payload True

    where
      payload = encode $
            object [ "name" .= T.site_name settings
                   , "url"  .= T.site_url  settings
                   ]

postBoards
    :: T.JSONSettings
    -> [] String
    -> Int
    -> IO (Either HttpError [ Boards.Board ])
postBoards settings boards siteid = eitherDecodeResponse <$>
    post settings "/boards" payload True

    where
      payload = encode $ fmap mk_obj boards

      mk_obj :: String -> Value
      mk_obj board = object
          [ "pathpart" .= board
          , "site_id"  .= siteid
          ]


postThreads
    :: T.JSONSettings
    -> [ Threads.Thread ]
    -> IO (Either HttpError [ Threads.Thread ])
postThreads settings threads = eitherDecodeResponse <$>
    post settings "/threads" payload True

    where
      payload = encode $ fmap mk_obj threads

      mk_obj :: Threads.Thread -> Value
      mk_obj thread = object
          [ "board_thread_id" .= Threads.board_thread_id thread
          , "creation_time"   .= Threads.creation_time thread
          , "board_id"        .= Threads.board_id thread
          ]


getAllSites :: T.JSONSettings -> IO (Either HttpError [ Sites.Site ])
getAllSites settings = eitherDecodeResponse <$>
    get settings "/sites"


getThreads :: T.JSONSettings -> Int -> [ Int64 ] -> IO (Either HttpError [ Threads.Thread ])
getThreads settings board_id board_thread_ids = eitherDecodeResponse <$>
    get settings path

    where
        path = "/threads?board_thread_id=in.(" ++ ids ++ ")&board_id=eq." ++ show board_id
        ids :: String = intercalate "," $ map show board_thread_ids


deleteThreads :: T.JSONSettings -> Int -> [ Int64 ] -> IO (Either HttpError LBS.ByteString)
deleteThreads settings board_id board_thread_ids =
    delete settings path False

    where
        path = "/threads?board_thread_id=in.(" ++ ids ++ ")&board_id=eq." ++ show board_id
        ids :: String = intercalate "," $ map show board_thread_ids


getThreadMaxLocalIdx :: T.JSONSettings -> [ Int64 ] -> IO (Either HttpError [(Int64, Int)])
getThreadMaxLocalIdx settings thread_ids = do
    result :: Either HttpError [ T.ThreadMaxIdx ] <- eitherDecodeResponse <$> get settings path

    let results = result >>= \x -> return $ map (\t -> (T.thread_id t, T.max_idx t)) x

    return results

    where
        path = "/posts?select=thread_id,max_idx:local_idx.max()&thread_id=in.(" ++ ids ++ ")"
        ids :: String = intercalate "," $ map show thread_ids


-- | Splits a list into chunks of a given size.
chunkList :: Int -> [a] -> [[a]]
chunkList _ [] = []
chunkList n xs = let (chunk, rest) = splitAt n xs in chunk : chunkList n rest


-- TODO: idk if we need this anymore!
getAttachments :: T.JSONSettings -> [Int64] -> IO (Either HttpError [Attachments.Attachment])
getAttachments settings post_ids = do
    results <- forM (chunkList chunkSize post_ids) (getAttachmentsChunk settings)
    return $ combineResults results
  where
    chunkSize = 1000


-- | Combines the results, prioritizing errors.
combineResults :: [Either e [b]] -> Either e [b]
combineResults results =
    case lefts results of
        [] -> Right (concat (rights results))
        (err:_) -> Left err


-- TODO: idk if we need this anymore!
-- | Function to handle each chunk.
getAttachmentsChunk :: T.JSONSettings -> [Int64] -> IO (Either HttpError [Attachments.Attachment])
getAttachmentsChunk settings chunk = eitherDecodeResponse <$>
    get settings path

    where
        path = "/attachments?post_id=in.(" ++ intercalate "," (map show chunk) ++ ")"


getAllAttachmentsPaged
    :: T.JSONSettings
    -> Int
    -> Int
    -> IO (Either HttpError [ Site.Site ])
getAllAttachmentsPaged settings limit offset = do
    response <- get settings path
    return $ sitesFromSSites <$> eitherDecodeResponse response

    where
        path = "/attachments?select=*,posts:post_id(*,threads:thread_id(*,boards:board_id(*,sites:site_id(*))))&order=attachment_id.desc"
            ++ "&limit=" ++ show limit
            ++ "&offset=" ++ show offset


postAttachments
    :: T.JSONSettings
    -> [ Attachments.Attachment ]
    -> IO (Either HttpError [ Attachments.Attachment ])
postAttachments settings attachments = eitherDecodeResponse <$>
    post settings "/attachments" payload True

    where
        payload = encode attachments


-- -- TODO: this can be deleted
-- -- | Function to handle each chunk.
-- getPostsChunk :: T.JSONSettings -> [ PostId ] -> IO (Either HttpError [ Posts.Post ])
-- getPostsChunk settings chunk = eitherDecodeResponse <$>
--     post settings "/rpc/get_posts" payload False
-- 
--     where
--         payload = encode $ object [ "board_posts" .= chunk ]
-- 
-- 
-- -- TODO: this can be deleted
-- getPosts :: T.JSONSettings -> [ PostId ] -> IO (Either HttpError [ Posts.Post ])
-- getPosts settings xs = do
--     results <- forM (chunkList chunkSize xs) (getPostsChunk settings)
--     return $ combineResults results
-- 
--   where
--     chunkSize = 1000

-- | Get post_ids based on the board_id and a list of board_post_ids
getPostIdsChunk :: T.JSONSettings -> Int -> [ Int64 ] -> IO (Either HttpError [ PostId ])
getPostIdsChunk settings board_id board_post_ids = eitherDecodeResponse <$>
    get settings path

    where
        path = "/posts?select=board_post_id,threads:thread_id!inner()&board_post_id=in.("
            ++ intercalate "," (map show board_post_ids)
            ++ ")&threads.board_id=eq." ++ show board_id
            ++ "&attachment_not_considered=eq.false"

getPostIdsByBoardIds :: T.JSONSettings -> Int -> [ Int64 ] -> IO (Either HttpError [ Int64 ] )
getPostIdsByBoardIds settings board_id board_post_ids = do
    results <- forM
        (chunkList chunkSize board_post_ids)
        (getPostIdsChunk settings board_id)
    return $ (map board_post_id) <$> (combineResults results)

    where
        chunkSize = 1000

postPosts
    :: T.JSONSettings
    -> [ Posts.Post ]
    -> IO (Either HttpError [ Posts.Post ])
postPosts settings posts = eitherDecodeResponse <$>
    post settings "/posts" payload True

    where
        payload = encode posts


eitherDecodeResponse :: (FromJSON a) => Either HttpError LBS.ByteString -> Either HttpError a
eitherDecodeResponse (Left err) = Left err
eitherDecodeResponse (Right bs) =
    case eitherDecode bs of
        Right val -> Right val
        Left err -> Left $ StatusCodeError 500 $ LC8.pack $ "Failed to decode JSON: " ++ err ++ " " ++ show bs


getJSON :: (FromJSON a) => String -> IO (Either HttpError a)
getJSON url = eitherDecodeResponse <$> get_ url []


getFile :: String -> IO (Either HttpError String)
getFile url = do
    putStrLn $ "getFile " ++ url
    result <- get_ url []

    case result of
        Left (err :: HttpError) -> return $ Left err
        Right lbs -> do
            putStrLn $ "getFile " ++ url ++ " SUCCESS!"
            tmp_root <- getCanonicalTemporaryDirectory
            (tmp_filepath, tmp_filehandle) <- openBinaryTempFile tmp_root "chan.attachment"
            putStrLn $ "Created " ++ tmp_filepath
            putStrLn "Writing attachment..."
            LBS.hPut tmp_filehandle lbs
            hClose tmp_filehandle
            return $ Right tmp_filepath


getLatestPostsPerBoard :: T.JSONSettings -> IO (Either HttpError [ GLPPBR.GetLatestPostsPerBoardResponse ])
getLatestPostsPerBoard settings = eitherDecodeResponse <$>
    post settings "/rpc/get_latest_posts_per_board" mempty False


updatePostAttachmentNotConsidered :: T.JSONSettings -> [ Int64 ] -> IO (Either HttpError LBS.ByteString )
updatePostAttachmentNotConsidered settings post_ids =
    patch settings path payload False

    where
        path = "/posts?thread_id=in.(" ++ intercalate "," (map show post_ids) ++ ")&attachment_not_considered=eq.true"
        payload = encode $ object [ "attachment_not_considered" .= False ]


siteFromSSite :: Flx.SSite -> Site.Site
siteFromSSite (SSite s) = s


sitesFromSSites :: [ Flx.SSite ] -> [ Site.Site ]
sitesFromSSites = map siteFromSSite
