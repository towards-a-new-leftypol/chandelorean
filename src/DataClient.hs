{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveAnyClass #-}

module DataClient
  ( HttpError(..)
  , PostId (..)
  , get
  , getSiteBoards
  , getAllSites
  , postSite
  , post
  , postBoards
  , getThreads
  , postThreads
  , postPosts
  , getAttachments
  , postAttachments
  ) where

import Control.Monad (forM)
import Data.Int (Int64)
import Data.Either (lefts, rights)
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Lazy.Char8 as LC8
import Data.List (intercalate)
import Data.Aeson
  ( eitherDecode
  , FromJSON
  , (.=)
  , object
  , encode
  , Value
  )
import GHC.Generics

import qualified JSONSettings as T
import qualified SitesType as Sites
import qualified BoardsType as Boards
import qualified ThreadType as Threads
import qualified Common.AttachmentType as Attachments
import qualified Common.PostsType  as Posts
import Common.Network.HttpClient

data PostId = PostId
    { post_id :: Int64
    , board_post_id :: Int64
    , thread_id :: Int64
    } deriving (Show, Generic, FromJSON)

getSiteBoards :: T.JSONSettings -> Int -> IO (Either HttpError [ Boards.Board ])
getSiteBoards settings site_id_ = get settings path >>= return . eitherDecodeResponse
  where
    path = "/boards?site_id=eq." ++ show site_id_


postSite :: T.JSONSettings -> IO (Either HttpError [ Sites.Site ])
postSite settings =
    post settings "/sites" payload True >>= return . eitherDecodeResponse

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
postBoards settings boards siteid =
    post settings "/boards" payload True >>= return . eitherDecodeResponse

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
postThreads settings threads =
    post settings "/threads" payload True >>= return . eitherDecodeResponse

    where
      payload = encode $ fmap mk_obj threads

      mk_obj :: Threads.Thread -> Value
      mk_obj thread = object
          [ "board_thread_id" .= Threads.board_thread_id thread
          , "creation_time"   .= Threads.creation_time thread
          , "board_id"        .= Threads.board_id thread
          ]


getAllSites :: T.JSONSettings -> IO (Either HttpError [ Sites.Site ])
getAllSites settings = get settings "/sites" >>= return . eitherDecodeResponse

getThreads :: T.JSONSettings -> Int -> [ Int ] -> IO (Either HttpError [ Threads.Thread ])
getThreads settings board_id board_thread_ids =
    get settings path >>= return . eitherDecodeResponse

    where
        path = "/threads?board_thread_id=in.(" ++ ids ++ ")&board_id=eq." ++ show board_id
        ids :: String = intercalate "," $ map show board_thread_ids


-- | Splits a list into chunks of a given size.
chunkList :: Int -> [a] -> [[a]]
chunkList _ [] = []
chunkList n xs = let (chunk, rest) = splitAt n xs in chunk : chunkList n rest


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


-- | Function to handle each chunk.
getAttachmentsChunk :: T.JSONSettings -> [Int64] -> IO (Either HttpError [Attachments.Attachment])
getAttachmentsChunk settings chunk =
    get settings path >>= return . eitherDecodeResponse

    where
        path = "/attachments?post_id=in.(" ++ intercalate "," (map show chunk) ++ ")"


postAttachments
    :: T.JSONSettings
    -> [ Attachments.Attachment ]
    -> IO (Either HttpError [ Attachments.Attachment ])
postAttachments settings attachments = do
    BL.putStrLn payload
    post settings "/attachments" payload True >>= return . eitherDecodeResponse

    where
      payload = encode attachments


postPosts
    :: T.JSONSettings
    -> [ Posts.Post ]
    -> IO (Either HttpError [ PostId ])
postPosts settings posts =
    post settings "/rpc/insert_posts_and_return_ids" payload True >>= return . eitherDecodeResponse

    where
      payload = encode $ object [ "new_posts" .= posts ]


eitherDecodeResponse :: (FromJSON a) => Either HttpError LBS.ByteString -> Either HttpError a
eitherDecodeResponse (Left err) = Left err
eitherDecodeResponse (Right bs) =
    case eitherDecode bs of
        Right val -> Right val
        Left err -> Left $ StatusCodeError 500 $ LC8.pack $ "Failed to decode JSON: " ++ err ++ " " ++ (show bs)
