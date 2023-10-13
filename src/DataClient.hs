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
  ) where

import Data.Int (Int64)
import Network.HTTP.Simple
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Lazy.Char8 as LC8
import Network.HTTP.Types.Status (statusCode)
import Control.Exception.Safe (tryAny, SomeException)
import Data.List (intercalate)
import qualified Data.ByteString.Char8 as C8
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
import qualified PostsType  as Posts

data HttpError
    = HttpException SomeException
    | StatusCodeError Int LBS.ByteString
    deriving (Show)

data PostId = PostId
    { post_id :: Int64
    , board_post_id :: Int64
    , thread_id :: Int64
    } deriving (Show, Generic, FromJSON)

get :: T.JSONSettings -> String -> IO (Either HttpError LBS.ByteString)
get settings path = do
    let requestUrl = T.postgrest_url settings ++ path
    initReq <- parseRequest requestUrl
    let req = setRequestHeader "Authorization" [C8.pack $ "Bearer " ++ T.jwt settings] initReq
    putStrLn $ "calling " ++ requestUrl
    handleHttp (httpLBS req)


post
  :: T.JSONSettings
  -> String
  -> LBS.ByteString
  -> Bool
  -> IO (Either HttpError LBS.ByteString)
post settings path payload return_repr = do
    let requestUrl = T.postgrest_url settings ++ path
    initReq <- parseRequest requestUrl
    let req = setRequestMethod "POST"
            . setRequestHeader "Authorization" [ jwt_header ]
            . setRequestHeader "Content-Type" [ "application/json" ]
            . setRequestBodyLBS payload
            . prefer
            $ initReq

    putStrLn $ "posting to " ++ requestUrl
    -- putStrLn $ "Payload: " ++ (LC8.unpack payload)
    handleHttp (httpLBS req)

    where
      jwt_header = C8.pack $ "Bearer " ++ T.jwt settings
      prefer =
        if return_repr
        then setRequestHeader "Prefer" [ "return=representation" ]
        else id


handleHttp :: IO (Response LBS.ByteString) -> IO (Either HttpError LBS.ByteString)
handleHttp action = do
    result <- tryAny action
    case result of
        Right response -> 
            let responseBody = getResponseBody response
            in if 200 <= (statusCode $ getResponseStatus response) && (statusCode $ getResponseStatus response) < 300
               then return $ Right responseBody
               else return $ Left (StatusCodeError (statusCode $ getResponseStatus response) responseBody)
        Left e -> return $ Left $ HttpException e


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
