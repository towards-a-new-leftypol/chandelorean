{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module Network.SpamNoticer where

import GHC.Generics
import Data.Aeson (ToJSON, FromJSON, Value, encode)
import Data.Text (Text)
import Data.Int (Int64)
import Network.HTTP.Simple
    ( setRequestMethod
    , parseRequest
    , httpLBS
    )
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)

import Common.Network.HttpClient (HttpError, handleHttp)
import Network.HTTP.Client.MultipartFormData
    ( formDataBody
    , partLBS
    , partFileSource
    )
import Network.DataClient (eitherDecodeResponse)
import qualified Lib
import qualified SitesType  as Sites
import qualified BoardsType as Boards
import qualified ThreadType as Threads
import qualified Common.PostsType as Posts
import qualified Common.AttachmentType as At
import Hash (computeMD5)

data SpamNoticerAttachmentMetadata =
    SpamNoticerAttachmentMetadata
        { filename :: Text
        -- , thumbnail_url :: Maybe String
        , mimetype :: Text
        , md5_hash :: Text
        } deriving (Show, Generic, ToJSON, FromJSON)
    
data SpamNoticerRequestInfo =
    SpamNoticerRequestInfo
        { attachments :: [ SpamNoticerAttachmentMetadata ]
        , body  :: Maybe Text
        , time_stamp :: Integer
        , website_name :: String
        , board_name :: String
        , thread_id :: Int64
        , skip_recent_record :: Bool
        } deriving (Show, Generic, ToJSON, FromJSON)

data SpamNoticerResponse =
    SpamNoticerResponse
        { noticed :: Bool
        , reason  :: Maybe Integer
        , details :: Maybe Value
        , debug_transaction_log :: Maybe Value
        } deriving (Show, Generic, ToJSON, FromJSON)

data SpamNoticerSettings =
    SpamNoticerSettings
        { base_url :: String
        } deriving (Show, Generic, ToJSON, FromJSON)

askNoticer
    :: SpamNoticerSettings
    -> SpamNoticerRequestInfo
    -> [ FilePath ]
    -> IO (Either HttpError SpamNoticerResponse)
askNoticer settings requestInfo attachmentPaths = do
    putStrLn $ "POSTing SpamNoticer query to " ++ url

    eitherDecodeResponse <$>
        ( handleHttp $ do
            req <- parseRequest url
            let httpRequest = setRequestMethod "POST" req
            request <- formDataBody (jsonPart : attachmentParts) httpRequest
            httpLBS request
        )

    where
        url = base_url settings ++ path

        path = "/"

        jsonPart = partLBS "json" $ encode requestInfo

        attachmentParts = map (partFileSource "attachments") attachmentPaths


noticerReqInfoFromDetails
    :: Sites.Site
    -> Boards.Board
    -> Threads.Thread
    -> Posts.Post
    -> [ Lib.Details ]
    -> IO SpamNoticerRequestInfo -- IO because we need to make an md5_sum
noticerReqInfoFromDetails site board thread post attDetails = do
    hashes <- mapM computeMD5 (attDetails >>= selectAtFilePath)

    return SpamNoticerRequestInfo
        { attachments = zipWith ($) (attDetails >>= attachmentMetaFromDetails) hashes
        , body = Posts.body post
        , time_stamp = round $ utcTimeToPOSIXSeconds $ Posts.creation_time post
        , website_name = Sites.name site
        , board_name = Boards.pathpart board
        , thread_id = Threads.board_thread_id thread
        , skip_recent_record = True
        }

    where
        attachmentMetaFromDetails :: Lib.Details -> [ Text -> SpamNoticerAttachmentMetadata ]
        attachmentMetaFromDetails (_, _, _, _, Nothing) = []
        attachmentMetaFromDetails (_, _, _, _, Just (_, at)) =
            [ \md5 -> SpamNoticerAttachmentMetadata
                { filename = At.board_filename at
                , mimetype = At.mimetype at
                , md5_hash = md5
                }
            ]

        selectAtFilePath :: Lib.Details -> [ FilePath ]
        selectAtFilePath (_site, _board, _thread, _post, Just (paths, _attachment))
            = [ At.file_path paths ]
        selectAtFilePath _= []
