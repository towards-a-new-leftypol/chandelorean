{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module Network.SpamNoticer where

import GHC.Generics
import Data.Aeson (ToJSON, FromJSON, Value)
import Data.Text (Text)
import Data.Time.Clock (UTCTime)
import Data.Int (Int64)
import Network.HTTP.Simple
    ( setRequestHeader
    , setRequestMethod
    , parseRequest
    , httpLBS
    )

import Common.Network.HttpClient (HttpError, handleHttp)
import Network.HTTP.Client.MultipartFormData (formDataBody)
import Network.DataClient (eitherDecodeResponse)

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
        , time_stamp :: UTCTime
        , website_name :: String
        , board_name :: String
        , thread_id :: Int64
        , skip_recent_record :: Bool
        } deriving (Show, Generic, ToJSON, FromJSON)

data SpamNoticerResponse =
    SpamNoticerResponse
        { noticed :: Bool
        , reason :: Integer
        , details :: Value
        , debug_transaction_log :: Value
        } deriving (Show, Generic, ToJSON, FromJSON)

data SpamNoticerSettings =
    SpamNoticerSettings
        { base_url :: String
        } deriving (Show, Generic, ToJSON, FromJSON)

askNoticer
    :: SpamNoticerSettings
    -> SpamNoticerRequestInfo
    -> IO (Either HttpError SpamNoticerResponse)
askNoticer settings requestInfo = do
    req <- parseRequest url

    let httpRequest = setRequestMethod "POST"
            . setRequestHeader "Content-Type" [ "application/json" ]
            $ req

    request <- formDataBody [] httpRequest

    putStrLn $ "POSTing SpamNoticer query to" ++ url

    eitherDecodeResponse <$> handleHttp (httpLBS request)

    where
        url = base_url settings ++ path

        path = "/"
