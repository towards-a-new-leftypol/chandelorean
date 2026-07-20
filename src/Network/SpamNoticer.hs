{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module Network.SpamNoticer where

import GHC.Generics
import Data.Aeson (ToJSON, FromJSON, Value)
import Data.Text (Text)
import Data.Time.Clock (UTCTime)
import Data.Int (Int64)

data SpamNoticerAttachmentMetadata =
    SpamNoticerAttachmentMetadata
        { filename :: Text
        -- , thumbnail_url :: Maybe String
        , mimetype :: Text
        , md5_hash :: Text
        } deriving (Show, Generic, ToJSON, FromJSON)
    
data SpamNoticerRequest =
    SpamNoticerRequest
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

data SpamNoticerSettings = SpamNoticerSettings

askNoticer :: SpamNoticerSettings -> SpamNoticerRequest
askNoticer = undefined
