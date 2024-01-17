{-# LANGUAGE DeriveAnyClass #-}
module AttachmentType
( Attachment (..)
) where

import GHC.Generics
import Data.Int (Int64)
import Data.Aeson (FromJSON, ToJSON)
import Data.Text (Text)
import Data.Time.Clock (UTCTime)

data Attachment = Attachment
    { attachment_id   :: Maybe Int64
    , mimetype        :: Text
    , creation_time   :: UTCTime
    , sha256_hash     :: Text
    , phash           :: Int64
    , illegal         :: Bool
    , post_id         :: Int64
    } deriving (Show, Generic, FromJSON, ToJSON)
