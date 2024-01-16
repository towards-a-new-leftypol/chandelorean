{-# LANGUAGE DeriveAnyClass #-}
module AttachmentType
( Attachment (..)
) where

import GHC.Generics
import Data.Int (Int64)
import Data.Aeson (FromJSON)
import Data.Text (Text)

data Attachment = Attachment
    { attachment_id   :: Maybe Int64
    , mimetype        :: Text
    , creation_time   :: UTCTime
    , sha256_hash     :: Int
    , phash           :: Int64
    , phash           :: Bool
    , post_id         :: Int64
    } deriving (Show, Generic, FromJSON)
