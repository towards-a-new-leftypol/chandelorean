{-# LANGUAGE DeriveAnyClass #-}
module AttachmentType
( Attachment (..)
, Dimension (..)
, Paths (..)
) where

import GHC.Generics
import Data.Int (Int64)
import Data.Aeson (FromJSON, ToJSON)
import Data.Text (Text)
import Data.Time.Clock (UTCTime)

data Dimension = Dimension
  { width  :: Int
  , height :: Int
  } deriving (Show, Generic, FromJSON, ToJSON)

data Paths = Paths
  { file_path :: Text
  , thumbnail_path :: Text
  }

data Attachment = Attachment
    { attachment_id   :: Maybe Int64
    , mimetype        :: Text
    , creation_time   :: UTCTime
    , sha256_hash     :: Text
    , phash           :: Int64
    , illegal         :: Bool
    , post_id         :: Int64
    , resolution      :: Maybe Dimension
    } deriving (Show, Generic, FromJSON, ToJSON)
