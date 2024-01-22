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
  { file_path :: FilePath
  , thumbnail_path :: FilePath
  } deriving (Show)

data Attachment = Attachment
    { mimetype          :: Text
    , creation_time     :: UTCTime
    , sha256_hash       :: Text
    , phash             :: Maybe Int64
    , illegal           :: Bool
    , post_id           :: Int64
    , resolution        :: Maybe Dimension
    , file_extension    :: Maybe Text
    , original_filename :: Maybe Text
    , file_size_bytes   :: Int
    } deriving (Show, Generic, FromJSON, ToJSON)
