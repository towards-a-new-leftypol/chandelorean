{-# LANGUAGE OverloadedStrings #-}

module Main where

import Lib (computeAttachmentHash)
import qualified Common.AttachmentType as At

badImageFp :: String = "/tmp/lpjson/1745404963219.jpg"

{-
data Paths = Paths
  { file_path :: FilePath
  , thumbnail_path :: Maybe FilePath
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
    , thumb_extension   :: Maybe Text
    , original_filename :: Maybe Text
    , board_filename    :: Text
    , spoiler           :: Bool
    , file_size_bytes   :: Int
    , attachment_idx    :: Int
    } deriving (Show, Generic, FromJSON, ToJSON, Eq)
-}

main :: IO ()
main = do
    putStrLn "Bitch"
    putStrLn badImageFp

    let path = At.Paths badImageFp undefined

    let attachment = At.Attachment { At.mimetype = "image/jpeg" }

    let details = (undefined, undefined, undefined, undefined, path, attachment)

    a <- computeAttachmentHash details
    print a

    putStrLn "Fin"
