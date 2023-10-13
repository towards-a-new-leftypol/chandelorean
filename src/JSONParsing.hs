module JSONParsing
    ( Thread (..)
    , Catalog (..)
    , parseJSONCatalog
    , parsePosts
    ) where

{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

import Data.Text (Text)
import GHC.Generics
import qualified Data.ByteString.Lazy as B
import Data.Aeson

import qualified JSONPost as Post
import qualified JSONCommonTypes as J

data Thread = Thread
  { no            :: Int
  , sub           :: Maybe Text
  , com           :: Maybe Text
  , name          :: Maybe Text
  , capcode       :: Maybe Text
  , time          :: Int
  , omitted_posts :: Maybe Int
  , omitted_images:: Maybe Int
  , replies       :: Maybe Int
  , images        :: Maybe Int
  , sticky        :: Maybe Int
  , locked        :: Maybe Int
  , cyclical      :: Maybe J.Cyclical
  , last_modified :: Int
  , board         :: Text
  , files         :: Maybe [J.File]
  , resto         :: Int
  , unique_ips    :: Maybe Int
  } deriving (Show, Generic)

instance FromJSON Thread
--instance ToJSON Thread

data Catalog = Catalog
  { threads :: [Thread]
  , page    :: Int
  } deriving (Show, Generic)

instance FromJSON Catalog
--instance ToJSON Catalog

parseJSONCatalog :: FilePath -> IO (Either String [Catalog])
parseJSONCatalog path = B.readFile path >>= return . eitherDecode

parsePosts :: FilePath -> IO (Either String Post.PostWrapper)
parsePosts path = B.readFile path >>= return . eitherDecode
