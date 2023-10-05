module JSONParsing
    ( Thread(..)
    , File(..)
    , Catalog(..)
    , parseJSONFile
    ) where

{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

import Data.Aeson
import GHC.Generics
import qualified Data.ByteString.Lazy as B
import qualified Data.Text as T
import Data.Aeson.Types (typeMismatch)

data Cyclical = Cyclical Int deriving (Show, Generic)

instance FromJSON Cyclical where
    parseJSON (Number n) = return $ Cyclical (floor n)
    parseJSON (String s) =
      case reads (T.unpack s) :: [(Int, String)] of
      [(n, "")] -> return $ Cyclical n
      _         -> typeMismatch "Int or String containing Int" (String s)

    parseJSON invalid    = typeMismatch "Int or String" invalid

data Thread = Thread
  { no            :: Int
  , sub           :: Maybe String
  , com           :: Maybe String
  , name          :: Maybe String
  , capcode       :: Maybe String
  , time          :: Int
  , omitted_posts :: Maybe Int
  , omitted_images:: Maybe Int
  , replies       :: Maybe Int
  , images        :: Maybe Int
  , sticky        :: Maybe Int
  , locked        :: Maybe Int
  , cyclical      :: Maybe Cyclical
  , last_modified :: Int
  , board         :: String
  , files         :: Maybe [File]
  , resto         :: Int
  , unique_ips    :: Maybe Int
  } deriving (Show, Generic)

data File = File
  { id         :: String
  , mime       :: String
  , ext        :: String
  , h          :: Maybe Int
  , w          :: Maybe Int
  , fsize      :: Int
  , filename   :: String
  , spoiler    :: Maybe Bool
  , md5        :: String
  , file_path  :: String
  , thumb_path :: String
  } deriving (Show, Generic)

data Catalog = Catalog
  { threads :: [Thread]
  , page    :: Int
  } deriving (Show, Generic)

instance FromJSON Thread
--instance ToJSON Thread
instance FromJSON File
--instance ToJSON File
instance FromJSON Catalog
--instance ToJSON Catalog


parseJSONFile :: FilePath -> IO (Either String [Catalog])
parseJSONFile path = do
    jsonData <- B.readFile path
    return $ eitherDecode jsonData
