module JSONPost
  ( Post (..)
  , PostWrapper (..)
  ) where

import Data.Text (Text)
import Data.Aeson (FromJSON)
import GHC.Generics
import qualified JSONCommonTypes as J

data Post = Post
    { no             :: Int
    , com            :: Maybe Text
    , name           :: Maybe Text
    , time           :: Int
    , omitted_posts  :: Maybe Int
    , omitted_images :: Maybe Int
    , sticky         :: Maybe Int
    , locked         :: Maybe Int
    , cyclical       :: Maybe Int
    , last_modified  :: Int
    , board          :: String
    , files          :: Maybe [J.File]
    , resto          :: Int
    , unique_ips     :: Maybe Int
    } deriving (Show, Generic)

instance FromJSON Post

data PostWrapper = PostWrapper
    { posts :: [Post]
    } deriving (Show, Generic)

instance FromJSON PostWrapper
