module JSONPost
  ( Post (..)
  , PostWrapper (..)
  ) where

import Data.Text (Text)
import Data.Int (Int64)
import Data.Aeson (FromJSON)
import GHC.Generics
import qualified JSONCommonTypes as J

data Post = Post
    { no             :: Int64
    , com            :: Maybe Text
    , name           :: Maybe Text
    , sub            :: Maybe Text
    , email          :: Maybe Text
    , time           :: Int
    , omitted_posts  :: Maybe Int
    , omitted_images :: Maybe Int
    , sticky         :: Maybe Int
    , locked         :: Maybe Int
    , cyclical       :: Maybe J.Cyclical
    , last_modified  :: Int
    , embed          :: Maybe Text
    -- , board          :: Text
    , files          :: Maybe [J.File]
    , resto          :: Int
    , unique_ips     :: Maybe Int

    -- legacy attributes
    , filename       :: Maybe Text
    , h              :: Maybe Int
    , w              :: Maybe Int
    , ext            :: Maybe Text
    , tim            :: Maybe Text
    , fsize          :: Maybe Int
    , spoiler        :: Maybe Int
    } deriving (Show, Generic)

instance FromJSON Post

data PostWrapper = PostWrapper
    { posts :: [Post]
    } deriving (Show, Generic)

instance FromJSON PostWrapper
