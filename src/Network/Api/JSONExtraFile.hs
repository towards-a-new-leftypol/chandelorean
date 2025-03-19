module Network.Api.JSONExtraFile where

import Data.Text (Text)
import Data.Aeson (FromJSON)
import GHC.Generics

data ExtraFile = ExtraFile
    { h        :: Maybe Int
    , w        :: Maybe Int
    -- , tn_h :: Maybe Int
    -- , tn_w :: Maybe Int
    , fsize    :: Int
    , filename :: Text
    , ext      :: Text
    , tim      :: Text
    -- , md5      :: Text
    } deriving (Show, Generic)

instance FromJSON ExtraFile
