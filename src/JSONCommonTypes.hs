module JSONCommonTypes
  ( File (..)
  ) where

import Data.Text (Text)
import Data.Aeson (FromJSON)
import GHC.Generics

data File = File
  { id         :: Text
  , mime       :: Text
  , ext        :: Text
  , h          :: Maybe Int
  , w          :: Maybe Int
  , fsize      :: Int
  , filename   :: Text
  , spoiler    :: Maybe Bool
  , md5        :: Text
  , file_path  :: Text
  , thumb_path :: Text
  } deriving (Show, Generic)

instance FromJSON File
--instance ToJSON File
