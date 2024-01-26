module JSONCommonTypes
  ( File (..)
  , Cyclical (..)
  ) where

import Data.Text (Text)
import qualified Data.Text as T
import Data.Aeson
import Data.Aeson.Types (typeMismatch)
import GHC.Generics

data Cyclical = Cyclical Int deriving (Show, Generic)

instance FromJSON Cyclical where
    parseJSON (Number n) = return $ Cyclical (floor n)
    parseJSON (String s) =
      case reads (T.unpack s) :: [(Int, String)] of
      [(n, "")] -> return $ Cyclical n
      _         -> typeMismatch "Int or String containing Int" (String s)

    parseJSON invalid    = typeMismatch "Int or String" invalid


data File = File
  { id         :: Text
  , mime       :: Maybe Text
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
