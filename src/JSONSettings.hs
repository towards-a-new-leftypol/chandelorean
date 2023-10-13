module JSONSettings
  ( JSONSettings(..)
  ) where

import GHC.Generics
import Data.Aeson (FromJSON)

data JSONSettings = JSONSettings
  { postgrest_url :: String
  , jwt :: String
  , backup_read_root :: FilePath
  , site_name :: String
  , site_url :: String
  } deriving (Show, Generic)

instance FromJSON JSONSettings
