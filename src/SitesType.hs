{-# LANGUAGE DeriveAnyClass #-}

module SitesType
    ( Site (..) )
    where

import GHC.Generics
import Data.Aeson (FromJSON)

data Site = Site
    { site_id :: Int
    , name :: String
    , url :: String
    } deriving (Show, Generic, FromJSON)

