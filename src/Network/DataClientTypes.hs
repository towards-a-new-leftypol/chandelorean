{-# LANGUAGE DeriveAnyClass #-}

module Network.DataClientTypes where

import Data.Int (Int64)
import Data.Aeson (FromJSON)
import GHC.Generics

data ThreadMaxIdx = ThreadMaxIdx
    { thread_id :: Int64
    , max_idx   :: Int
    } deriving (Show, Generic, FromJSON)

