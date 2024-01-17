{-# LANGUAGE DeriveAnyClass #-}

module ThreadType
    ( Thread (..) )
    where

import GHC.Generics
import Data.Aeson (FromJSON)
import Data.Time.Clock (UTCTime) -- Required for timestamp with time zone
import Data.Int (Int64)

data Thread = Thread
    { thread_id       :: Int64
    , board_thread_id :: Int
    , creation_time   :: UTCTime
    , board_id        :: Int
    } deriving (Show, Generic, FromJSON)
