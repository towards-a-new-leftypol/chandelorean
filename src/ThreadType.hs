{-# LANGUAGE DeriveAnyClass #-}

module ThreadType
    ( Thread (..) )
    where

import GHC.Generics
import Data.Aeson (FromJSON)
import Data.Time.Clock (UTCTime) -- Required for timestamp with time zone

data Thread = Thread
    { thread_id       :: Int
    , board_thread_id :: Int
    , creation_time   :: UTCTime
    , board_id        :: Int
    } deriving (Show, Generic, FromJSON)
