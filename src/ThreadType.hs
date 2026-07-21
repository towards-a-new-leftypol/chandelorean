{-# LANGUAGE DeriveAnyClass #-}

module ThreadType
    ( Thread (..) )
    where

import GHC.Generics
import Data.Aeson (FromJSON)
import Data.Time.Clock (UTCTime) -- Required for timestamp with time zone
import Data.Int (Int64)
import Data.Ord (comparing)

data Thread = Thread
    { thread_id       :: Int64
    , board_thread_id :: Int64
    , creation_time   :: UTCTime
    , board_id        :: Int
    } deriving (Show, Generic, FromJSON)

instance Eq Thread where
  x == y = tuple x == tuple y

    where
        tuple x = (board_id x, board_thread_id x, creation_time x)

instance Ord Thread where
  compare = comparing (\t -> (board_id t, board_thread_id t))
