{-# LANGUAGE DeriveAnyClass #-}

module Network.GetLatestPostsPerBoardResponse
where

-- import Data.Int (Int64)
import Data.Time.Clock (UTCTime)
import Data.Aeson (FromJSON)
import GHC.Generics

data GetLatestPostsPerBoardResponse = GetLatestPostsPerBoardResponse
    { board_id        :: Int
    , site_id         :: Int
    , pathpart        :: String
    -- , post_id         :: Maybe Int64
    -- , board_post_id   :: Int64
    , creation_time   :: Maybe UTCTime
    -- , thread_id       :: Int64
    -- , board_thread_id :: Integer
    } deriving (Show, Generic, FromJSON)

-- actually used:
--   site_id
--   pathpart
--   board_id
--   creation_time
