{-# LANGUAGE DeriveAnyClass #-}

module PostsType
    ( Post (..) )
    where

import GHC.Generics
import Data.Aeson (FromJSON, ToJSON)
import Data.Time.Clock (UTCTime) -- Required for timestamp with time zone
import Data.Int (Int64)
import Data.Text (Text)

data Post = Post
    { post_id         :: Maybe Int64
    , board_post_id   :: Int64
    , creation_time   :: UTCTime
    , body            :: Maybe Text
    , name            :: Maybe Text
    , subject         :: Maybe Text
    , email           :: Maybe Text
    , thread_id       :: Int
    } deriving (Show, Generic, FromJSON, ToJSON)
