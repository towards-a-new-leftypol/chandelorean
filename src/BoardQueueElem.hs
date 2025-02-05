module BoardQueueElem where

import Data.Time.Clock (UTCTime)

import SitesType (Site)
import BoardsType (Board)

data BoardQueueElem = BoardQueueElem
    { site :: Site
    , board :: Board
    , last_modified :: UTCTime
    } deriving (Show, Eq)

instance Ord BoardQueueElem where
  (<=) :: BoardQueueElem -> BoardQueueElem -> Bool
  a <= b = last_modified a >= last_modified b
