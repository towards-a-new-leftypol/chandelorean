module BoardQueueElem where

import Data.Time.Clock (UTCTime)

import SitesType (Site)
import BoardsType (Board)
import JSONParsing (Thread)

data BoardQueueElem = BoardQueueElem
    { site :: Site
    , board :: Board
    , last_modified :: UTCTime
    , last_catalog :: Maybe [ Thread ]
    } deriving (Show, Eq)

instance Ord BoardQueueElem where
  (<=) :: BoardQueueElem -> BoardQueueElem -> Bool
  a <= b = last_modified a <= last_modified b
