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
    } deriving (Eq)

instance Show BoardQueueElem where
    show :: BoardQueueElem -> String
    show b = "<BoardQueueElem modified: " ++ show (last_modified b) ++ "; " ++ show (site b) ++ "; " ++ show (board b) ++ ">"

instance Ord BoardQueueElem where
  (<=) :: BoardQueueElem -> BoardQueueElem -> Bool
  a <= b = last_modified a <= last_modified b
