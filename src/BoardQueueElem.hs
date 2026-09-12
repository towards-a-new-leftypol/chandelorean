module BoardQueueElem where

import Data.Time.Clock (UTCTime)

import BoardsType (Board)
import Network.Api.JSONParsing (Thread)
import CliSettings (ClientApiType)
import Common.Network.SiteType (Site)

data BoardQueueElem = BoardQueueElem
    { site :: Site
    , board :: Board
    , last_modified :: UTCTime
    , last_catalog :: Maybe [ Thread ]
    , client_api_type :: ClientApiType
    } deriving Eq

instance Show BoardQueueElem where
    show :: BoardQueueElem -> String
    show b = "<BoardQueueElem modified: " ++ show (last_modified b) ++ "; " ++ show (site b) ++ "; " ++ show (board b) ++ ">"

instance Ord BoardQueueElem where
  (<=) :: BoardQueueElem -> BoardQueueElem -> Bool
  a <= b = last_modified a <= last_modified b
