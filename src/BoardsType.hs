{-# LANGUAGE DeriveAnyClass #-}

module BoardsType
    ( Board (..) )
    where

import GHC.Generics
import Data.Aeson (FromJSON)

data Board = Board
    { board_id  :: Int
    , name      :: Maybe String
    , pathpart  :: String
    , site_id   :: Int
    } deriving (Show, Generic, FromJSON)

