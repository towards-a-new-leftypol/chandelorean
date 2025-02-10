module Lib2
  ( getCatalogJSON
  , ProgramException (..)
  ) where

import Control.Monad.Trans.Except (ExceptT (..))
import System.FilePath ((</>))

import qualified Network.DataClient as Client
import qualified SitesType  as Sites
import qualified BoardsType as Boards
import Common.Network.HttpClient (HttpError)
import JSONParsing (Catalog)

data ProgramException = HttpException HttpError
  deriving Show

-- over http
getCatalogJSON
  :: Sites.Site
  -> Boards.Board
  -> ExceptT ProgramException IO [ Catalog ]
getCatalogJSON site board =
  ExceptT $ fmap (either (Left . HttpException) Right) req

  where
    path = Boards.pathpart board </> "catalog.json"
    req = Client.getJSON $ Sites.url site </> path
