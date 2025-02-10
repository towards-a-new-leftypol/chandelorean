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
import JSONParsing (Catalog, Thread)
import qualified ThreadType as Threads
import Common.Server.JSONSettings (JSONSettings)
import qualified Lib

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


ensureThreads
    :: JSONSettings
    -> Boards.Board
    -> [ Thread ]
    -> ExceptT ProgramException IO [ Threads.Thread ]
ensureThreads settings web_threads = do
    db_threads <- Client.getThreads settings (Boards.board_id board) (map no web_threads)

    -- save new threads
    Client.postThreads
        settings
        (map (Lib.apiThreadToArchiveThread board_id) threads_to_create)
