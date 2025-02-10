module Lib2
  ( getCatalogJSON
  , ProgramException (..)
  , saveNewThreads
  ) where

import Control.Monad.Trans.Except (ExceptT (..))
import System.FilePath ((</>))
import qualified Data.Set as Set

import qualified Network.DataClient as Client
import qualified SitesType  as Sites
import qualified BoardsType as Boards
import Common.Network.HttpClient (HttpError)
import qualified JSONParsing as JSON
import qualified ThreadType as Threads
import Common.Server.JSONSettings (JSONSettings)
import qualified Lib


data ProgramException = HttpException HttpError
  deriving Show


liftHttpIO :: IO (Either HttpError a) -> ExceptT ProgramException IO a
liftHttpIO = ExceptT . fmap (either (Left . HttpException) Right)


-- over http
getCatalogJSON
  :: Sites.Site
  -> Boards.Board
  -> ExceptT ProgramException IO [ JSON.Catalog ]
getCatalogJSON site board = liftHttpIO req

  where
    path = Boards.pathpart board </> "catalog.json"
    req = Client.getJSON $ Sites.url site </> path


saveNewThreads
    :: JSONSettings
    -> Boards.Board
    -> [ JSON.Thread ]
    -> ExceptT ProgramException IO [ Threads.Thread ]
saveNewThreads settings board web_threads = do
    db_threads <- liftHttpIO $
        Client.getThreads
            settings
            (Boards.board_id board)
            (map JSON.no web_threads)

    let
        archived_board_thread_ids :: Set.Set Int
        archived_board_thread_ids =
            Set.fromList $ map Threads.board_thread_id db_threads

        threads_to_create :: [ JSON.Thread ]
        threads_to_create =
            filter
                ((`Set.notMember` archived_board_thread_ids) . JSON.no)
                web_threads

        board_id :: Int = Boards.board_id board

    -- save new threads
    new_threads <- liftHttpIO $ Client.postThreads
        settings
        (map (Lib.apiThreadToArchiveThread board_id) threads_to_create)

    return $ db_threads ++ new_threads
