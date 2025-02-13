module Lib2
  ( httpGetCatalogJSON
  , ProgramException (..)
  , saveNewThreads
  , httpGetPostsJSON
  ) where

import Control.Monad.Trans.Except (ExceptT (..))
import System.FilePath ((</>))
import qualified Data.Set as Set
import Data.Aeson (FromJSON)

import qualified Network.DataClient as Client
import qualified SitesType  as Sites
import qualified BoardsType as Boards
import Common.Network.HttpClient (HttpError)
import qualified JSONParsing as JSON
import qualified JSONPost   as JSONPosts
import qualified ThreadType as Threads
import Common.Server.JSONSettings (JSONSettings)
import qualified Lib


data ProgramException = HttpException HttpError
  deriving Show


liftHttpIO :: IO (Either HttpError a) -> ExceptT ProgramException IO a
liftHttpIO = ExceptT . fmap (either (Left . HttpException) Right)


httpSiteGetRequest :: (FromJSON a) => Sites.Site -> String -> IO (Either HttpError a)
httpSiteGetRequest site path = Client.getJSON $ Sites.url site </> path

httpGetCatalogJSON
  :: Sites.Site
  -> Boards.Board
  -> ExceptT ProgramException IO [ JSON.Catalog ]
httpGetCatalogJSON site board = liftHttpIO $ httpSiteGetRequest site path
  where
    path = Boards.pathpart board </> "catalog.json"


httpGetPostsJSON
  :: Sites.Site
  -> Boards.Board
  -> Threads.Thread
  -> ExceptT ProgramException IO (Threads.Thread, [ JSONPosts.Post ])
httpGetPostsJSON site board thread =
    liftHttpIO $
        fmap ((thread,) . JSONPosts.posts) <$> httpSiteGetRequest site path

    where
        path = Boards.pathpart board
            </> "res"
            </> (show (Threads.board_thread_id thread) ++ ".json")


saveNewThreads
    :: JSONSettings
    -> Boards.Board
    -> [ JSON.Thread ]
    -> ExceptT ProgramException IO [ Threads.Thread ]
saveNewThreads settings board web_threads = do
    existing_threads <- liftHttpIO $
        Client.getThreads
            settings
            (Boards.board_id board)
            (map JSON.no web_threads)

    let
        archived_board_thread_ids :: Set.Set Int
        archived_board_thread_ids =
            Set.fromList $ map Threads.board_thread_id existing_threads

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

    return $ existing_threads ++ new_threads
