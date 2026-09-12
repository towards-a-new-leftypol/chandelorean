module Clients.TinyboardCCHTML
    ( tinyboardHTMLClient ) where

import System.FilePath ((</>))
import Data.Time.Clock (getCurrentTime)
import Control.Monad.IO.Class (liftIO)
import Data.Text.Lazy.Encoding (decodeUtf8)
import Data.Text.Lazy (toStrict)
import qualified Data.Map as Map
import Data.Int (Int64)

import ClientAPI
import qualified BoardQueueElem as QE
import Lib2 (IOe, httpGet)
import qualified BoardsType as B
import qualified Network.Api.JSONParsing as T
import Network.Api.JSONPost (Post)
import Parsing.TinyboardCCPostHtmlParser
import Parsing.TinyboardCCThreadHtmlParser
import qualified Lib
import qualified ThreadType as Thread
import Common.Network.SiteType (Site)

tinyboardHTMLClient :: ClientAPI
tinyboardHTMLClient = ClientAPI
    { getChangedThreads = f
    , getWebPosts = g
    }


f :: QE.BoardQueueElem -> IOe ChangedThreadsResult
f boardQe = do
    let
        site = QE.site boardQe
        board = QE.board boardQe
        boardLastModifiedTime = QE.last_modified boardQe

    currentCatalog <- httpGetCatalogHTML site board

    return $ case QE.last_catalog boardQe of
        Nothing -> ChangedThreadsResult
            { changedThreads =
                    filter
                        (\t -> Lib.epochToUTCTime (T.last_modified t) > boardLastModifiedTime)
                        currentCatalog
            , catalogThreads = currentCatalog
            }
        Just previousThreads ->
            let
                prev = Map.fromList
                    [ (compareKey t, t) | t <- previousThreads ]
                current = Map.fromList
                    [ (compareKey t, t) | t <- currentCatalog ]
            in ChangedThreadsResult
                { changedThreads =
                        map snd (Map.toList (current `Map.difference` prev))
                , catalogThreads = currentCatalog
                }

    where
        compareKey :: T.Thread -> (Int64, Int, Maybe Int)
        compareKey t = (T.no t, T.time t, T.replies t)
                


httpGetCatalogHTML
  :: Site
  -> B.Board
  -> IOe [ T.Thread ]
httpGetCatalogHTML site board = do
    now <- liftIO getCurrentTime
    pageBS <- httpGet site path
    return $ processCatalogPage now $ toStrict $ decodeUtf8 pageBS

    where
        path = B.pathpart board </> "catalog"


g
    :: QE.BoardQueueElem
    -> [ Thread.Thread ]
    -> IOe [ (Thread.Thread, [ Post ]) ]
g boardQe = mapM getPosts
    where
        site = QE.site boardQe
        board = QE.board boardQe

        getPosts :: Thread.Thread -> IOe (Thread.Thread, [ Post ])
        getPosts thread = do
            pageBS <- httpGet site path
            return $ (thread, processThreadPage $ toStrict $ decodeUtf8 pageBS)

            where
                path = B.pathpart board
                    </> "res" </> (show $ Thread.board_thread_id thread)
                    <> ".html"
