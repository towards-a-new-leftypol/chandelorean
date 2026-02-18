module Clients.LainJSONClient where

import Data.Maybe (fromMaybe)

import ClientAPI
import qualified Network.Api.JSONParsing as Thread
import qualified BoardQueueElem as QE
import Lib2 (httpGetCatalogJSON, httpGetPostsJSON, IOe)
import Lib (epochToUTCTime)
import Network.Api.JSONPost (Post)
import ThreadType (Thread)

lainJSONClient :: ClientAPI
lainJSONClient = ClientAPI
    { getChangedThreads = f
    , getWebPosts = g
    }

    where
        f :: QE.BoardQueueElem -> IOe ChangedThreadsResult
        f board_elem = do
            let
                site = QE.site board_elem
                board = QE.board board_elem

            catalog_results <- Lib2.httpGetCatalogJSON site board

            let catalog_threads =
                    concatMap
                        (fromMaybe [] . Thread.threads)
                        catalog_results

            -- on the first run, this value comes from the latest_posts_per_board_results call
            -- but then we should update it.
            let board_last_modified = QE.last_modified board_elem

            pure $ ChangedThreadsResult
                { changedThreads =
                    filter
                        (\t -> Lib.epochToUTCTime (Thread.last_modified t) > board_last_modified)
                        catalog_threads
                , catalogThreads = catalog_threads
                }

        g
            :: QE.BoardQueueElem
            -> [ Thread ]
            -> IOe [ (Thread, [ Post ]) ]
        g board_elem = mapM (httpGetPostsJSON site board)
            where
                site = QE.site board_elem
                board = QE.board board_elem
