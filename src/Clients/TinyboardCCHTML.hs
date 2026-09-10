module Clients.TinyboardHTML
    ( tinyboardHTMLClient ) where

import qualified Data.ByteString.Lazy as LBS
import System.FilePath ((</>))

import ClientAPI
import qualified BoardQueueElem as QE
import Lib2 (IOe, httpGet)
import SitesType (Site)
import qualified BoardsType as B
import Network.Api.JSONParsing (Thread)

tinyboardHTMLClient :: ClientAPI
tinyboardHTMLClient = ClientAPI
    { getChangedThreads = f
    , getWebPosts = undefined
    }

f :: QE.BoardQueueElem -> IOe ChangedThreadsResult
f board_elem@QE.BoardQueueElem { QE.last_catalog = Nothing } = undefined
f board_elem = do
    let
        site = QE.site board_elem
        board = QE.board board_elem

    _ <- httpGetCatalogHTML site board
    undefined


httpGetCatalogHTML
  :: Site
  -> B.Board
  -> IOe [ Thread ]
httpGetCatalogHTML site board = parseCatalogPageHTML <$> httpGet site path
    where
        path = B.pathpart board </> "catalog"


parseCatalogPageHTML :: LBS.ByteString -> [ Thread ]
parseCatalogPageHTML = undefined
