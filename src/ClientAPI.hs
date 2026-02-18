module ClientAPI where

import Network.Api.JSONParsing (Thread)
import BoardQueueElem (BoardQueueElem)
import Lib2 (IOe)
import Network.Api.JSONPost (Post)
import qualified ThreadType as T

data ChangedThreadsResult =
    ChangedThreadsResult
        { changedThreads :: [ Thread ]
        , catalogThreads :: [ Thread ]
        }

data ClientAPI
    = ClientAPI
        { getChangedThreads :: BoardQueueElem -> IOe ChangedThreadsResult
        , getWebPosts :: BoardQueueElem -> [ T.Thread ] -> IOe [ (T.Thread, [ Post ]) ]
        }
