module ClientAPI where

import Network.Api.JSONParsing (Thread)
import BoardQueueElem (BoardQueueElem)
import Lib2 (IOe)

data ChangedThreadsResult =
    ChangedThreadsResult
        { changedThreads :: [ Thread ]
        , catalogThreads :: [ Thread ]
        }

data ClientAPI
    = ClientAPI
        { getChangedThreads :: BoardQueueElem -> IOe ChangedThreadsResult
        }
