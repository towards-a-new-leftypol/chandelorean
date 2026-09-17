module Clients.EscapedLainJSONClient
    ( escapedLainJSONClient)
    where

import Text.HTML.Parser.EntityDecode

import ClientAPI
import Clients.LainJSONClient
import qualified Network.Api.JSONPost as P
import ThreadType (Thread)
import Lib2 (IOe)

escapedLainJSONClient :: ClientAPI
escapedLainJSONClient = f lainJSONClient
    where
        f :: ClientAPI -> ClientAPI
        f api@ClientAPI { getWebPosts } =
            api { getWebPosts = (g .) . getWebPosts }

        g :: IOe [ (Thread, [ P.Post ]) ] -> IOe [ (Thread, [ P.Post ]) ]
        g xs = xs >>= return . (map $ \(t, ps) -> (t, map escapePostFields ps))

escapePostFields :: P.Post -> P.Post
escapePostFields p@P.Post { P.com, P.sub } =
    p
        { P.com = decodeEntities <$> com
        , P.sub = decodeEntities <$> sub
        }
