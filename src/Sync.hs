module Sync where

import Common.Server.ConsumerSettings

syncWebsites :: ConsumerJSONSettings -> IO ()
syncWebsites _ = do
    putStrLn "Starting channel web synchronization."

    -- first we need all the (Site, Board) tuples
    -- perhaps we even want all (Site, Board, Thread) pairs
    -- But then we don't load the posts of each thread, instead only do
    -- that for threads which change,
    --    - which means after we get all the threads
    --    - enter a loop where you
    --        - pick a board
    --        - compare the threads online to memory
    --        - load only the changed/new ones
    --        - put board back

