module Sync where

import Common.Server.ConsumerSettings
import Lib (getBoards, toClientSettings)
import SitesType (Site)
import BoardsType (Board)

getSiteBoards :: ConsumerJSONSettings -> JSONSiteSettings -> IO (Site, [ Board ])
getSiteBoards settings site_settings =
    let client_settings = toClientSettings settings site_settings
    in getBoards
        client_settings
        (boards site_settings)

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

