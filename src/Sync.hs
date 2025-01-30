{-# LANGUAGE RecordWildCards #-}

module Sync where

import Common.Server.ConsumerSettings as Settings
import Common.Server.JSONSettings as JSONSettings
import Network.DataClient (getLatestPostsPerBoard)

consumerSettingsToPartialJSONSettings :: Settings.ConsumerJSONSettings -> JSONSettings.JSONSettings
consumerSettingsToPartialJSONSettings ConsumerJSONSettings {..} =
    JSONSettings
        { JSONSettings.postgrest_url = postgrest_url
        , JSONSettings.jwt = jwt
        , backup_read_root = undefined
        , JSONSettings.media_root_path
        , site_name = undefined
        , site_url = undefined
        }

syncWebsites :: ConsumerJSONSettings -> IO ()
syncWebsites consumer_settings = do
    putStrLn "Starting channel web synchronization."

    let json_settings = consumerSettingsToPartialJSONSettings consumer_settings

    asdf <- getLatestPostsPerBoard json_settings

    print asdf
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

