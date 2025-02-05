{-# LANGUAGE RecordWildCards #-}

module Sync where

import System.Exit (exitFailure)
import qualified Data.Map as Map
import qualified Data.Set as Set
-- import Control.Monad.Trans.Except (ExceptT, runExceptT)

import qualified Common.Server.ConsumerSettings as S
import qualified Common.Server.JSONSettings as JS
import qualified Network.DataClient as Client
import qualified Lib
import qualified Network.GetLatestPostsPerBoardResponse as GLPPBR
import qualified SitesType as Site

consumerSettingsToPartialJSONSettings :: S.ConsumerJSONSettings -> JS.JSONSettings
consumerSettingsToPartialJSONSettings S.ConsumerJSONSettings {..} =
    JS.JSONSettings
        { JS.postgrest_url = postgrest_url
        , JS.jwt = jwt
        , backup_read_root = undefined
        , JS.media_root_path
        , site_name = undefined
        , site_url = undefined
        }

syncWebsites :: S.ConsumerJSONSettings -> IO ()
syncWebsites consumer_settings = do
    putStrLn "Starting channel web synchronization."

    let json_settings = consumerSettingsToPartialJSONSettings consumer_settings

    sitesResult <- Client.getAllSites json_settings

    sites <- mapM (flip Lib.ensureSiteExists sitesResult . Lib.toClientSettings consumer_settings) (S.websites consumer_settings)

    print sites

    latest_posts_per_board_results <- Client.getLatestPostsPerBoard json_settings

    case latest_posts_per_board_results of
        Left e -> do
            putStrLn $ "Error getting board information: " ++ show e
            exitFailure
        Right latest_posts_per_board -> do
            print latest_posts_per_board

            let boards_per_site :: Map.Map Int [ String ] =
                    foldl
                        (\m b ->
                            let key = GLPPBR.site_id b
                                pathpart = GLPPBR.pathpart b
                            in

                            Map.insertWith (++) key [ pathpart ] m
                        )
                        Map.empty
                        latest_posts_per_board

            let site_name_to_site_id :: Map.Map String Int = Map.fromList $ map (\s -> (Site.name s, Site.site_id s)) sites

            mapM_
                (\site_settings ->
                    let s_id = (Map.!) site_name_to_site_id (S.name site_settings) in

                    Lib.createArchivesForNewBoards
                        (Lib.toClientSettings consumer_settings site_settings)
                        (Set.fromList $ S.boards site_settings)
                        ((Map.!) boards_per_site s_id)
                        s_id
                )
                (S.websites consumer_settings)

    -- we have our boards last modified timestamps
    -- get list of boards per site

    -- first we need all the (Site, Board) tuples ✓
    -- perhaps we even want all (Site, Board, Thread) ✓
    -- But then we don't load the posts of each thread, instead only do
    -- that for threads which change,
    --    - which means after we get all the threads
    --    - enter a loop where you
    --        - pick a board
    --        - compare the threads online to memory
    --        - load only the changed/new ones
    --        - put board back


    -- NEW TODO:
    --  - ensure that sites in the settings exist in the database! ✓
    --  - ensure that boards per site in the settings exist in the database!
    --  - finish using ExceptT and use sites, latest_posts_per_board to populate
    --    our PriorityQueue
    --  - write event loop that
    --       - get pq from stm shared value
    --       - uses the pq (there was something about the timestamps in the pq having to be reversed btw)
    --       - ensures threads
    --       - has a value that should be added to the pq
    --       - uses stm to update pq shared value
