{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use tuple-section" #-}
{-# HLINT ignore "Fuse foldr/map" #-}

module Sync where

import System.Exit (exitFailure)
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Maybe (mapMaybe, fromMaybe)
import Control.Concurrent.QSem
import Control.Concurrent.STM.TVar
import Control.Concurrent.STM (atomically, retry)
import Control.Concurrent (threadDelay, forkFinally)
import System.Random (StdGen, getStdGen)
import Control.Monad.Trans.Except (runExceptT)

import qualified Common.Server.ConsumerSettings as S
import qualified Common.Server.JSONSettings as JS
import qualified Network.DataClient as Client
import qualified Lib
import qualified Network.GetLatestPostsPerBoardResponse as GLPPBR
import qualified SitesType as Site
import qualified BoardsType as Board
import qualified ThreadType as Thread
import qualified BoardQueueElem as QE
import qualified PriorityQueue as PQ
import qualified Lib2
import qualified JSONParsing as JS
import qualified JSONPost
import qualified Common.PostsType as Post


consumerSettingsToPartialJSONSettings :: S.ConsumerJSONSettings -> JS.JSONSettings
consumerSettingsToPartialJSONSettings S.ConsumerJSONSettings {..} =
    JS.JSONSettings
        { postgrest_url = postgrest_url
        , jwt = jwt
        , backup_read_root = undefined
        , media_root_path
        , site_name = undefined
        , site_url = undefined
        }


mkJsonSettings :: S.ConsumerJSONSettings -> Site.Site -> JS.JSONSettings
mkJsonSettings cs site = (consumerSettingsToPartialJSONSettings cs)
    { JS.site_name = Site.name site
    , JS.site_url = Site.url site
    }


threadMain :: S.ConsumerJSONSettings -> QE.BoardQueueElem -> IO QE.BoardQueueElem
threadMain csmr_settings board_elem = do
    putStrLn $ Board.pathpart $ QE.board board_elem

    -- this is essentially the same as Lib.processBoard
    -- but uses ExceptT instead of IO, which saves us from writing all
    -- of the error handling cases every time we make an http call. That can be done
    -- once at the end.
    thread_results <- runExceptT $ do
        let site = QE.site board_elem
        let board = QE.board board_elem

        catalog_results <- Lib2.httpGetCatalogJSON site board

        let catalog_threads = concatMap (fromMaybe [] . JS.threads) catalog_results

        let board_last_modified = QE.last_modified board_elem

        let changed_threads = filter
                (\t -> Lib.epochToUTCTime (JS.last_modified t) > board_last_modified)
                catalog_threads

        if null changed_threads
        then
            return board_last_modified
        else do
            let settings = mkJsonSettings csmr_settings site

            threads <- Lib2.saveNewThreads settings (QE.board board_elem) changed_threads

            web_posts :: [ (Thread.Thread, [ JSONPost.Post ]) ] <- mapM
                (Lib2.httpGetPostsJSON site board)
                threads

            posts <- Lib2.saveNewPosts settings web_posts

            let web_post_tuples
                    :: [ (Site.Site, Board.Board, Thread.Thread, JSONPost.Post) ]
                    = concatMap
                        (\(t, ps) -> map (\p -> (site, board, t, p)) ps)
                        web_posts

            let post_tuples = Lib.addPostsToTuples web_post_tuples posts

            Lib2.saveNewAttachments settings post_tuples

            -- here there needs to be a lock if we're also going to listen to
            -- events sent from the board. While we're doing multiple calls
            -- to the database here, another thread could have added more threads
            -- in which case they will be deleted here.
            Lib2.removeDeletedThreads settings site board (map Post.thread_id posts)


            -- So we also might want to build a service that http posts go to
            -- to signal new posts, and to also broadcast this out to everyone
            -- that connects.

            -- result is the most recent timestamp of all the posts we just saved
            return
                $ foldr max board_last_modified
                $ map Post.creation_time posts


    case thread_results of
        Left err -> do
            putStrLn $ "Thread error occurred while processing " ++ show board_elem
            print err
            return board_elem
        Right max_t ->
            return board_elem { QE.last_modified = max_t }


mainLoop :: S.ConsumerJSONSettings -> PQ.Queue QE.BoardQueueElem -> IO ()
mainLoop csmr_settings pq = do
    sem <- newQSem (S.sync_max_concurrent_workers csmr_settings)
    pqvar <- newTVarIO pq
    stdGen <- getStdGen

    loop sem stdGen pqvar

    where
        loop :: QSem -> StdGen -> TVar (PQ.Queue QE.BoardQueueElem) -> IO ()
        loop sem stdGen pqvar = do
            waitQSem sem -- make sure we don't have too many threads running

            -- select a board
            (board_elem, stdGen_) <- atomically $ do
                pq_a <- readTVar pqvar

                if Set.null pq_a
                then
                    retry
                else do
                    let (i, stdGen_) = PQ.selectSkewedIndex (Set.size pq) stdGen

                    let (board_elem, pq_b) = PQ.take i pq_a

                    writeTVar pqvar pq_b

                    return (board_elem, stdGen_)

            -- process the board...
            _ <- forkFinally (threadMain csmr_settings board_elem) $ \threadResult -> do
                board_elem_ <- case threadResult of
                    Left e -> print e >> return board_elem
                    Right a -> return a

                -- ...and put it back in the queue
                atomically $ modifyTVar' pqvar (PQ.put board_elem_)

                -- release the semaphore, to allow another thread to pick up a task
                signalQSem sem

            threadDelay (S.sync_loop_timeout_microseconds csmr_settings)

            loop sem stdGen_ pqvar


syncWebsites :: S.ConsumerJSONSettings -> IO ()
syncWebsites csmr_settings = do
    putStrLn "Starting channel web synchronization."

    let json_settings = consumerSettingsToPartialJSONSettings csmr_settings

    sitesResult <- Client.getAllSites json_settings

    sites <- mapM (flip Lib.ensureSiteExists sitesResult . Lib.toClientSettings csmr_settings) (S.websites csmr_settings)

    print sites

    -- initial query to populate boards
    latest_posts_per_board_results <- Client.getLatestPostsPerBoard json_settings

    latest_posts_per_board <- case latest_posts_per_board_results of
        Left e -> do
            putStrLn $ "Error getting board information: " ++ show e
            exitFailure
        Right latest_posts_per_board -> return latest_posts_per_board

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

    let board_id_to_last_modified = Map.fromList $
            map
                ( \b ->
                    ( GLPPBR.board_id b
                    -- set t = 0 if there are no posts on the board yet
                    -- this way it will check all of the threads
                    , fromMaybe (Lib.epochToUTCTime 0) $ GLPPBR.creation_time b
                    )
                )
                latest_posts_per_board

    let site_name_to_site :: Map.Map String Site.Site =
            Map.fromList $ map (\s -> (Site.name s, s)) sites

    let site_id_board_id_to_glppbr = Map.fromList $
            map
                (\b -> ((GLPPBR.site_id b, GLPPBR.pathpart b), b))
                latest_posts_per_board

    site_and_board_list_ <- mapM
        (\site_settings -> do
            let site = (Map.!) site_name_to_site (S.name site_settings)
            let s_id = Site.site_id site

            let existing_board_info =
                    mapMaybe
                        (\board_pathpart ->
                            Map.lookup (s_id, board_pathpart) site_id_board_id_to_glppbr
                        )
                        (S.boards site_settings)

            let existing_boards =
                    map
                        (\b -> Board.Board
                            { Board.board_id = GLPPBR.board_id b
                            , Board.name = Nothing
                            , Board.pathpart = GLPPBR.pathpart b
                            , Board.site_id = GLPPBR.site_id b
                            }
                        )
                        existing_board_info

            boards <- Lib.createArchivesForNewBoards
                    (Lib.toClientSettings csmr_settings site_settings)
                    (Set.fromList $ S.boards site_settings)
                    ((Map.!) boards_per_site s_id)
                    s_id

            return (site, existing_boards ++ boards)

        )
        (S.websites csmr_settings)

    let site_and_board_list = concatMap (\(a, bs) -> map (\b -> (a, b)) bs) site_and_board_list_

    let queue_elems =
            map
                (\(site, board) -> QE.BoardQueueElem
                    { QE.site = site
                    , QE.board = board
                    , QE.last_modified =
                        (Map.!)
                            board_id_to_last_modified
                            (Board.board_id board)
                    }
                )
                site_and_board_list

    let pq :: PQ.Queue QE.BoardQueueElem = Set.fromList queue_elems

    mainLoop csmr_settings pq
