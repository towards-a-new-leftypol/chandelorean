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
import Control.Monad.IO.Class (liftIO)
import UnliftIO.Async (pooledMapConcurrentlyN)

import qualified CliSettings as S
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
import qualified Network.Api.JSONPost as JSONPost
import qualified Common.PostsType as Post
import qualified Common.AttachmentType as At
import qualified ClientAPI as API
import Clients.LainJSONClient (lainJSONClient)
import Clients.TinyboardHTML (tinyboardHTMLClient)
import qualified Network.SpamNoticer as SN

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
threadMain csmr_settings boardElem = do
    putStrLn $ Board.pathpart $ QE.board boardElem

    thread_results <- runExceptT $ do
        let
            site = QE.site boardElem
            board = QE.board boardElem
            settings = mkJsonSettings csmr_settings site
            board_last_modified = QE.last_modified boardElem
            api = chooseApi $ QE.client_api_type boardElem

        (API.ChangedThreadsResult changedApiThreads allCatalogApiThreads) <-
            API.getChangedThreads api boardElem

        last_modified <- if null changedApiThreads
        then
            return board_last_modified
        else do
            let changedThreads = map
                        (Lib.apiThreadToArchiveThread $ Board.board_id board)
                        changedApiThreads

            apiPosts :: [ (Thread.Thread, [ JSONPost.Post ]) ] <-
                    API.getWebPosts api boardElem changedThreads
        
            -- -- changed plus new threads, so all the ones we need to fetch posts for
            -- threads <- Lib2.saveNewThreads settings board changedApiThreads

            -- web_posts :: [ (Thread.Thread, [ JSONPost.Post ]) ] <-
            --         API.getWebPosts api boardElem threads

            -- posts <- Lib2.saveNewPosts settings web_posts

            -- let web_post_tuples
            --         :: [ (Site.Site, Board.Board, Thread.Thread, JSONPost.Post) ]
            --         = concatMap
            --             (\(t, ps) -> map (\p -> (site, board, t, p)) ps)
            --             web_posts

            -- let post_tuples = Lib.addPostsToTuples web_post_tuples posts

            -- at this point the Post.thread_id is undefined, because it's undefined
            -- in the thread because apiThreadToArchiveThread set its to undefined.
            let changedThreadPosts =
                    [ (t, map (\x -> (x, Lib.apiPostToArchivePost undefined t x)) jps)
                    | (t, jps) <- apiPosts
                    ] :: [ (Thread.Thread, [ (JSONPost.Post, Post.Post) ]) ]

            -- TODO:
            -- - use liftHttpIO Client.getPostIdsByBoardIds to test which changedThreadPosts
            --   are in the db ✓
            -- - use Lib2.downloadAttachment to get all the missing attachments ✓
            --      - need to figure out whether or not to use liftHttpIO here, the old code doesn't do this, it seems to try and get as many as possible
            -- - create http client for SpamNoticer based on the php one ✓
            -- - filter the list of posts using SpamNoticer ✓
            -- - insert (with header Prefer: resolution=ignore-duplicates) all the threads into db
            --      - the threads that already exist won't be echoed back, so we won't have their ids
            --      - so need to query threads
            -- - insert all the posts into the db
            -- - insert all the attachment metadata into the db
            -- - it looks like the old code

            existingBoardPostIds <- Lib2.liftHttpIO $
                Client.getPostIdsByBoardIds
                    settings
                    (Board.board_id board)
                    [ Post.board_post_id p
                    | (_, xs) <- changedThreadPosts
                    , (_, p) <- xs
                    ]

            let
                existingBoardPostIdSet = Set.fromList existingBoardPostIds
                missingPostsDetails =
                    [ d
                    | (t, xs) <- changedThreadPosts
                    , (jp, p) <- xs
                    , Set.notMember (Post.board_post_id p) existingBoardPostIdSet
                    , d <- Lib.parseAttachments (JS.site_url settings) (site, board, t, jp, p)
                    ] :: [ Lib.Details ]

            downloadedMissingPosts :: [ Lib.Details ] <- mapM
                (Lib2.liftHttpIO . Lib2.downloadAttachment)
                missingPostsDetails

            let
                mNoticerSettings = S.spam_noticer csmr_settings
                postsPerThread = Lib2.groupDetails downloadedMissingPosts

            cleanPostsPerThread <- case mNoticerSettings of
                Nothing -> return postsPerThread
                Just noticerSettings -> do

                    noticerRequestInfos <- liftIO $ mapM
                          ( \(a, b, c, d, e) ->
                              SN.noticerReqInfoFromDetails a b c d e
                          )
                          [ (site, board, t, post, detailsList)
                          | (t, xs) <- postsPerThread
                          , (post, detailsList) <- xs
                          ]

                    let noticerArgs = zip noticerRequestInfos
                            [ i >>=
                                (\(_, _, _, _, x) ->
                                        case x of
                                            Nothing -> []
                                            Just (p, _) -> [ At.file_path p ]
                                )
                            | (_, xs) <- postsPerThread
                            , (_, i) <- xs
                            ]

                    let noticerJobs = S.max_concurrent_requests noticerSettings

                    noticerResponses <- Lib2.liftHttpIO $ sequence <$> pooledMapConcurrentlyN
                        noticerJobs
                        (uncurry (SN.askNoticer noticerSettings))
                        noticerArgs

                    liftIO $ mapM_ SN.logNoticerNoticed noticerResponses

                    let detailsWithSNResponses = zip
                            [ i
                            | (_, xs) <- postsPerThread
                            , (_, i) <- xs
                            ]
                            noticerResponses

                    liftIO $ mapM_ Lib2.unlinkAttachmentFiles
                        [ i
                        | (i, j) <- detailsWithSNResponses
                        , SN.noticed j
                        ]

                    return $ Lib2.groupDetails $
                        ( map fst $
                            filter
                                (\(_, noticerResp) ->
                                    SN.noticed noticerResp == False
                                )
                                detailsWithSNResponses
                        ) >>= id


            -- save new posts

            let changedThreads_ = map fst cleanPostsPerThread

            newThreads <- Lib2.liftHttpIO $
                Client.postThreads settings changedThreads_

            let existingThreads_ = (Set.fromList changedThreads_)
                    `Set.difference` (Set.fromList newThreads)


            -- query existing threads to get their thread_ids to be able
            -- to save posts

            existingThreads <- Lib2.liftHttpIO $
                Client.getThreads settings (Board.board_id board) $ Set.toList $
                    Set.map Thread.board_thread_id existingThreads_

            let threadThreadMap = Map.fromList
                    [ (i, i) | i <- newThreads ++ existingThreads ]

            -- At this point thread_id is still undefined for Thread and Post
            let
                cleanPPTWithThreadIds = map
                    ( \(t, xs) ->
                        let t_ = (Map.!) threadThreadMap t
                        in
                            ( t_
                            , [ (p { Post.thread_id = Thread.thread_id t_ }, ds)
                              | (p, ds) <- xs
                              ]
                            )
                    )
                    cleanPostsPerThread

                postsToSave =
                    [ x
                    | (_, xs) <- cleanPPTWithThreadIds
                    , x <- xs
                    ]

            newPosts <- Lib2.liftHttpIO $
                Client.postPosts settings (map fst postsToSave)

            let existingPostIds = Set.fromList (map (Client.idFromPost . fst) postsToSave)
                    `Set.difference` Set.fromList (map Client.idFromPost newPosts)

            existingPosts <- Lib2.liftHttpIO $
                Client.getPosts settings $ Set.toList existingPostIds

            -- result is the most recent timestamp of all the posts we just saved
            return $ foldr max board_last_modified $ map Post.creation_time
                [ post
                | (_, xs) <- postsPerThread
                , (post, _) <- xs
                ]

        Lib2.removeDeletedThreads settings boardElem allCatalogApiThreads
        return (last_modified, Just allCatalogApiThreads)

    case thread_results of
        Left err -> do
            putStrLn $ "Thread error occurred while processing " ++ show boardElem
            print err
            return boardElem
        Right (max_t, current_catalog) ->
            return boardElem { QE.last_modified = max_t, QE.last_catalog = current_catalog }


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

    site_and_board_and_api_list_ <- mapM
        (\site_settings -> do
            let site_name = S.name site_settings

            putStrLn $ "site_name_to_site map: " ++ (show site_name_to_site) ++ " key: " ++ site_name
            putStrLn $ "member? " ++ show (Map.member site_name site_name_to_site)

            let site = (Map.!) site_name_to_site site_name

            putStrLn $ "Site OK: " ++ show site

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
                    (Map.findWithDefault [] s_id boards_per_site)
                    s_id

            return (site, existing_boards ++ boards, S.client_api_type site_settings)

        )
        (S.websites csmr_settings)

    let site_and_board_and_api_list =
            concatMap
                ( \(a, bs, api) ->
                    map
                        ( \b -> (a, b, api)
                        )
                        bs
                )
                site_and_board_and_api_list_

    let queue_elems =
            map
                (\(site, board, api) -> QE.BoardQueueElem
                    { site = site
                    , board = board
                    , last_modified =
                        Map.findWithDefault
                            (Lib.epochToUTCTime 0)
                            (Board.board_id board)
                            board_id_to_last_modified
                    , last_catalog = Nothing
                    , client_api_type = api
                    }
                )
                site_and_board_and_api_list

    let pq :: PQ.Queue QE.BoardQueueElem = Set.fromList queue_elems

    mainLoop csmr_settings pq


chooseApi :: S.ClientApiType -> API.ClientAPI
chooseApi S.LainJSON = lainJSONClient
chooseApi S.TinyboardHTML = tinyboardHTMLClient
