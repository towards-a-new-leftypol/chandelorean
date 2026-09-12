{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use tuple-section" #-}
{-# HLINT ignore "Fuse foldr/map" #-}

module Sync where

import System.Exit (exitFailure, exitSuccess)
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Maybe (mapMaybe, fromMaybe, fromJust)
import Control.Concurrent.QSem
import Control.Concurrent.STM.TVar
import Control.Concurrent.STM (atomically, retry)
import Control.Concurrent (threadDelay, forkFinally)
import System.Random (StdGen, getStdGen)
import Control.Monad.Trans.Except (runExceptT)
import Control.Monad.IO.Class (liftIO)
import UnliftIO.Async (pooledMapConcurrentlyN)
import Data.Text (Text, pack, unpack)

import qualified CliSettings as S
import qualified Common.Server.JSONSettings as JS
import qualified Network.DataClient as Client
import qualified Lib
import qualified Network.GetLatestPostsPerBoardResponse as GLPPBR
import qualified BoardsType as Board
import qualified ThreadType as Thread
import qualified BoardQueueElem as QE
import qualified PriorityQueue as PQ
import qualified Lib2
import qualified Network.Api.JSONPost as JSONPost
import qualified Network.Api.JSONParsing as JSONThread
import qualified Common.PostsType as Post
import qualified Common.AttachmentType as At
import qualified ClientAPI as API
import Clients.LainJSONClient (lainJSONClient)
import Clients.TinyboardCCHTML (tinyboardHTMLClient)
import qualified Network.SpamNoticer as SN
import qualified Common.Network.SiteType as NSite

println :: String -> Lib2.IOe ()
-- println = const $ return ()
println = liftIO . putStrLn


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


mkJsonSettings :: S.ConsumerJSONSettings -> NSite.Site -> JS.JSONSettings
mkJsonSettings cs site = (consumerSettingsToPartialJSONSettings cs)
    { JS.site_name = unpack $ NSite.name site
    , JS.site_url = unpack $ NSite.url site
    }


threadMain :: S.ConsumerJSONSettings -> QE.BoardQueueElem -> IO QE.BoardQueueElem
threadMain csmr_settings boardElem = do
    putStrLn $ (Board.pathpart $ QE.board boardElem)
            ++ " last touched: " ++ show (QE.last_modified boardElem)

    thread_results <- runExceptT $ do
        let
            site = QE.site boardElem
            board = QE.board boardElem
            settings = mkJsonSettings csmr_settings site
            board_last_modified = QE.last_modified boardElem
            api = chooseApi $ QE.client_api_type boardElem

        (API.ChangedThreadsResult changedApiThreads allCatalogApiThreads) <-
            API.getChangedThreads api boardElem

        println "BEGIN"
        println $ "Number of changedApiThreads: " ++ (show $ length changedApiThreads)
        mapM_
            (\t -> println $ "board_thread_id: " ++ (show $ JSONThread.no t)
                ++ " last modified: " ++ (show $ Lib.epochToUTCTime $ JSONThread.last_modified t)
            )
            changedApiThreads

        last_modified <- if null changedApiThreads
        then do
            println $ "changedApiThreads is null returning last modified: " ++ show board_last_modified
            return board_last_modified
        else do
            println "HELLO A"

            let changedThreads = map
                        (Lib.apiThreadToArchiveThread $ Board.board_id board)
                        changedApiThreads

            apiPosts :: [ (Thread.Thread, [ JSONPost.Post ]) ] <-
                    API.getWebPosts api boardElem changedThreads

            println $ "getWebPosts result length: " ++ (show $ length apiPosts)
            println $ "number of JSONPosts in getWebPosts result: " ++ (show $ length (apiPosts >>= snd))

            println "HELLO B"

            existingBoardPostIds <- Lib2.liftHttpIO $
                Client.getPostIdsByBoardIds
                    settings
                    (Board.board_id board)
                    (map (Thread.board_thread_id . fst) apiPosts)
                    (apiPosts >>= (map JSONPost.no) . snd)

            println $ "PostId count from Client.getPostIdsByBoardIds results: " ++ (show $ length existingBoardPostIds)

            println "HELLO C"

            let existingThreadIds = Set.fromList $
                    map Client.thread_id existingBoardPostIds

            println $ "existingThreadIds size: " ++ (show $ Set.size existingThreadIds)


            maxLocalIdxMap <- Map.fromList <$> (
                Lib2.liftHttpIO $ Client.getThreadMaxLocalIdx
                    settings
                    (Set.toList existingThreadIds)
                )

            println $ "maxLocalIdxMap (Client.getThreadMaxLocalIdx result) size: " ++ (show $ Map.size maxLocalIdxMap)

            println "HELLO D"

            let boardTidTidMap = Lib2.figureOutBoardThreadIdToThreadIdMap
                    (Map.fromList [ (Thread.board_thread_id t, map JSONPost.no jps)
                    | (t, jps) <- apiPosts
                    ])
                    (Map.fromList [ (Client.board_post_id postId, Client.thread_id postId)
                    | postId <- existingBoardPostIds
                    ])

            println $ "boardTidTidMap size: " ++ (show $ Map.size boardTidTidMap)

            -- at this point the Post.thread_id is undefined, because it's undefined
            -- in the thread because apiThreadToArchiveThread set its to undefined.
            let
                changedThreadPosts =
                    [ let
                            threadId = Map.lookup (Thread.board_thread_id t) boardTidTidMap
                            (t_, idx) =
                                case threadId of
                                    Nothing -> (t, 0)
                                    Just tid ->
                                        ( t { Thread.thread_id = tid }
                                        , fromMaybe 0 $ Map.lookup tid maxLocalIdxMap
                                        )
                      in
                        ( t_
                        , map
                            (\(x, i) -> (x, Lib.apiPostToArchivePost i t_ x))
                            (zip jps [(idx + 1)..])
                        )
                    | (t, jps) <- apiPosts
                    ] :: [ (Thread.Thread, [ (JSONPost.Post, Post.Post) ]) ]

                postAlreadyExists t p =
                    case Map.lookup (Thread.board_thread_id t) boardTidTidMap of
                        Nothing -> False
                        Just tid ->
                            let p_ = p { Post.thread_id = tid }
                            in Set.member (Client.idFromPost p_) existingBoardPostIdSet

                existingBoardPostIdSet = Set.fromList existingBoardPostIds
                missingPostsDetails =
                    [ d
                    | (t, xs) <- changedThreadPosts
                    , (jp, p) <- xs
                    , not (postAlreadyExists t p)
                    , d <- Lib.parseAttachments (JS.site_url settings) (site, board, t, jp, p)
                    ] :: [ Lib.Details ]

            println $ "changedThreadPosts :: [ (Thread.Thread, [ (JSONPost.Post, Post.Post) ]) ] thread count: " ++
                (show $ length changedThreadPosts) ++ " combined Post count: " ++ (show $ length $ changedThreadPosts >>= snd)
            println $ "existingBoardPostIdSet size: " ++ (show $ Set.size existingBoardPostIdSet)
            println $ "number of missingPostsDetails: " ++ (show $ length missingPostsDetails)

            downloadedMissingPosts :: [ Lib.Details ] <- mapM
                (Lib2.liftHttpIO . Lib2.downloadAttachment)
                missingPostsDetails

            println $ "downloadedMissingPosts result size (after running downloadAttachment): " ++ (show $ length downloadedMissingPosts)

            println "HELLO E"

            let
                mNoticerSettings = S.spam_noticer csmr_settings
                postsPerThread = Lib2.groupDetails downloadedMissingPosts

            cleanPostsPerThread <- case mNoticerSettings of
                Nothing -> return postsPerThread
                Just noticerSettings ->
                    let skipCheck =
                            maybe
                                False
                                (Set.member (unpack $ NSite.name site))
                                (S.trusted_sites noticerSettings)
                    in if skipCheck then return postsPerThread else do

                    noticerRequestInfos <- liftIO $ mapM
                          ( \(a, b, c, d, e) ->
                              SN.noticerReqInfoFromDetails a b c d e
                          )
                          [ (site, board, t, post, detailsList)
                          | (t, xs) <- postsPerThread
                          , (post, detailsList) <- xs
                          ]

                    println $ "noticerRequestInfos size: " ++ (show $ length noticerRequestInfos)

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

                    println $ "SpamNoticer should be given this many requests: " ++ (show $ length noticerArgs)

                    noticerResponses <- Lib2.liftHttpIO $ sequence <$> pooledMapConcurrentlyN
                        noticerJobs
                        (uncurry (SN.askNoticer noticerSettings))
                        noticerArgs

                    println $ "noticerResponses size: " ++ (show $ length noticerResponses)

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

            println $ "cleanPostsPerThread length: " ++ (show $ length cleanPostsPerThread)
            println $ "number of Posts total in cleanPostsPerThread: " ++ (show $ length (cleanPostsPerThread >>= snd))

            println "HELLO F"

            -- save new posts

            let changedThreads_ = map fst cleanPostsPerThread

            newThreads <- Lib2.liftHttpIO $
                Client.postThreads settings $
                    filter
                        (\t -> Map.notMember
                                (Thread.board_thread_id t) boardTidTidMap
                        )
                        changedThreads_

            println $ "newThreads after post: " ++ (show $ length newThreads)

            println "HELLO F2"

            -- At this point thread_id is still undefined for Thread and Post
            let
                threadIdByBoardTid = Map.unions
                        [ Map.fromList
                            [ (Thread.board_thread_id t, Thread.thread_id t)
                            | t <- newThreads
                            ]

                        , boardTidTidMap
                        ]

                cleanPPTWithThreadIds = map
                    ( \(t, xs) ->
                        let btid = Thread.board_thread_id t
                            tid  =
                                case Map.lookup btid threadIdByBoardTid of
                                    Just x -> x
                                    Nothing ->
                                        error $
                                            "BUG: no local thread_id for board_thread_id "
                                            ++ show btid

                            t_ = t { Thread.thread_id = tid }
                        in
                            ( t_
                            , [ (p { Post.thread_id = tid }, ds)
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

            println $ "threadIdByBoardTid size: " ++ (show $ Map.size threadIdByBoardTid)
            println $ "cleanPPTWithThreadIds length: " ++ (show $ length cleanPPTWithThreadIds)
            println $ "number of Posts total in cleanPPTWithThreadIds: " ++ (show $ length (cleanPPTWithThreadIds >>= snd))
            println $ "postsToSave length: " ++ (show $ length postsToSave)

            newPosts <- Lib2.liftHttpIO $
                Client.postPosts settings (map fst postsToSave)

            println $ "newPosts (postPosts response) length: " ++ (show $ length newPosts)

            println "HELLO G"

            let existingPostIds = Set.fromList (map (Client.idFromPost . fst) postsToSave)
                    `Set.difference` Set.fromList (map Client.idFromPost newPosts)

            println $ "existingPostIds set size: " ++ (show $ Set.size existingPostIds)

            existingPosts <- Lib2.liftHttpIO $
                Client.getPosts settings $ Set.toList existingPostIds

            println $ "existingPosts set size: " ++ (show $ Set.size existingPostIds)

            println "HELLO H"

            let
                postIdMap = Map.fromList
                    [ (Client.idFromPost p, p)
                    | p <- newPosts ++ existingPosts ]

                detailsWithFreshPosts =
                    [ (s, b, t_, p, mat)
                    | (t_, xs) <- cleanPPTWithThreadIds
                    , (p, ds) <- xs
                    , d <- ds
                    , let (s, b, _oldT, _oldP, mat) = d
                    ]

            println $ "postIdMap size: " ++ (show $ Map.size postIdMap)
            println $ "detailsWithFreshPosts length: " ++ (show $ length detailsWithFreshPosts)

            finalDetails <- liftIO $ mapM
                ( \(s, b, t_, p_, mat) ->
                    let key = Client.idFromPost p_
                        p =
                            case Map.lookup key postIdMap of
                                Just x -> x
                                Nothing ->
                                    error $
                                        "BUG: postIdMap missing post key "
                                        ++ show key
                                        ++ " (board_post_id="
                                        ++ show (Post.board_post_id p_)
                                        ++ ", thread_id="
                                        ++ show (Post.thread_id p_)
                                        ++ ")"
                    in
                        case mat of
                            Nothing ->
                                return (s, b, t_, p, Nothing)

                            Just (paths, attachment) -> do
                                a <- Lib.computeAttachmentHash
                                    paths
                                    ( attachment
                                        { At.post_id = fromJust $ Post.post_id p
                                        }
                                    )
                                return (s, b, t_, p, Just (paths, a))
                )
                detailsWithFreshPosts

            println $ "finalDetails length: " ++ (show $ length finalDetails)

            println "HELLO I"

            _savedAttachments <- Lib2.liftHttpIO $ Client.postAttachments settings
                [ a
                | (_, _, _, _, Just (_, a)) <- finalDetails
                ]

            println $ "_savedAttachments (postAttachments result) length: " ++ (show $ length _savedAttachments)

            println "HELLO J"

            liftIO $ mapM_ (Lib.copyOrMoveFiles settings Lib.moveAttachmentAndThumb)
                finalDetails

            println "HELLO K"

            _ <- Lib2.liftHttpIO $
                    Client.updatePostAttachmentNotConsidered
                        settings
                        (map (Thread.thread_id . fst) cleanPPTWithThreadIds)

            println "HELLO L"

            if null missingPostsDetails
            then do
                -- this will be hit if we just start the scraper and the last
                -- post on this board was saged, the thread_bump_time_slices table
                -- won't have the saged post record, and the process will try
                -- to query those threads
                let result = foldl' max board_last_modified
                      [ Lib.epochToUTCTime $ JSONThread.last_modified t
                      | t <- changedApiThreads ]
                println $ "missingPostsDetails is null. result last board_last_modified is " ++ show result
                return result
            else do
                -- result is the most recent timestamp of all the posts we just saved
                let result = foldr max board_last_modified $ map Post.creation_time
                      [ post
                      | (_, xs) <- postsPerThread
                      , (post, _) <- xs
                      ]
                println $ "missingPostsDetails is not null. result last board_last_modified is " ++ show result
                return result

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

    print sitesResult

    exitSuccess

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

    let site_name_to_site :: Map.Map Text NSite.Site =
            Map.fromList $ map (\s -> (NSite.name s, s)) sites

    let site_id_board_id_to_glppbr = Map.fromList $
            map
                (\b -> ((GLPPBR.site_id b, GLPPBR.pathpart b), b))
                latest_posts_per_board

    site_and_board_and_api_list_ <- mapM
        (\site_settings -> do
            let site_name = pack $ S.name site_settings

            putStrLn $ "member? " ++ show (Map.member site_name site_name_to_site)

            let site = (Map.!) site_name_to_site site_name

            putStrLn $ "Site OK: " ++ show site

            let s_id = NSite.site_id site

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
chooseApi S.TinyboardCCHTML = tinyboardHTMLClient
