{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use tuple-section" #-}

module Lib2
  ( httpGetCatalogJSON
  , ProgramException (..)
  , httpGetPostsJSON
  , httpGet
  , removeDeletedThreads
  , liftHttpIO
  , downloadAttachment
  , IOe
  , groupDetails
  , unlinkAttachmentFiles
  , figureOutBoardThreadIdToThreadIdMap
  ) where

import Control.Monad.Trans.Except (ExceptT (..))
import System.FilePath ((</>))
import qualified Data.Map as Map
import Data.Aeson (FromJSON)
import Data.Int (Int64)
import Data.Bifunctor (first)
import Data.Maybe (fromJust)
import System.Directory (removeDirectoryRecursive, doesDirectoryExist)
import Control.Monad.IO.Class (liftIO)
import Control.Monad (when, unless, forM_)
import qualified Data.ByteString.Lazy as LBS
import System.Directory (removeFile)
import System.IO.Error (catchIOError, isDoesNotExistError)
import qualified Data.Text as T

import qualified Network.DataClient as Client
import qualified BoardsType as Boards
import Common.Network.HttpClient (HttpError)
import qualified Network.Api.JSONParsing as JSON
import qualified Network.Api.JSONPost as JSONPost
import qualified ThreadType as Thread
import qualified Common.PostsType as Posts
import Common.Server.JSONSettings (JSONSettings)
import qualified Common.AttachmentType as At
import qualified Lib
import qualified BoardQueueElem as QE
import qualified Common.Network.SiteType as Site


data ProgramException = HttpException HttpError
  deriving Show


type IOe a = ExceptT ProgramException IO a


liftHttpIO :: IO (Either HttpError a) -> IOe a
liftHttpIO = ExceptT . fmap (first HttpException)


httpSiteJSONGetRequest
    :: (FromJSON a)
    => Site.Site
    -> String
    -> IOe a
httpSiteJSONGetRequest site path = liftHttpIO $
    Client.getJSON $ T.unpack (Site.url site) </> path


httpGetCatalogJSON :: Site.Site -> Boards.Board -> IOe [ JSON.Catalog ]
httpGetCatalogJSON site board = httpSiteJSONGetRequest site path
    where
        path = Boards.pathpart board </> "catalog.json"


httpGet
  :: Site.Site
  -> String
  -> IOe LBS.ByteString
httpGet site path = liftHttpIO $
    Client.get_ (T.unpack (Site.url site) </> path) []


httpGetPostsJSON
  :: Site.Site
  -> Boards.Board
  -> Thread.Thread
  -> IOe (Thread.Thread, [ JSONPost.Post ])
httpGetPostsJSON site board thread =
    (thread,) . JSONPost.posts <$> httpSiteJSONGetRequest site path

    where
        path = Boards.pathpart board
            </> "res"
            </> (show (Thread.board_thread_id thread) ++ ".json")


-- Downloads attachment and thumbnail to temporary files, and returns their paths.
downloadAttachment :: Lib.Details -> IO (Either HttpError Lib.Details)
downloadAttachment (a, b, c, d, Just (paths, f)) = do
    result <- do
        file_result <- Client.getFile (At.file_path paths)

        case file_result of
            -- return Right if we get 404, to keep going and just save the Post without this attachment
            Left (Client.StatusCodeError 404 _) -> return $ Right Nothing
            Left (Client.StatusCodeError 451 _) -> return $ Right Nothing
            Left e -> return $ Left e
            Right filepath -> do
                case At.thumbnail_path paths of
                    Nothing -> return $ Right $ Just $ At.Paths filepath Nothing
                    Just thumb_url -> do
                        thumb_result <- Client.getFile thumb_url

                        case thumb_result of
                            Left err -> do
                                print err
                                return $ Right $ Just $ At.Paths filepath Nothing
                            Right thumb_path -> return $ Right $ Just $ At.Paths filepath $ Just thumb_path

    return $ result >>= maybe
        (Right (a, b, c, d, Nothing))
        (Right . (\y -> (a, b, c, d, Just (y, f))))

downloadAttachment x = return $ Right x


-- Only run this after syncing all of the threads on the board successfully
removeDeletedThreads
    :: JSONSettings
    -> QE.BoardQueueElem
    -> [ JSON.Thread ]
    -> IOe ()
removeDeletedThreads _ QE.BoardQueueElem { QE.last_catalog = Nothing } _ = return ()
removeDeletedThreads settings board_elem new_catalog = do
    let old_map :: Map.Map Int64 Int = createIdxMap (map JSON.no $ fromJust $ QE.last_catalog board_elem)
    let new_map :: Map.Map Int64 Int = createIdxMap (map JSON.no new_catalog)

    let gone = old_map `Map.difference` new_map

    let max_position = Map.size old_map `div` 2
    let to_delete = Map.filter (< max_position) gone
    let to_del_board_thread_ids :: [ Int64 ] = map fst $ Map.toList to_delete

    unless (Map.null to_delete) $ do
        liftIO $ putStrLn $ "Deleting " ++ show (Map.size to_delete) ++ " threads: " ++ show to_del_board_thread_ids

        _ <- liftHttpIO $
            Client.deleteThreads
                settings
                (Boards.board_id board)
                to_del_board_thread_ids

        mapM_ (liftIO . rmThreadFiles) to_del_board_thread_ids


    where
        createIdxMap :: (Ord a) => [a] -> Map.Map a Int
        createIdxMap xs = Map.fromList $ zip xs [0..]

        site = QE.site board_elem
        board = QE.board board_elem

        rmThreadFiles :: Int64 -> IO ()
        rmThreadFiles board_thread_id = do
            let path = Lib.makeThreadAttachmentFsPath settings site board board_thread_id

            exists <- doesDirectoryExist path

            when exists $ removeDirectoryRecursive path


groupDetails :: [ Lib.Details ] -> [ (Thread.Thread, [ (Posts.Post, [ Lib.Details ]) ]) ]
groupDetails =
    Map.toList
    . Map.map Map.elems
    . foldl' add Map.empty

    where
        add acc x@(_, _, t, p, _) = Map.alter (Just . insertThread) t acc
            where
                postKey = Posts.board_post_id p

                insertThread Nothing =
                    Map.singleton postKey (p, [x])

                insertThread (Just postMap) =
                    Map.alter (Just . insertPost) postKey postMap

                insertPost Nothing = (p, [x])
                insertPost (Just (oldP, oldDetails)) =
                    (oldP, oldDetails ++ [x])


unlinkAttachmentFiles :: [ Lib.Details ] -> IO ()
unlinkAttachmentFiles = (flip forM_) unlinkOne
    where
        unlinkOne :: Lib.Details -> IO ()
        unlinkOne (_, _, _, _, Just (paths, _)) = do
            unlinkFile (At.file_path paths)
            forM_ (At.thumbnail_path paths) unlinkFile
        unlinkOne _ = pure ()

        unlinkFile :: FilePath -> IO ()
        unlinkFile path =
            removeFile path `catchIOError` \e ->
                unless (isDoesNotExistError e) (ioError e)

type BoardThreadId = Int64
type ThreadId      = Int64
type BoardPostId   = Int64

figureOutBoardThreadIdToThreadIdMap
    :: Map.Map BoardThreadId [ BoardPostId ]
    -> Map.Map BoardPostId ThreadId
    -> Map.Map BoardThreadId ThreadId
figureOutBoardThreadIdToThreadIdMap
    boardThreadIdBoardPostIdsMap
    boardPostIdThreadIdMap =
        Map.mapMaybe findTid boardThreadIdBoardPostIdsMap

        where
            findTid :: [ BoardPostId ] -> Maybe ThreadId
            findTid [] = Nothing
            findTid (x:xs) =
                case Map.lookup x boardPostIdThreadIdMap of
                    Nothing -> findTid xs
                    Just tid -> Just tid
