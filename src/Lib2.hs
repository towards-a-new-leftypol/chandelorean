module Lib2
  ( httpGetCatalogJSON
  , ProgramException (..)
  , saveNewThreads
  , httpGetPostsJSON
  , saveNewPosts
  , saveNewAttachments
  ) where

import Control.Monad.Trans.Except (ExceptT (..))
import System.FilePath ((</>))
import qualified Data.Set as Set
import qualified Data.Map as Map
import Data.Aeson (FromJSON)
import Data.Int (Int64)
import Data.List (sortBy, foldl')
import Data.Ord (comparing)
import Data.Bifunctor (first)
import Data.Maybe (fromJust, catMaybes)
import Data.Text (Text)

import qualified Network.DataClient as Client
import qualified SitesType  as Sites
import qualified BoardsType as Boards
import Common.Network.HttpClient (HttpError)
import qualified JSONParsing as JSON
import qualified JSONPost
import qualified ThreadType as Thread
import qualified Common.PostsType as Posts
import Common.Server.JSONSettings (JSONSettings)
import qualified Common.Server.JSONSettings as JSettgs
import qualified Common.AttachmentType as At
import qualified Lib


data ProgramException = HttpException HttpError
  deriving Show


type IOe a = ExceptT ProgramException IO a


liftHttpIO :: IO (Either HttpError a) -> IOe a
liftHttpIO = ExceptT . fmap (first HttpException)


httpSiteGetRequest :: (FromJSON a) => Sites.Site -> String -> IO (Either HttpError a)
httpSiteGetRequest site path = Client.getJSON $ Sites.url site </> path

httpGetCatalogJSON
  :: Sites.Site
  -> Boards.Board
  -> IOe [ JSON.Catalog ]
httpGetCatalogJSON site board = liftHttpIO $ httpSiteGetRequest site path
  where
    path = Boards.pathpart board </> "catalog.json"


httpGetPostsJSON
  :: Sites.Site
  -> Boards.Board
  -> Thread.Thread
  -> IOe (Thread.Thread, [ JSONPost.Post ])
httpGetPostsJSON site board thread =
    liftHttpIO $
        fmap ((thread,) . JSONPost.posts) <$> httpSiteGetRequest site path

    where
        path = Boards.pathpart board
            </> "res"
            </> (show (Thread.board_thread_id thread) ++ ".json")


saveNewThreads
    :: JSONSettings
    -> Boards.Board
    -> [ JSON.Thread ]
    -> IOe [ Thread.Thread ]
saveNewThreads settings board web_threads = do
    existing_threads <- liftHttpIO $
        Client.getThreads
            settings
            (Boards.board_id board)
            (map JSON.no web_threads)

    let
        archived_board_thread_ids :: Set.Set Int
        archived_board_thread_ids =
            Set.fromList $ map Thread.board_thread_id existing_threads

        threads_to_create :: [ JSON.Thread ]
        threads_to_create =
            filter
                ((`Set.notMember` archived_board_thread_ids) . JSON.no)
                web_threads

        board_id :: Int = Boards.board_id board

    -- save new threads
    new_threads <- liftHttpIO $ Client.postThreads
        settings
        (map (Lib.apiThreadToArchiveThread board_id) threads_to_create)

    return $ existing_threads ++ new_threads


saveNewPosts
    :: JSONSettings
    -> [ (Thread.Thread, [ JSONPost.Post ]) ]
    -> IOe [ Posts.Post ]
saveNewPosts settings thread_posts = do
    existing_posts <- liftHttpIO $ Client.getPosts settings post_ids

    thread_max_local_idx <- liftHttpIO $ Client.getThreadMaxLocalIdx settings thread_ids

    let existing_set :: Set.Set (Int64, Int64) =
            Set.fromList
                (map (\x -> (Posts.thread_id x, Posts.board_post_id x))
                existing_posts)

    let tuples_to_insert :: [ (Thread.Thread, JSONPost.Post, Client.PostId) ] =
            sortBy (comparing $ \(_, _, p) -> Client.board_post_id p) $
                newPosts post_tuples existing_set

    let local_idx :: Map.Map Int64 Int = Map.fromList thread_max_local_idx

    let posts_to_insert :: [ Posts.Post ] = fst $ foldl' Lib.localIndexFoldf ([], local_idx) tuples_to_insert

    new_posts <- liftHttpIO $ Client.postPosts settings posts_to_insert

    return $ existing_posts ++ new_posts

    where
        flat_posts = concatMap (\(i, j) -> map (i,) j) thread_posts

        post_tuples = map
            (\(i, j) -> (i, j, Client.PostId (Thread.thread_id i) (JSONPost.no j)))
            flat_posts

        post_ids = map (\(_, _, x) -> x) post_tuples

        thread_ids :: [ Int64 ]
        thread_ids = map (Thread.thread_id . fst) thread_posts

        newPosts :: [(Thread.Thread, JSONPost.Post, Client.PostId)] -> Set.Set (Int64, Int64) -> [(Thread.Thread, JSONPost.Post, Client.PostId)]
        newPosts xs existing_set = filter (\(_, _, c) -> Set.notMember (Client.thread_id c, Client.board_post_id c) existing_set) xs


saveNewAttachments
    :: JSONSettings
    -> [(Sites.Site, Boards.Board, Thread.Thread, JSONPost.Post, Posts.Post)]
    -> IOe ()
saveNewAttachments settings post_tuples = do
    db_attachments <- let posts = map (\(_, _, _, _, x) -> x) post_tuples in
        liftHttpIO $
            Client.getAttachments
                settings
                (map (fromJust . Posts.post_id) posts)

    let existing_attachment_map :: Map.Map (Int64, Text) [ At.Attachment ] =
            Map.fromListWith
                (++)
                [ ((At.post_id a, At.board_filename a), [a])
                | a <- db_attachments
                ]

    let attachments_on_board :: [ Lib.Details ] =
            concatMap
                (Lib.parseAttachments (JSettgs.site_url settings))
                post_tuples

    let attachments_on_board_map =
            Map.fromListWith
                (++)
                [ ((At.post_id a, At.board_filename a), [x])
                | x@(_, _, _, _, _, a) <- attachments_on_board
                ]

    let to_insert = concat $ Map.elems $ attachments_on_board_map `Map.difference` existing_attachment_map

    attachment_paths_results <- mapM downloadAttachment to_insert

    let attachment_paths = catMaybes attachment_paths_results

    return ()


-- Downloads attachment and thumbnail to temporary files, and returns their paths.
downloadAttachment :: Lib.Details -> IOe (Maybe Lib.Details)
downloadAttachment (a, b, c, d, paths, f) = do
    result <- ExceptT $ do
        file_result <- Client.getFile (At.file_path paths)

        case file_result of
            -- return Right if we get 404, to keep going and just save the Post without this attachment
            Left (Client.StatusCodeError 404 _) -> return $ Right Nothing
            Left e -> return $ Left $ HttpException e
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

    return $ result >>= \x -> Just (a, b, c, d, x, f)
