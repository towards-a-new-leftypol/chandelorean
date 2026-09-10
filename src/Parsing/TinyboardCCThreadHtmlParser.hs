{-# LANGUAGE OverloadedStrings #-}

module Parsing.VichanCCThreadHtmlParser where

import Data.Text (Text)
import qualified Data.Text as Txt
import qualified Data.Text.IO as Txt
import qualified Data.Text.Read as Txt
import Data.Tree (Forest, Tree, flatten)
import Text.HTML.Parser
import Text.HTML.Tree
import Data.Maybe (fromJust, fromMaybe, maybe)
import Data.Char (isDigit)
import Data.Int (Int64)
import Data.Time.Clock (getCurrentTime, UTCTime)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime, utcTimeToPOSIXSeconds)

import Parsing.HtmlParsingUtils
import Network.Api.JSONParsing as J

processCatalogPage :: Text -> IO ()
processCatalogPage htmlText =
    case rawTokensToForest $ parseRawTokens htmlText of
        Left err -> error $ show err
        Right forest -> do
            now <- getCurrentTime
            let threads = parseThreads now forest
            mapM_ print threads


parseThreads :: UTCTime -> Forest RawToken -> [ Thread ]
parseThreads now forest = map ((parseThread now) . (: [])) elemThreadsList
    where
        elemThreadsList = getChildElements $ head $ findByTag "ul" forest



parseThread :: UTCTime -> Forest RawToken -> Thread
parseThread now elemThread =
    -- putStrLn $ "boardThreadId: " <> show boardThreadId
    -- print mFileEpoch
    -- print $ fromMaybe now mFileTime
    -- print elemOpImg

    -- putStrLn $ "last_modified: " <> show lastModified
    -- putStrLn $ "time: " <> show creationTime
    -- putStrLn $ "replies: " <> show replyCount
    -- putStrLn ""
    Thread
      { no            = boardThreadId
      , sub           = Nothing
      , com           = Nothing
      , name          = Nothing
      , capcode       = Nothing
      , time          = utcTimeToEpochSeconds creationTime
      , omitted_posts = Nothing
      , omitted_images= Nothing
      , replies       = Just replyCount
      , images        = Nothing
      , sticky        = Nothing
      , locked        = Nothing
      , cyclical      = Nothing
      , last_modified = utcTimeToEpochSeconds lastModified
      -- , board         :: Text
      , files         = Nothing
      , resto         = 0
      , unique_ips    = Nothing
      }

    where
        linkElem = fromJust $ findFirstByClass "catalog-link" elemThread

        elemOpImg = treeHead $ head $ findByTag "img" elemThread

        mFileEpoch =
            (getAttribute "src" elemOpImg)
            >>= parseFilenameTime

        mFileTime = posixSecondsToUTCTime <$> mFileEpoch

        timeText = fromJust $ getAttribute "title" elemOpImg

        boardThreadId = fromJust $ boardThreadIdFromUrl <$>
                getAttribute "href" (treeHead linkElem)

        -- read the text in the .reply-count span into an Int
        replyCount = fst $ fromRight $ Txt.decimal $ extractText $ (: []) $
                fromJust $ findFirstByClass "reply-count" elemThread

        lastModified = fromJust $
            parseTitleTime (maybe (Left now) Right mFileTime) timeText

        creationTime =
                case mFileTime of
                    Nothing -> lastModified -- OP doesn't have an image with the timestamp filename, we have to use what's in the title attribute and wrongly assume it's not the bump time but rather the creation time. However this is only the creation time for the thread table, the real creation time will be in the opening post in the posts table
                    Just  t -> t


treeHead :: Tree a -> a
treeHead = head . flatten


boardThreadIdFromUrl :: Text -> Int64
boardThreadIdFromUrl = fst . fromRight . Txt.decimal . Txt.takeWhile isDigit .
    last . Txt.splitOn "/"


-- WARN: Partial function
fromRight :: Either a b -> b
fromRight (Left _) = error "Expected Right value, got Left"
fromRight (Right x) = x


main :: IO ()
main = do
    putStrLn "Hello World"
    txt <- Txt.readFile "/home/phil/Downloads/https___crystal.cafe_b_catalog.html"
    -- txt <- Txt.readFile "/home/phil/Downloads/cc_pretty.html"
    processCatalogPage txt
