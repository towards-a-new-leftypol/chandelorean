{-# LANGUAGE OverloadedStrings #-}

import Prelude hiding (id)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as Txt
import Text.HTML.Parser
import Text.HTML.Tree
import Data.Tree (Forest, Tree, subForest, rootLabel)
import Data.Int (Int64)
import Data.Maybe (fromJust, fromMaybe, listToMaybe, mapMaybe)
import Data.Char (isDigit)
import Data.Time (UTCTime, parseTimeM, defaultTimeLocale)
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)
import Text.Read (readMaybe)
import Control.Monad (guard)
import Network.Mime (defaultMimeLookup)
import Data.Text.Encoding (decodeUtf8)

import HtmlParsingUtils

data File = File
  { id         :: Text
  , mime       :: Maybe Text
  , ext        :: Text
  , h          :: Maybe Int
  , w          :: Maybe Int
  , fsize      :: Int
  , filename   :: Text
  , spoiler    :: Maybe Bool
  , md5        :: Text
  , file_path  :: Text
  , thumb_path :: Text
  } deriving (Show, Eq, Ord)

data Post = Post
    { no             :: Int64
    , com            :: Maybe Text
    , name           :: Maybe Text
    , sub            :: Maybe Text
    , email          :: Maybe Text
    , time           :: Int
    , omitted_posts  :: Maybe Int
    , omitted_images :: Maybe Int
    , sticky         :: Maybe Int
    , locked         :: Maybe Int
    , cyclical       :: Maybe Bool
    , last_modified  :: Int
    , embed          :: Maybe Text
    , files          :: Maybe [ File ]
    , resto          :: Int
    , unique_ips     :: Maybe Int
    -- legacy attributes
    , filename_      :: Maybe Text
    , h_             :: Maybe Int
    , w_             :: Maybe Int
    , ext_           :: Maybe Text
    , tim_           :: Maybe Text
    , fsize_         :: Maybe Int
    , spoiler_       :: Maybe Int
    , extra_files_   :: Maybe [ Text ]
    } deriving Show

-- ==============================================================================
-- HELPERS
-- ==============================================================================

-- | Parses the robust ISO8601 datetime attribute from the <time> tag.
parseDatetimeAttr :: Text -> Maybe UTCTime
parseDatetimeAttr txt =
    let str = T.unpack txt
        formats = [ "%Y-%m-%dT%H:%M:%S%QZ"
                  , "%Y-%m-%dT%H:%M:%SZ"
                  , "%Y-%m-%dT%H:%M:%S%Q%z"
                  , "%Y-%m-%dT%H:%M:%S%z"
                  , "%Y-%m-%dT%H:%M:%S%Q"
                  , "%Y-%m-%dT%H:%M:%S"
                  ]
    in listToMaybe $ mapMaybe (\f -> parseTimeM True defaultTimeLocale f str) formats

-- | Parses size strings like "61.56 KB" or "1.2 MB" into bytes.
parseSize :: Text -> Maybe Int
parseSize s = do
    let (numStr, unit) = T.span (\c -> isDigit c || c == '.') s
    num <- readMaybe (T.unpack numStr) :: Maybe Double
    guard $ num > 0
    let multiplier = case T.strip unit of
          "KB" -> 1024
          "MB" -> 1024 * 1024
          "GB" -> 1024 * 1024 * 1024
          "B"  -> 1
          _    -> 1
    return $ round (num * multiplier)

-- | Parses the Vichan fileinfo title attribute: "filename.ext (61.56 KB, 1069x387)"
-- Uses breakOnEnd to safely handle filenames that contain parentheses.
parseFileTitle :: Text -> Maybe (Text, Int, Int, Int)
parseFileTitle title = do
    let (nameAndRest, rest) = T.breakOnEnd " (" title
    guard $ not $ T.null rest
    let name = T.dropEnd 2 nameAndRest -- drop the trailing " ("
    let inner = T.init rest             -- drop the trailing ")"
    let parts = T.splitOn ", " inner
    guard $ length parts == 2
    let sizeStr = parts !! 0
        dimStr = parts !! 1
    fsize <- parseSize sizeStr
    let dims = T.splitOn "x" dimStr
    guard $ length dims == 2
    w <- readMaybe $ T.unpack $ dims !! 0
    h <- readMaybe $ T.unpack $ dims !! 1
    return (name, fsize, w, h)

-- | Extracts a File record from a list of sibling/child nodes containing fileinfo and img tags.
extractFile :: [Tree RawToken] -> Maybe File
extractFile trees = do
    fileInfoTree <- listToMaybe $ filter
        ( \t ->
            let u = rootLabel t
            in getTagName u == Just "p" && hasTokenClass "fileinfo" u
        ) trees

    let aTrees = findByTag "a" (subForest fileInfoTree)

    aTree <- listToMaybe aTrees
    href <- getAttribute "href" (rootLabel aTree)
    title <- getAttribute "title" (rootLabel aTree)
    
    (parsedName, fsize, w, h) <- parseFileTitle title
    
    imgTree <- listToMaybe $ findByTag "img" trees
    thumbSrc <- getAttribute "src" (rootLabel imgTree)
    
    let ext = T.toLower $ last $ T.splitOn "." href
        fileId = T.takeWhile isDigit $ last $ T.splitOn "/" href
        linkText = extractText (subForest aTree)
        isSpoiler = linkText == "Spoiler" || "spoiler" `T.isInfixOf` thumbSrc
        
        -- Use mime-types to lookup the mimetype based on the parsed filename extension
        mimeType = decodeUtf8 $ defaultMimeLookup parsedName
        
    return File
        { id = fileId
        , mime = Just mimeType
        , ext = ext
        , h = Just h
        , w = Just w
        , fsize = fsize
        , filename = parsedName
        , spoiler = Just isSpoiler
        , md5 = ""
        , file_path = href
        , thumb_path = thumbSrc
        }

-- | Extracts an embed URL/ID from a list of sibling/child nodes containing a video-container.
extractEmbed :: [Tree RawToken] -> Maybe Text
extractEmbed trees = do
    videoTree <- listToMaybe $ filter
        (\t -> getTagName (rootLabel t) == Just "div" && hasTokenClass "video-container" (rootLabel t))
        trees
    
    return $ outerHtml [ videoTree ]


isOpDiv :: Tree RawToken -> Bool
isOpDiv t = getTagName u == Just "div" && hasTokenClass "op" u
    where
        u = rootLabel t

isReplyDiv :: Tree RawToken -> Bool
isReplyDiv t = getTagName u == Just "div" && hasTokenClass "reply" u
    where
        u = rootLabel t

-- ==============================================================================
-- CORE PARSING LOGIC
-- ==============================================================================

parseThreadContainer :: Tree RawToken -> [Post]
parseThreadContainer threadTree =
    let children = getChildElements threadTree
        -- The OP's file/embed nodes appear as preceding siblings to the div.post.op
        (opMedia, opAndReplies) = span (not . isOpDiv) children
        
        opPost = case dropWhile (not . isOpDiv) opAndReplies of
                   (x:_) -> Just x
                   _     -> Nothing
        
        replies = filter isReplyDiv opAndReplies
        
        opFilesParsed = fmap (:[]) $ extractFile opMedia
        opEmbedParsed = extractEmbed opMedia
        
        opPostParsed = maybe [] (\t -> [parsePost t opFilesParsed opEmbedParsed True]) opPost
        
        -- For replies, files/embeds are children of the div.post.reply
        repliesParsed = map (\t -> 
            let rFiles = fmap (:[]) $ extractFile (subForest t)
                rEmbed = extractEmbed (subForest t)
            in parsePost t rFiles rEmbed False) replies
    in opPostParsed ++ repliesParsed

parsePost :: Tree RawToken -> Maybe [File] -> Maybe Text -> Bool -> Post
parsePost postTree mbOpFiles mbOpEmbed isOp =
    let introTree = fromJust $ findFirstByClass "intro" (subForest postTree)
        bodyTree = findFirstByClass "body" (subForest postTree)
        
        -- The ID of the <p class="intro"> is exactly the post number
        postNoStr = fromMaybe "0" $ getAttribute "id" (rootLabel introTree)
        no = fromMaybe 0 $ readMaybe (T.unpack postNoStr) :: Int64
        
        subTree = findFirstByClass "subject" (subForest introTree)
        sub = fmap (extractText . subForest) subTree
        
        nameTree = findFirstByClass "name" (subForest introTree)
        name = fmap (extractText . subForest) nameTree
        
        timeLink = findFirstByClass "date-link" (subForest introTree)
        
        -- Prefer the robust ISO8601 datetime attribute over the Vichan title string
        timeUTCTime = case timeLink >>= \t -> listToMaybe (findByTag "time" (subForest t)) >>= getAttribute "datetime" . rootLabel >>= parseDatetimeAttr of
            Just t  -> t
            Nothing -> error $ "Missing or invalid datetime attribute for post " ++ show no
            
        timeEpoch = utcTimeToEpochSeconds timeUTCTime
        
        -- Extract verbatim HTML for the comment body
        com = fmap innerHtml bodyTree
        
        files = if isOp
                then mbOpFiles
                else fmap (:[]) $ extractFile (subForest postTree)
                
        embed = if isOp
                then mbOpEmbed
                else extractEmbed (subForest postTree)
                
    in Post
        { no = no
        , com = com
        , name = name
        , sub = sub
        , email = Nothing
        , time = timeEpoch
        , omitted_posts = Nothing
        , omitted_images = Nothing
        , sticky = Nothing
        , locked = Nothing
        , cyclical = Nothing
        , last_modified = timeEpoch
        , embed = embed
        , files = files
        , resto = 0
        , unique_ips = Nothing
        , filename_ = Nothing
        , h_ = Nothing
        , w_ = Nothing
        , ext_ = Nothing
        , tim_ = Nothing
        , fsize_ = Nothing
        , spoiler_ = Nothing
        , extra_files_ = Nothing
        }

findThreadContainer :: Forest RawToken -> Maybe (Tree RawToken)
findThreadContainer forest = listToMaybe $ filter isThreadContainer (findByTag "div" forest)
  where
    isThreadContainer tree = maybe False (T.isPrefixOf "thread_") (getAttribute "id" (rootLabel tree))

processThreadPage :: Text -> IO ()
processThreadPage htmlText =
    case rawTokensToForest $ parseRawTokens htmlText of
        Left err -> error $ show err
        Right forest -> do
            let posts = parsePosts forest
            mapM_ print posts

parsePosts :: Forest RawToken -> [Post]
parsePosts pageForest =
    case findThreadContainer pageForest of
        Nothing -> []
        Just threadTree -> parseThreadContainer threadTree

main :: IO ()
main = do
    putStrLn "Hello World"
    -- txt <- Txt.readFile "/home/phil/Downloads/_b_ - Lolcow.Farm Hate Thread #12_ Eternity Of Insanity Edition.html"
    txt <- Txt.readFile "/home/phil/Downloads/_media_ - Electronic Music Thread..._.html"
    processThreadPage txt
