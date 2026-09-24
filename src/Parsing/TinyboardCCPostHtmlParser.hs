{-# LANGUAGE OverloadedStrings #-}

module Parsing.TinyboardCCPostHtmlParser
  ( processThreadPage )
  where

import Prelude hiding (id)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Tree (Forest, Tree, subForest, rootLabel)
import Data.Int (Int64)
import Data.Maybe (fromJust, fromMaybe, listToMaybe, mapMaybe)
import Data.Char (isDigit)
import Data.Time (UTCTime, parseTimeM, defaultTimeLocale)
import Text.Read (readMaybe)
import Control.Monad (guard)
import Network.Mime (defaultMimeLookup)
import Data.Text.Encoding (decodeUtf8)

import Parsing.HtmlParsingUtils
import Network.Api.JSONCommonTypes as JF
import Network.Api.JSONPost as J

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


-- | Parses the Vichan fileinfo title attribute.
--
-- Supports both:
--   "filename.ext (61.56 KB, 1069x387)"
--   "filename.ext (61.56 KB)"
--
-- Dimensions are optional.
parseFileTitle :: Text -> Maybe (Text, Int, Maybe Int, Maybe Int)
parseFileTitle title = do
    let (nameAndRest, rest) = T.breakOnEnd " (" title
    guard $ not $ T.null rest

    let name = T.dropEnd 2 nameAndRest -- drop the trailing " ("
    let inner = T.init rest             -- drop the trailing ")"
    let parts = T.splitOn ", " inner

    case parts of
        [sizeStr, dimStr] -> do
            fsize <- parseSize sizeStr
            let dims = T.splitOn "x" dimStr
            guard $ length dims == 2
            w <- readMaybe $ T.unpack $ dims !! 0
            h <- readMaybe $ T.unpack $ dims !! 1
            return (name, fsize, Just w, Just h)

        [sizeStr] -> do
            fsize <- parseSize sizeStr
            return (name, fsize, Nothing, Nothing)

        _ -> Nothing


-- | Parse CSS width/height from a style attribute.
--
-- Example:
--   "width: 141px; height: 141px"
parseStyleDims :: Text -> Maybe (Int, Int)
parseStyleDims style = do
    let decls = T.splitOn ";" style
    w <- findCssLength "width" decls
    h <- findCssLength "height" decls
    return (w, h)
  where
    findCssLength :: Text -> [Text] -> Maybe Int
    findCssLength prop decls = do
        decl <- listToMaybe $ filter
            (\d -> T.strip (T.toLower (fst (T.breakOn ":" d))) == prop)
            decls

        let val = T.strip $ snd $ T.breakOn ":" decl
        parseCssPx val

    parseCssPx :: Text -> Maybe Int
    parseCssPx v =
        let numStr = T.takeWhile (\c -> isDigit c || c == '.') v
        in round <$> (readMaybe (T.unpack numStr) :: Maybe Double)


-- | Find the thumbnail node for a file block.
--
-- Prefers the media inside `<a class="file">`, and accepts either
-- `<img>` or `<video>`.
findThumbTree :: [Tree RawToken] -> Maybe (Tree RawToken)
findThumbTree trees =
    let fileLink = listToMaybe $ filter isFileLink trees
    in case fileLink of
         Just link ->
             case listToMaybe (thumbCandidates (subForest link)) of
               Just t  -> Just t
               Nothing -> listToMaybe (thumbCandidates trees)
         Nothing ->
             listToMaybe (thumbCandidates trees)
  where
    thumbCandidates scope =
        findByTag "img" scope ++ findByTag "video" scope

    isFileLink t =
        getTagName (rootLabel t) == Just "a"
        && hasTokenClass "file" (rootLabel t)


-- | Extract a thumbnail URL from an `<img>` or `<video>`.
--
-- For `<img>` this is normally `src`.
-- For `<video>` we also allow `poster` as a fallback.
thumbSrcAttr :: Tree RawToken -> Maybe Text
thumbSrcAttr t =
    case getAttribute "src" (rootLabel t) of
      Just src -> Just src
      Nothing  -> getAttribute "poster" (rootLabel t)


-- | Extracts a File record from a list of sibling/child nodes containing
-- fileinfo and thumbnail/media tags.
extractFile :: [Tree RawToken] -> Maybe JF.File
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

    (parsedName, fsize, titleW, titleH) <- parseFileTitle title

    let mbThumbTree = findThumbTree trees
        mbStyleDims = mbThumbTree
                  >>= getAttribute "style" . rootLabel
                  >>= parseStyleDims

        -- Prefer title dimensions, because those are normally the real media
        -- dimensions. Fall back to thumbnail style dimensions if needed.
        (w, h) = case (titleW, titleH) of
                   (Just tw, Just th) -> (Just tw, Just th)
                   _ -> case mbStyleDims of
                          Just (sw, sh) -> (Just sw, Just sh)
                          Nothing       -> (titleW, titleH)

        thumbSrc = fromMaybe "" (mbThumbTree >>= thumbSrcAttr)

        ext = T.toLower $ last $ T.splitOn "." href
        fileId = T.takeWhile isDigit $ last $ T.splitOn "/" href
        linkText = extractText (subForest aTree)
        isSpoiler = linkText == "Spoiler" || "spoiler" `T.isInfixOf` thumbSrc

        -- Use mime-types to lookup the mimetype based on the parsed filename extension
        mimeType = decodeUtf8 $ defaultMimeLookup parsedName

    return JF.File
        { JF.id = fileId
        , JF.mime = Just mimeType
        , JF.ext = ext
        , JF.h = h
        , JF.w = w
        , JF.fsize = fsize
        , JF.filename = basename parsedName
        , JF.spoiler = Just isSpoiler
        , JF.md5 = ""
        , JF.file_path = href
        , JF.thumb_path = thumbSrc
        }


-- | Basename without the final extension.
-- Examples:
--   "photo.jpg"       -> "photo"
--   "archive.tar.gz"  -> "archive.tar"
--   "photo"           -> "photo"
--   ".hidden"         -> ".hidden"
--   "dir/photo.jpg"   -> "photo"
basename :: T.Text -> T.Text
basename p =
  let name = T.takeWhileEnd (\c -> c /= '/' && c /= '\\') p
   in case T.breakOnEnd "." name of
        (baseDot, ext)
          | T.null ext          -> name   -- no extension
          | T.length baseDot <= 1 -> name -- dotfile like ".hidden"
          | otherwise           -> T.init baseDot


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

parseThreadContainer :: Tree RawToken -> [ Post ]
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
                
    in J.Post
        { J.no = no
        , J.com = com
        , J.name = name
        , J.sub = sub
        , J.email = Nothing
        , J.time = timeEpoch
        , J.omitted_posts = Nothing
        , J.omitted_images = Nothing
        , J.sticky = Nothing
        , J.locked = Nothing
        , J.cyclical = Nothing
        , J.last_modified = timeEpoch
        , J.embed = embed
        , J.files = files
        , J.resto = 0
        , J.unique_ips = Nothing
        , J.filename = Nothing
        , J.h = Nothing
        , J.w = Nothing
        , J.ext = Nothing
        , J.tim = Nothing
        , J.fsize = Nothing
        , J.spoiler = Nothing
        , J.extra_files = Nothing
        }

findThreadContainer :: Forest RawToken -> Maybe (Tree RawToken)
findThreadContainer forest = listToMaybe $ filter isThreadContainer (findByTag "div" forest)
  where
    isThreadContainer tree = maybe False (T.isPrefixOf "thread_") (getAttribute "id" (rootLabel tree))

processThreadPage :: Text -> [ Post ]
processThreadPage htmlText =
    case rawTokensToForest $ parseRawTokens htmlText of
        Left err -> error $ show err
        Right forest -> parsePosts forest

parsePosts :: Forest RawToken -> [ Post ]
parsePosts pageForest =
    case findThreadContainer pageForest of
        Nothing -> []
        Just threadTree -> parseThreadContainer threadTree
