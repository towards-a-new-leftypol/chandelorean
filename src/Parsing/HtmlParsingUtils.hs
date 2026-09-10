{-# LANGUAGE OverloadedStrings #-}

module Parsing.HtmlParsingUtils where

import Data.Text (Text)
import qualified Data.Text as T
import Data.Tree
import Data.Char (isDigit)
import Data.Maybe (mapMaybe, listToMaybe)
import Data.Time
import Data.Time.Clock.POSIX
import Text.Read (readMaybe)
import Control.Monad (guard)
import qualified Data.Attoparsec.Text as A
import Data.List (unfoldr)

import Text.HTML.Parser
import Text.HTML.Tree


-- | A token paired with the exact raw source text it was parsed from.
data RawToken = RawToken
    { rawSource :: !Text
    , tokenVal  :: !Token
    } deriving (Show, Eq, Ord)


{-
 - NAVIGATION
 -}

-- | Find all descendant nodes with a specific CSS class.
findByClass :: Text -> Forest RawToken -> [Tree RawToken]
findByClass targetClass = concatMap go
  where
    go node@(Node rt children)
        | hasTokenClass targetClass rt = node : findByClass targetClass children
        | otherwise                    = findByClass targetClass children


-- | Find the first descendant node with a given CSS class.
findFirstByClass :: Text -> Forest RawToken -> Maybe (Tree RawToken)
findFirstByClass targetClass = listToMaybe . findByClass targetClass


-- | Find all descendant nodes with a specific HTML tag name.
findByTag :: Text -> Forest RawToken -> [Tree RawToken]
findByTag targetTag = concatMap go
  where
    go node@(Node rt children)
        | getTagName rt == Just targetTag = node : findByTag targetTag children
        | otherwise                       = findByTag targetTag children


-- | Get only the element children of a node, ignoring text/comments.
getChildElements :: Tree RawToken -> [ Tree RawToken ]
getChildElements (Node _ children) = filter isElement children
  where
    isElement (Node t _) = case t of
        RawToken _ (TagOpen {})      -> True
        RawToken _ (TagSelfClose {}) -> True
        _                            -> False


{-
 - INSPECTION (Operating on 'Token')
 -}

-- | Safely extract an attribute value from a Token.
-- | Safely extract an attribute value from a Token.
getAttribute :: Text -> RawToken -> Maybe Text
getAttribute attrName (RawToken _ tok) = case tok of
    TagOpen _ attrs      -> findAttr attrs
    TagSelfClose _ attrs -> findAttr attrs
    _                    -> Nothing
  where
    findAttr [] = Nothing
    findAttr (Attr key value : rest)
        | key == attrName = Just value
        | otherwise       = findAttr rest


-- | Check if a Token has a specific CSS class.
hasTokenClass :: Text -> RawToken -> Bool
hasTokenClass targetClass rt = 
    maybe False (elem targetClass . T.words) (getAttribute "class" rt)


-- | Get the tag name.
getTagName :: RawToken -> Maybe Text
getTagName (RawToken _ tok) = case tok of
    TagOpen name _      -> Just name
    TagSelfClose name _ -> Just name
    _                   -> Nothing


{-
 - TEXT EXTRACTION
-}

-- | Flatten a Forest of RawTokens back into a linear list.
-- Synthesizes closing tags for non-void elements since they were consumed by the tree builder.
rawTokensFromForest :: Forest RawToken -> [RawToken]
rawTokensFromForest = mconcat . fmap rawTokensFromTree


rawTokensFromTree :: Tree RawToken -> [RawToken]
rawTokensFromTree (Node rt children) =
    case tokenVal rt of
        TagOpen n _ | n `notElem` nonClosing ->
            [rt] <> rawTokensFromForest children <> [syntheticClose n]
        _ ->
            [rt] <> rawTokensFromForest children
  where
    -- We synthesize the closing tag text. It won't preserve weird whitespace 
    -- like `</ div >`, but that is exceptionally rare and perfectly valid HTML.
    syntheticClose n = RawToken ("</" <> n <> ">") (TagClose n)


-- | Get the exact, verbatim inner HTML of a node.
-- No re-rendering, no escaping, no entity normalization.
innerHtml :: Tree RawToken -> Text
innerHtml = outerHtml . subForest


outerHtml :: Forest RawToken -> Text
outerHtml = T.concat . map rawSource . rawTokensFromForest


-- | Recursively extract and concatenate all text content from a Forest.
extractText :: Forest RawToken -> Text
extractText = T.concat . map go
  where
    go (Node (RawToken _ tok) children) = case tok of
        ContentText t -> t
        ContentChar c -> T.singleton c
        _             -> extractText children


{-
 - Timestamp helpers
 -}

-- | Compare two 'UTCTime's up to minute precision.
--
-- This is necessary because Vichan title timestamps usually have only
-- minute precision:
--
--   "Feb 29 20:00"
--
-- while the filename timestamp may have second/millisecond precision:
--
--   1583006454.268 == Feb 29 20:00:54.268
--
-- These should be considered the same minute.
sameUpToMinute :: UTCTime -> UTCTime -> Bool
sameUpToMinute left right =
    utctDay left == utctDay right
    && minuteNumber left == minuteNumber right
  where
    minuteNumber timestamp =
        let seconds = floor (utctDayTime timestamp) :: Integer
        in seconds `div` 60


-- | Choose the most recent plausible year relative to a fallback "now" time.
--
-- This is used when there is no OP-image filename timestamp.
--
-- We try the current year, then previous years. This handles:
--
--   * ordinary dates that have not happened yet this year
--   * Feb 29 titles when the current year is not a leap year
--   * Feb 29 titles when the current leap year's Feb 29 has not happened yet
--
-- The search is bounded. 16 years is enough to handle normal leap-year gaps
-- and the Gregorian century exception, e.g. 2096 -> 2104.
inferRelativeToNow :: UTCTime -> (Integer -> Maybe UTCTime) -> Maybe UTCTime
inferRelativeToNow now parseInYear =
    let (currentYear, _, _) = toGregorian $ utctDay now
        candidateYears = take 16 $ iterate (subtract 1) currentYear
        plausible candidate = candidate <= now
    in
    listToMaybe $
        filter plausible $
        mapMaybe parseInYear candidateYears


-- | Choose a plausible year relative to a precise filename timestamp.
--
-- The filename timestamp is the OP creation time, or at least a very good
-- lower bound. The title timestamp is the last-updated time, so it should be
-- greater than or equal to the filename timestamp.
--
-- However, the title timestamp usually only has minute precision. Therefore,
-- if the parsed title time is in the same minute as the filename time, we
-- accept it and later prefer the more precise filename time.
inferRelativeToFile :: UTCTime -> (Integer -> Maybe UTCTime) -> Maybe UTCTime
inferRelativeToFile filenameTime parseInYear =
    let (referenceYear, _, _) = toGregorian $ utctDay filenameTime
        candidateYears = take 16 $ iterate (+ 1) referenceYear

        plausible candidate =
            candidate >= filenameTime
            || sameUpToMinute candidate filenameTime
    in
    listToMaybe $
        filter plausible $
        mapMaybe parseInYear candidateYears


-- | If the candidate matches the reference timestamp up to the minute,
-- return the reference timestamp instead.
--
-- This matters because title timestamps usually lack seconds:
--
--   title:    "Feb 29 20:00"
--   filename: 1583006454.268 == Feb 29 20:00:54.268
--
-- We should prefer the filename timestamp because it is more precise.
preferReferencePrecision :: UTCTime -> UTCTime -> UTCTime
preferReferencePrecision referenceTime candidateTime =
    if sameUpToMinute candidateTime referenceTime
        then referenceTime
        else candidateTime


-- | Parse a Vichan title timestamp into a 'UTCTime'.
--
-- The first argument is the reference time, usually the more precise
-- timestamp parsed from the image filename.
--
-- Examples:
--
--   parseTitleTime referenceUTCTime "Sep 05 21:13"
--   parseTitleTime referenceUTCTime "Feb 29 20:00"
--
-- Vichan title timestamps often omit the year. When the year is missing,
-- we infer it from the reference time’s year.
--
-- Because the title timestamp has only minute precision, if it falls in the
-- same minute as the reference timestamp, we return the reference timestamp.
-- That preserves the more precise seconds/milliseconds from the filename.
--
-- This fixes the leap-day case:
--
--   filename: /b/thumb/1583006454268.jpg
--   title:    "Feb 29 20:00"
--
-- The filename is:
--
--   2020-02-29 20:00:54.268 UTC
--
-- The title parses to:
--
--   2020-02-29 20:00:00 UTC
--
-- These are the same minute, so we return the filename time instead of
-- incorrectly trying 2021 and failing on a non-existent Feb 29.
--
-- Left now:
--   We do not have a precise filename timestamp. Use the fetch/current time
--   only to choose a plausible year. The chosen time must not be in the future.
--
-- Right filenameTime:
--   We have a precise timestamp from the OP image filename. Use it as the
--   reference time and lower bound. If the title matches the filename time
--   up to the minute, prefer the filename time because it has seconds or
--   milliseconds.
parseTitleTime :: Either UTCTime UTCTime -> Text -> Maybe UTCTime
parseTitleTime reference titleText =
    let titleString =
            T.unpack
            $ T.unwords
            $ T.words
            $ T.replace "," "" titleText

        parseWith format input =
            parseTimeM True defaultTimeLocale format input :: Maybe UTCTime

        formatsWithYear =
            [ "%b %d %Y %H:%M"
            , "%B %d %Y %H:%M"
            , "%Y-%m-%d %H:%M"
            , "%b %d %H:%M %Y"
            , "%B %d %H:%M %Y"

            -- Also allow explicit seconds, just in case.
            , "%b %d %Y %H:%M:%S"
            , "%B %d %Y %H:%M:%S"
            , "%Y-%m-%d %H:%M:%S"
            , "%b %d %H:%M:%S %Y"
            , "%B %d %H:%M:%S %Y"
            ]

        formatsWithoutYear =
            [ "%b %d %H:%M"
            , "%B %d %H:%M"
            , "%b %d %H:%M:%S"
            , "%B %d %H:%M:%S"
            ]

        explicitCandidate =
            listToMaybe $
                mapMaybe
                    (\format -> parseWith format titleString)
                    formatsWithYear

        parseInYear year =
            let input = titleString ++ " " ++ show year
                yearFormats = map (++ " %Y") formatsWithoutYear
            in
            listToMaybe $
                mapMaybe
                    (\format -> parseWith format input)
                    yearFormats

    in
    case reference of
        Left now ->
            case explicitCandidate of
                Just explicit
                    -- If the title has an explicit year, trust it only if it
                    -- is not in the future relative to the fallback time.
                    --
                    -- If you want to trust explicit years unconditionally,
                    -- remove this guard and use:
                    --
                    --   Just explicit -> Just explicit
                    | explicit <= now -> Just explicit
                    | otherwise -> Nothing

                Nothing ->
                    inferRelativeToNow now parseInYear

        Right filenameTime ->
            case explicitCandidate of
                Just explicit ->
                    Just $ preferReferencePrecision filenameTime explicit

                Nothing ->
                    preferReferencePrecision filenameTime <$>
                        inferRelativeToFile filenameTime parseInYear


-- | Extract a POSIX timestamp from a Vichan image filename.
--
-- The return type is 'POSIXTime', which is the time library’s fixed-point
-- decimal representation of seconds since the Unix epoch.
parseFilenameTime :: Text -> Maybe POSIXTime
parseFilenameTime source = do
    let filename = last $ T.splitOn "/" source
        digits = T.takeWhile isDigit filename

    guard $ T.length digits >= 10

    let secondsText = T.take 10 digits
        fractionalText = T.drop 10 digits

    seconds <- readMaybe (T.unpack secondsText) :: Maybe Integer

    let secondsPOSIX :: POSIXTime
        secondsPOSIX = fromInteger seconds

    if T.null fractionalText
        then pure secondsPOSIX
        else do
            fractional <- readMaybe (T.unpack fractionalText) :: Maybe Integer

            let fractionalLength = T.length fractionalText
                scale = (10 :: Integer) ^ fractionalLength

                scalePOSIX :: POSIXTime
                scalePOSIX = fromInteger scale

                fractionalPOSIX :: POSIXTime
                fractionalPOSIX = fromInteger fractional / scalePOSIX

            pure $ secondsPOSIX + fractionalPOSIX


-- | Parse a lazy list of RawTokens from strict Text.
-- This uses attoparsec's `match` to capture the verbatim source slice.
parseRawTokens :: Text -> [ RawToken ]
parseRawTokens = unfoldr f
  where
    f :: Text -> Maybe (RawToken, Text)
    f t | T.null t = Nothing
        | otherwise = case A.parse (A.match token) t of
            A.Done rest (raw, tok) -> Just (RawToken raw tok, rest)
            A.Partial cont -> case cont mempty of
                A.Done rest (raw, tok) -> Just (RawToken raw tok, rest)
                _ -> Nothing
            _ -> Nothing


-- | Lenient PStack for RawTokens
data RawPStack = RawPStack
    { _rawToplevelSiblings :: Forest RawToken
    , _rawParents          :: [(RawToken, Forest RawToken)]
    } deriving (Show)


-- | Construct a Forest from RawTokens, gracefully handling mismatched/unclosed tags.
rawTokensToForest :: [RawToken] -> Either ParseTokenForestError (Forest RawToken)
rawTokensToForest = f (RawPStack [] [])
  where
    f (RawPStack ss []) [] =
        Right (reverse ss)

    -- EOF: close any remaining open tags.
    f (RawPStack ss ((p, ss') : ps)) [] =
        f (RawPStack (Node p (reverse ss) : ss') ps) []

    f pstack (t : ts) =
        case tokenVal t of
            TagOpen n _ ->
                if n `elem` nonClosing
                    then f (pushFlatSibling t pstack) ts
                    else f (pushParent t pstack) ts

            TagSelfClose {} ->
                f (pushFlatSibling t pstack) ts

            TagClose n ->
                (`f` ts) =<< popParent n pstack

            _ ->
                f (pushFlatSibling t pstack) ts


pushParent :: RawToken -> RawPStack -> RawPStack
pushParent t (RawPStack ss ps) = RawPStack [] ((t, ss) : ps)


pushFlatSibling :: RawToken -> RawPStack -> RawPStack
pushFlatSibling t (RawPStack ss ps) = RawPStack (Node t [] : ss) ps


popParent :: TagName -> RawPStack -> Either ParseTokenForestError RawPStack
popParent n pstack@(RawPStack _ ps)
    | not (any isTarget ps) = Right pstack
    | otherwise = go pstack
  where
    isTarget (RawToken _ (TagOpen n' _), _) =
        n == n'

    isTarget _ =
        False

    go (RawPStack ss ((p@(RawToken _ (TagOpen n' _)), ss') : rest))
        | n == n' =
            Right $ RawPStack (Node p (reverse ss) : ss') rest

        | otherwise =
            go (RawPStack (Node p (reverse ss) : ss') rest)

    go _ =
        error "popParent: impossible"


utcTimeToEpochSeconds :: UTCTime -> Int
utcTimeToEpochSeconds = truncate . utcTimeToPOSIXSeconds
