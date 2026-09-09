{-# LANGUAGE OverloadedStrings #-}

module HtmlParsingUtils where

import Data.Text (Text)
import qualified Data.Text as T
import Data.Tree
import Data.Char (isDigit)
import Data.Maybe (mapMaybe, listToMaybe)
import Data.Time
import Data.Time.Format
import Data.Time.Clock.POSIX
import Text.Read (readMaybe)
import Control.Monad (guard)

import Text.HTML.Parser
import Text.HTML.Tree

{-
 - NAVIGATION
 -}

-- | Find all descendant nodes (Trees) that possess a given CSS class.
-- We search the 'Forest' (list of trees) recursively.
findByClass :: Text -> Forest Token -> [Tree Token]
findByClass targetClass forest = concatMap go forest
  where
    go node@(Node token children)
        | hasTokenClass targetClass token = node : findByClass targetClass children
        | otherwise                       = findByClass targetClass children

-- | Find the first descendant node with a given CSS class.
findFirstByClass :: Text -> Forest Token -> Maybe (Tree Token)
findFirstByClass targetClass = listToMaybe . findByClass targetClass

-- | Find all descendant nodes with a specific HTML tag name.
findByTag :: Text -> Forest Token -> [Tree Token]
findByTag targetTag forest = concatMap go forest
  where
    go node@(Node token children)
        | getTagName token == Just targetTag = node : findByTag targetTag children
        | otherwise                          = findByTag targetTag children

-- | Get only the element children of a node, ignoring text/comments.
getChildElements :: Tree Token -> [Tree Token]
getChildElements (Node _ children) = filter isElement children
  where
    isElement (Node token _) = case token of
        TagOpen{}      -> True
        TagSelfClose{} -> True
        _              -> False

{-
 - INSPECTION (Operating on 'Token')
 -}

-- | Safely extract an attribute value from a Token.
-- | Safely extract an attribute value from a Token.
getAttribute :: Text -> Token -> Maybe Text
getAttribute attrName token = case token of
    TagOpen _ attrs      -> findAttr attrs
    TagSelfClose _ attrs -> findAttr attrs
    _                    -> Nothing
  where
    -- Recursively search the list of custom 'Attr' types
    findAttr [] = Nothing
    findAttr (Attr key value : rest)
        | key == attrName = Just value
        | otherwise       = findAttr rest

-- | Check if a Token has a specific CSS class.
hasTokenClass :: Text -> Token -> Bool
hasTokenClass targetClass token = 
    maybe False (elem targetClass . T.words) (getAttribute "class" token)

-- | Get the tag name of a Token.
getTagName :: Token -> Maybe Text
getTagName token = case token of
    TagOpen name _      -> Just name
    TagSelfClose name _ -> Just name
    _                   -> Nothing

{-
 - TEXT EXTRACTION
-}

-- | Recursively extract and concatenate all text content from a Forest.
extractText :: Forest Token -> Text
extractText forest = T.concat $ map go forest
  where
    go (Node token children) = case token of
        ContentText t -> t
        ContentChar c -> T.singleton c
        _             -> extractText children

-- | Extract text, but skip any subtrees rooted at a node with the excluded class.
extractTextExcluding :: Text -> Forest Token -> Text
extractTextExcluding excludedClass forest = T.concat $ mapMaybe go forest
  where
    go node@(Node token children)
        | hasTokenClass excludedClass token = Nothing -- Skip this entire subtree
        | isText token                      = Just (getText token)
        | otherwise                         = Just (extractTextExcluding excludedClass children)
    
    isText ContentText{} = True
    isText ContentChar{} = True
    isText _             = False
    
    getText (ContentText t) = t
    getText (ContentChar c) = T.singleton c
    getText _               = T.empty

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
