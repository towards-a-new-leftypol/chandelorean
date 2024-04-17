{-
   The purpose of this module is to provide a way to store things
   in order (for example as most recent), take the nth item in
   the order out, and put a new item in. In O(log n) time.

   There is also a function to choose a random integer, but favours lower ones.
   Together, this lets us process data in a safe way asynchronously,
   where if we keep choosing a skewed random item to process all of the items
   eventually get processed. No two threads will process the same thing at the
   same time, and more popular things get processed more often.
-}

module PriorityQueue
    ( Elem (..)
    , Queue
    , take
    , put
    , selectSkewedIndex
    )
where

import Prelude hiding (splitAt, take)
import Data.Set hiding (take, foldr, map)
import Data.Ord (comparing)
import System.Random (StdGen, getStdGen, randomR)
import Data.List (sort, group)


data Elem a = Elem
    { priority :: Int
    , element :: a
    }

instance Ord (Elem  a) where
    compare = comparing priority


instance Eq (Elem a) where
    (==) x y = priority x == priority y


type Queue a = Set (Elem a)


take :: Int -> Queue a -> (Elem a, Queue a)
take n set =
    let (_, greater) = splitAt (size set - n - 1) set
        elem = findMin greater
    in (elem, delete elem set)


put :: Elem a -> Queue a -> Queue a
put = insert


-- Simplified function to generate a number linearly skewed towards the start of the range
linearSkewRandom :: Double -> Double -> StdGen -> (Double, StdGen)
linearSkewRandom min max rng =
    let (u, rng') = randomR (0.0, 1.0) rng
        -- skewedValue = min + (u ** 2) * (max - min)
        skewedValue = (min - 0.5) + (u ** 2) * (max - min + 0.5)
        -- skewedValue = (min - 0.5) + (1 - sqrt u) * (max - min + 0.5)
        -- skewedValue = min + (1 - sqrt u) * (max - min)
    in (skewedValue, rng')


-- Function to select an index from 0 to n-1 with a linear skew towards lower numbers
selectSkewedIndex :: Int -> StdGen -> (Int, StdGen)
selectSkewedIndex n rng =
    let max = fromIntegral (n - 1)
        (randValue, newRng) = linearSkewRandom 0 max rng
    in (ceiling randValue, newRng)


main :: IO ()
main = do
    stdGen <- getStdGen
    --putStrLn "Hello World"

    -- let i = fst $ selectSkewedIndex (size q) stdGen
    -- let x = fst $ take i q
    -- print (i, priority x)

    let rs = foldr f ([], stdGen) [1..100000]
    mapM_ pf $ countOccurrences $ fst rs

    where
        pf :: (Show a, Show b) => (a, b) -> IO ()
        pf (a, b) = putStrLn $ (show a) ++ "," ++ (show b)

        f _ (xs, gen) =
            let (x, newgen) = selectSkewedIndex (size q) gen
            in (x:xs, newgen)

        q :: Queue Int
        q = fromList [ Elem i undefined | i <- [1..100] ]

countOccurrences :: (Eq a, Ord a) => [a] -> [(a, Int)]
countOccurrences rolls = map (\x -> (head x, length x)) . group . sort $ rolls


