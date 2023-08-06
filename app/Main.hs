{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE DisambiguateRecordFields #-}

module Main where

import Data.Tuple
import Data.Foldable
import Data.Time.Clock

data Sample
  = Sample
      { ts  :: UTCTime
      , val :: Integer }
  deriving Show

data Counter
  = Counter
    { sample :: Sample
    , cumsum :: Integer }
  deriving Show

type GroupFn a
  = [a] -> [[a]]

rangeGroup :: NominalDiffTime -> UTCTime -> GroupFn Sample
rangeGroup r s = intervalGroup intervals
  where
    intervalGroup (i:is) = uncurry (flip (:) . intervalGroup is) . swap . span ((>= i) . ts)
    intervals = map (((flip addUTCTime) s) . (* r)) [-1,-2..]

cons :: Sample -> [Counter] -> [Counter]
x `cons` [] = [Counter x (val x)]
x `cons` (y:ys) | (val x) >= (val $ sample y) = (Counter x ((cumsum y) - (val $ sample y) + (val x))):(y:ys)
                | otherwise = (Counter x ((cumsum y) + (val x))):(y:ys)

main :: IO ()
main = do
  curTime <- getCurrentTime

  let sec2 = secondsToNominalDiffTime (-2)
  let sec10 = secondsToNominalDiffTime (-10)
  
  let t1 = curTime
  let t2 = addUTCTime sec2 curTime
  let t3 = addUTCTime sec10 curTime

  let ms = [Sample t1 100, Sample t2 95, Sample t3 53]

  let sec5 = secondsToNominalDiffTime 5

  let ms' = rangeGroup sec5 (ts $ head ms) ms
  print ms
  print $ take 5 ms'

