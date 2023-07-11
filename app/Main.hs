module Main where

import Data.Tuple
import Data.Foldable
import Data.Time.Clock

data MetricPoint
  = Count
      { ts  :: UTCTime
      , val :: Integer }
  | Gauge
      { ts  :: UTCTime
      , val :: Integer }
  deriving Show

type GroupFn
  = [MetricPoint] -> [[MetricPoint]]

rangeGroup :: NominalDiffTime -> UTCTime -> GroupFn
rangeGroup r s ms = intervalGroup (intervals r s) ms
  where
    intervalGroup (r:rs) xs = uncurry (flip (:) . intervalGroup rs) $ swap $ span ((>= r) . ts) xs
    intervals r s = map (((flip addUTCTime) s) . (* r)) [-1,-2..]

main :: IO ()
main = do
  curTime <- getCurrentTime

  let sec2 = secondsToNominalDiffTime (-2)
  let sec10 = secondsToNominalDiffTime (-10)
  
  let t1 = curTime
  let t2 = addUTCTime sec2 curTime
  let t3 = addUTCTime sec10 curTime

  let ms = [Count t1 100, Count t2 95, Count t3 53]

  let sec5 = secondsToNominalDiffTime 5

  let ms' = rangeGroup sec5 (ts $ head ms) ms
  print ms
  print $ take 5 ms'

