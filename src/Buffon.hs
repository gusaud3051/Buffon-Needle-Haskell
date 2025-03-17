module Buffon (module Sample, buffonStep, simulateBuffon, runBuffonA, runBuffonB, runBuffonC) where

import Control.Monad (when)
import Control.Monad.IO.Class
import Sample (genCosSin, genMarsaglia, genSin, uniform0lGen)
import System.Random
import System.Random.Stateful (StatefulGen, newIOGenM)

{- | Buffon's needle simulation step.
needleLength: length of the needle
lineSpacing: spacing between parallel lines
Returns True if the needle crosses a line, False otherwise.
-}
buffonStep :: (StatefulGen g m) => Double -> Double -> g -> m Bool
buffonStep needleLength lineSpacing g = do
  -- Generate sine value for the needle's orientation.
  sin_sampled <- genSin g
  -- Generate the distance from the needle's center to the nearest line.
  x <- uniform0lGen (lineSpacing / 2) g
  let threshold = (needleLength / 2) * abs sin_sampled
  return (x <= threshold)

{- | Runs the Buffon's needle simulation for n steps.
After each step, it updates the estimate for pi.
The formula: hit probability = (2 * needleLength) / (pi * lineSpacing),
so rearranging yields \(pi \simeq (2 * needleLength * steps) / (lineSpacing * hits) \).
-}
simulateBuffon ::
  (StatefulGen g m, MonadIO m) =>
  -- | Number of simulation steps.
  Int ->
  -- | Needle length.
  Double ->
  -- | Line spacing.
  Double ->
  -- | Switch for stdout
  Bool ->
  -- | Random generator.
  g ->
  -- | Final estimate for pi.
  m Double
simulateBuffon n needleLength lineSpacing sout g = loop 0 0
 where
  loop i hits
    | i == n = do
        let finalEstimate =
              if hits == 0
                then 0
                else (2 * needleLength * fromIntegral n) / (lineSpacing * fromIntegral hits)
        return finalEstimate
    | otherwise = do
        hit <- buffonStep needleLength lineSpacing g
        let hits' = if hit then hits + 1 else hits
            currentStep = i + 1
            estimate =
              if hits' == 0
                then 0
                else
                  (2 * needleLength * fromIntegral currentStep)
                    / (lineSpacing * fromIntegral hits')
        liftIO . when sout . putStrLn $ "Step " <> show currentStep <> ": current pi estimate = " <> show estimate
        loop (i + 1) hits'

runBuffonA :: Int -> IO Double
runBuffonA n = do
  -- standard psdudo, time-invarient random generator.
  let pureGen = mkStdGen 42 -- period: 2^64
  g <- newIOGenM pureGen
  pi_estimated <- simulateBuffon n 1.0 2.0 True g
  return pi_estimated

-- | same as A, but it is more silent than A.
runBuffonC :: Int -> IO Double
runBuffonC n = do
  -- standard psdudo, time-invarient random generator.
  let pureGen = mkStdGen 42 -- period: 2^64
  g <- newIOGenM pureGen
  pi_estimated <- simulateBuffon n 1.0 2.0 False g
  return pi_estimated

---------------------------------------------------------------------------------------------------

-- | Below is another implementation of Buffon's needle, inspired by https://programmingpraxis.com/2013/03/15/buffons-needle/
x_rnds :: IO [Double]
x_rnds = fmap randoms newStdGen

-- | Generate an infinite list of abs . sine values.
genAbsSinIO :: IO [Double]
genAbsSinIO = do
  g <- newStdGen -- changes every time for each run
  let xs = randoms g :: [Double]
  return (go xs)
 where
  go :: [Double] -> [Double]
  go (x : y : xs) =
    let s = x * x + y * y
     in if (0 < s && s < 1)
          then
            let r_inv = sqrt (1 / s)
                sine = abs (y * r_inv)
             in sine : go xs
          else go xs
  go _ = [] -- in case the list has fewer than 2 elements

runBuffonB :: Int -> IO Double
runBuffonB n = do
  ys <- x_rnds -- needle center distances, uniform in [0,1]
  ss <- genAbsSinIO -- sine values from Marsaglia polar method (in [0,1])
  let hits = sum . take n $ zipWith (\y s -> if y < s / 2 then 1 else 0) ys ss
  return (fromIntegral n / fromIntegral hits)
