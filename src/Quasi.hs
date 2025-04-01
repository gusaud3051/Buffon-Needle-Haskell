module Quasi where

import System.Random (randomRIO)
import Control.Monad (replicateM)

-- Define the boundary function u(1,theta)
f :: Double -> Double
f theta = 2 + 3 * cos theta + cos (2 * theta)

-- Generate N random angles in [0, 2*pi)
generateRandomAngles :: Int -> IO [Double]
generateRandomAngles nSamples = replicateM nSamples (randomRIO (0, 2*pi))

-- Compute the average value of the boundary function
computeAverage :: [Double] -> Double
computeAverage angles = sum (map f angles) / fromIntegral (length angles)

-- >>> generateRandomAngles 10

-- Generate the n-th term of the binary Van der Corput sequence (n >= 1)
vanDerCorput :: Int -> Double
vanDerCorput n = go n 0 1
  where
    go 0 acc _     = acc
    go x acc denom =
      let bit  = x `mod` 2        -- least significant bit
          x'   = x `div` 2        -- shift down
          acc' = acc + fromIntegral bit / (2 * denom)
      in go x' acc' (denom * 2)

-- Generate N angles using Van der Corput sequence scaled to [0, 2*pi)
generateVDCAngles :: Int -> [Double]
generateVDCAngles nSamples = [ 2 * pi * vanDerCorput i | i <- [1..nSamples] ]

-- True center value
trueCenter :: Double
trueCenter = 2.0

computeError :: Double -> Double
computeError avg = abs (avg - trueCenter)

-- Main simulation for a given number of samples
simulate :: Int -> IO ()
simulate nSamples = do
  anglesRandom <- generateRandomAngles nSamples
  let avgRandom = computeAverage anglesRandom
      errorRandom = computeError avgRandom

      anglesVDC = generateVDCAngles nSamples
      avgVDC = computeAverage anglesVDC
      errorVDC = computeError avgVDC

  putStrLn $ "Samples: " ++ show nSamples
  putStrLn $ "Pseudo-Random Error: " ++ show errorRandom
  putStrLn $ "Van der Corput Error: " ++ show errorVDC
