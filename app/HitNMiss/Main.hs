{-# LANGUAGE BangPatterns #-}

module Main (main) where

import Control.Monad (replicateM)
import Control.Monad.MonteCarlo
import Control.Parallel (par)
import Control.Parallel.Strategies
import Criterion.Main
import Data.Summary.Bool
import System.Environment (getArgs)
import System.Random hiding (randomR)

import Data.List (intercalate)
import Data.Proxy (Proxy)
import Data.Result (Result (Obs))
import Options.Applicative

data HitnMissOptions = HitnMissOptions
  {number :: Int}

hitnMissOptionsParser :: Parser HitnMissOptions
hitnMissOptionsParser =
  HitnMissOptions
    <$> option
      auto
      ( long "number"
          <> short 'n'
          <> metavar "INT"
          <> help "Number of iterations"
      )

readMethod :: ReadM Char
readMethod = eitherReader $ \arg ->
  case arg of
    [c] | c `elem` "ABC" -> Right c
    _ -> Left "Method must be 'A', 'B', or 'C'"

-- | Check if a point (x, y) is inside the unit circle.
inCircle :: (Double, Double) -> Bool
inCircle (x, y) = x * x + y * y <= 1.0

-- | Group a list into (x, y) pairs.
pairUp :: [a] -> [(a, a)]
pairUp [] = []
pairUp (x : y : xs) = (x, y) : pairUp xs
pairUp _ = [] -- Should not occur if the list has even length

-- | Sequential simulation of π estimation.
simulatePi :: Int -> IO Double
simulatePi totalSamples = do
  gen <- getStdGen
  let randomNumbers = take (2 * totalSamples) $ randomRs (0.0, 1.0) gen :: [Double]
      points = pairUp randomNumbers
      inside = length $ filter inCircle points
      piEstimate = 4 * fromIntegral inside / fromIntegral totalSamples
  return piEstimate

dist2DUniform :: (RandomGen g) => MonteCarlo g (Double, Double)
dist2DUniform = (,) <$> randomR (-1, 1) <*> randomR (-1, 1)

inUnitCircle :: (RandomGen g) => MonteCarlo g Bool
inUnitCircle = do
  x <- randomR (-1, 1)
  y <- randomR (-1, 1)
  return $ x * x + y * y <= (1.0 :: Double)

trials :: Int
trials = 10000000000

parallelPi :: Int -> IO Double
parallelPi n = do
  g <- getStdGen
  let result = experimentP inUnitCircle n (n `div` 200) g :: BoolSumm
  let (mean, stderr) = (sampleMean result, sampleSE result)
  return mean

a n = (4 *) <$> ($ (experimentP inUnitCircle n (if n >= 200 then n `div` 200 else 1) (mkStdGen 40) :: BoolSumm)) <$> [sampleMean, sampleSE]

-- >>> (4*) <$> ($ (experimentS inUnitCircle 10000000 (mkStdGen 40) :: BoolSumm)) <$> [sampleMean, sampleSE]
-- [3.141748,5.192698243202661e-4]
--

-- | Run benchmarks using Criterion.
runBenchmarks :: IO ()
runBenchmarks =
  defaultMain
    [ bench "Sequential Pi" $ nfIO (simulatePi totalSamples)
    , bench "Parallel Pi" $ nfIO (parallelPi totalSamples)
    ]
 where
  totalSamples = trials

-- | Run example estimations and print results.
runExamples :: IO ()
runExamples = do
  let totalSamples = trials
  -- Sequential estimation:
  piSeq <- simulatePi totalSamples
  putStrLn $ "Sequential Pi estimate: " ++ show piSeq

  -- Parallel estimation:
  piPar <- parallelPi totalSamples
  putStrLn $ "Parallel Pi estimate: " ++ show piPar

-- Main

main :: IO ()
main = do
  options <- execParser opts
  result <- parallelPi (number options)
  let format = intercalate ", " [show $ number options, show $ result * 4]
  putStrLn format
 where
  opts =
    info
      (hitnMissOptionsParser <**> helper)
      ( fullDesc
          <> progDesc "Estimate pi using Hit-n-Miss method"
          <> header "pi estimate"
      )
