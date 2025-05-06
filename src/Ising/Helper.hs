{-# LANGUAGE ScopedTypeVariables #-}
module Ising.Helper where

import qualified Data.Vector.Unboxed as V
import qualified Data.Vector.Unboxed.Mutable as VM
import System.Random (StdGen, randomR, randomIO)
import Control.Monad (replicateM_, forM_)
import Control.Monad.ST (ST, runST)
import Control.Monad.Primitive (PrimMonad, PrimState)
import Control.Monad.Trans.State.Strict (gets, modify')
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.List (genericLength)
import Text.Printf (printf)

import Ising.Data

-- Calculates standard deviation
calculateStdDev :: [Double] -> Double
calculateStdDev xs
  | n <= 1    = 1.0 / 0.0 -- Return Infinity if std dev is undefined (<= 1 point)
  | otherwise = sqrt $ variance xs
  where
    n :: Double
    n = genericLength xs -- Use genericLength for Double count
    meanVal :: Double
    meanVal = sum xs / n
    variance :: [Double] -> Double
    variance vs = sum (map (\x -> (x - meanVal) ** 2) vs) / n

-- 1. Initialize Lattice (Random Spins) using Vector
initializeLattice :: Int -> StdGen -> (Lattice, StdGen)
initializeLattice size gen = runST $ do
    mvec <- VM.new (size * size)
    let fill index g
          | index >= size * size = return g
          | otherwise = do
              let (val, g') = randomR (0 :: Int, 1) g
              VM.write mvec index (if val == 0 then -1.0 else 1.0)
              fill (index + 1) g'
    finalGen <- fill 0 gen
    frozenVec <- V.unsafeFreeze mvec
    return (frozenVec, finalGen)

-- Helper to get spin at (r, c) with periodic boundary conditions
getSpin :: Lattice -> Int -> Int -> Double
getSpin lat r c = lat V.! index
  where
    wrap x = (x + l) `mod` l
    index = wrap r * l + wrap c

-- 3. Sum of Nearest Neighbors (using Vector access)
sumNearestNeighbors :: Lattice -> Int -> Int -> Double
sumNearestNeighbors lat r c =
    getSpin lat (r+1) c + getSpin lat (r-1) c +
    getSpin lat r (c+1) + getSpin lat r (c-1)

-- 2. Calculate Total Energy (Python Style for consistency)
totalEnergy :: Lattice -> Double -> Double -> Double
totalEnergy lat jConst bConst = totalRawEnergy / 2.0
  where
    totalRawEnergy = V.sum $ V.imap siteContribution lat
    siteContribution idx spin = -jConst * spin * nnSum - bConst * spin
      where
        r = idx `div` l
        c = idx `mod` l
        nnSum = sumNearestNeighbors lat r c

-- Helper function to get a random int in range [0, high-1] within SimM
getRandomInt :: Int -> SimM Int
getRandomInt high = do
    gen <- gets rng
    let (randVal, newGen) = randomR (0, high - 1) gen
    modify' (\s -> s { rng = newGen })
    return randVal

-- Helper function to get a random double in range [0, 1) within SimM
getRandomDouble :: SimM Double
getRandomDouble = do
    gen <- gets rng
    let (randVal, newGen) = randomR (0.0, 1.0) gen
    modify' (\s -> s { rng = newGen })
    return randVal

-- 4. Metropolis MCMC Simulation Step (one spin flip attempt)
runMetropolisStep :: SimM ()
runMetropolisStep = do
    r <- getRandomInt l
    c <- getRandomInt l
    let vecIndex = r * l + c
    lat <- gets lattice
    currentEnergy <- gets energy
    let currentSpin = lat V.! vecIndex
    let nnSum = sumNearestNeighbors lat r c
    let deltaE = 2.0 * currentSpin * (jParam * nnSum + bParam)
    accept <- if deltaE <= 0.0 then
                  return True
              else do
                  let acceptanceProb = exp (-deltaE / (k_B * temp))
                  randDouble <- getRandomDouble
                  return (randDouble < acceptanceProb)
    when accept $ do
        let newLattice = lat V.// [(vecIndex, -currentSpin)]
        let newEnergy = currentEnergy + deltaE
        modify' (\s -> s { lattice = newLattice, energy = newEnergy })

-- Run one full Monte Carlo Step (MCS) = N flip attempts
runMCS :: SimM ()
runMCS = replicateM_ n runMetropolisStep

-- Main simulation loop function - Modified for early stopping
simulateIsing :: SimM [Double] -- Returns energy history up to stop point
simulateIsing = do
    initialEnergy <- gets energy
    -- Start loop with initial energy in history and recent list
    loop mcsSteps [initialEnergy] [initialEnergy]
  where
    -- loop function signature: steps_remaining, full_history_accum, recent_W_energies
    loop :: Int -> [Double] -> [Double] -> SimM [Double]
    loop 0 energies _ = do -- Base case: Max steps reached
        liftIO $ putStrLn $ "Reached maximum steps (" ++ show mcsSteps ++ ")."
        return (reverse energies) -- Return accumulated history
    loop k fullHistory recentEnergies = do
        runMCS -- Perform one MCS
        currentEnergy <- gets energy
        let newFullHistory = currentEnergy : fullHistory
        -- Update recent energies: add new, keep only last W
        let updatedRecent = take checkWindow (currentEnergy : recentEnergies)
        let currentStep = mcsSteps - k + 1

        -- Check stopping condition ONLY if we have enough data points in the window
        shouldStop <- if length updatedRecent < checkWindow then
                          return False -- Not enough data yet
                      else do
                          let stdDev = calculateStdDev updatedRecent
                          -- Optional Debug Print:
                          -- liftIO $ printf "Debug: Step %d, Recent Energies Count: %d, StdDev: %.6f\n" currentStep (length updatedRecent) stdDev
                          return (stdDev < stabilityThreshold)

        if shouldStop then do
            liftIO $ printf "Equilibrium criterion met at step %d (Energy StdDev %.4f < %.4f). Stopping early.\n" currentStep (calculateStdDev updatedRecent) stabilityThreshold
            return (reverse newFullHistory) -- Return history up to this point
        else
            loop (k-1) newFullHistory updatedRecent -- Continue simulation


when :: Monad m => Bool -> m () -> m ()
when True action = action
when False _     = return ()
