module Main where

import Text.Printf (printf)
import System.IO (withFile, hPutStr, hPutStrLn, IOMode(WriteMode))
import Control.Monad (forM_)
import System.Random (getStdGen)
import Control.Monad.Trans.State.Strict (runStateT)

import Ising.Data
import Ising.Helper

main :: IO ()
main = do
    putStrLn $ "Starting Ising simulation... Max steps: " ++ show mcsSteps
    putStrLn $ "Stability check window: " ++ show checkWindow ++ " steps, Threshold (StdDev): " ++ show stabilityThreshold
    putStrLn $ "Output energy file: " ++ energyFilename
    putStrLn $ "Output lattice file: " ++ latticeFilename

    -- Get initial generator from the system's global IO generator
    initialStdGen <- getStdGen

    let (initialLattice, genAfterInit) = initializeLattice l initialStdGen
    let initialEnergy = totalEnergy initialLattice jParam bParam
    let initialState = SimState { lattice = initialLattice, rng = genAfterInit, energy = initialEnergy }

    -- Run simulation (might stop early)
    (energyHistory, finalState) <- runStateT simulateIsing initialState
    let finalLattice = lattice finalState
    let finalSystemEnergy = energy finalState -- Get the energy corresponding to the final lattice state

    -- Save the energy history (might be shorter than mcsSteps)
    putStrLn $ "Saving energy history (" ++ show (length energyHistory) ++ " steps) to " ++ energyFilename
    withFile energyFilename WriteMode $ \hFile -> do
        forM_ (zip [0..] energyHistory) $ \( (step :: Int), e) ->
             hPutStrLn hFile $ printf "%d %.4f" step e
    putStrLn "Energy history saved."

    -- >> New: Save the final lattice configuration
    putStrLn $ "Saving final lattice configuration to " ++ latticeFilename
    withFile latticeFilename WriteMode $ \hFile -> do
        forM_ [0..l-1] $ \r -> do -- Iterate rows
            forM_ [0..l-1] $ \c -> do -- Iterate columns
                let spin = getSpin finalLattice r c
                -- Write spin (e.g., "-1.0" or " 1.0") followed by a space
                -- %4.1f ensures consistent spacing (adjust width '4' if needed)
                hPutStr hFile $ printf "%4.1f " spin
            hPutStrLn hFile "" -- Newline after each row
    putStrLn "Lattice saved."
    -- << End New

    -- Use the energy from the actual final state
    putStrLn $ "Final System Energy: " ++ show finalSystemEnergy
