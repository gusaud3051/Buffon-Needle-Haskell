module Ising.Data where

import qualified Data.Vector.Unboxed as V
import Control.Monad.Trans.State.Strict (StateT)
import System.Random (StdGen)

-- Constants
l :: Int
l = 10 -- Grid size L x L

n :: Int
n = l * l -- Total number of spins

jParam :: Double
jParam = -1.0 -- Coupling constant : < 0 => Antiferromagnetic

bParam :: Double
bParam = 1.0 -- External magnetic field

k_B :: Double
k_B = 1.0 -- Boltzmann constant (set to 1)

temp :: Double
temp = 1.4 -- Temperature

mcsSteps :: Int
mcsSteps = 1000 -- Max Number of Monte Carlo Steps (MCS)

checkWindow :: Int
checkWindow = 100 -- Check stability over the last 100 steps (adjust as needed)

stabilityThreshold :: Double
stabilityThreshold = 0.05 -- Stop if energy std dev < threshold (adjust as needed, lower for stricter stability)

energyFilename :: String
energyFilename = "ising_energy_data.dat"

latticeFilename :: String
latticeFilename = "ising_lattice_final.dat"

-- Simulation State: Lattice configuration (Vector), Random Generator
type Lattice = V.Vector Double
data SimState = SimState {
    lattice :: !Lattice,
    rng     :: !StdGen,
    energy  :: !Double
}

-- Type alias for our simulation monad
type SimM = StateT SimState IO
