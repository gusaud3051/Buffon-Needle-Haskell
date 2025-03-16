module Sample (genMarsaglia, genCosSin, genSin, uniform0lGen) where

import Statistics.Distribution (ContGen (genContVar))
import Statistics.Distribution.Uniform (
  UniformDistribution,
  uniformDistr,
 )
import System.Random.Stateful (StatefulGen)

{- | To sample sin/cos values uniformly, we can use a normal distribution, but its expansion formula contains pi.
So we need another approximation.
-}

{- | Method 1.
Prepare \( X_i \~ Bin(n,1/2)\) for \( i = 1, \ldots, n \) for sufficiently large \(n > 100000000\),
and define an approximation of normal distibution by
\[ \sum_{i=1,\ldots, n} X_i / \sqrt{n} \to N(0,1) \],
without using \(\pi\).
-}

{- | Method 2.
Use Marsaglia Polar Method.
Step 1. Sample \( (x, y) \in (-1,1) \times (-1,1) \) until \( 0 < s = x^2 + y^2 <1 \).
Step 2. \[ x' = x \sqrt{\frac{\-2\ln{s}}{s}},\quad y' = x \sqrt{\frac{\-2\ln{s}}{s}} \]
Step 3. \( x', y' \~ N(0, 1) \)
-}

-- | Uniform Distribution \( [0, 1] \)
uniformDistr0l :: Double -> UniformDistribution
uniformDistr0l l = uniformDistr 0 l

-- | Uniform Distribution \( [-1, 1] \)
uniformDistrPM1 :: UniformDistribution
uniformDistrPM1 = uniformDistr (-1) 1

-- | Random number generator for the uniform distribution above.
uniform0lGen :: (StatefulGen g m) => Double -> g -> m Double
uniform0lGen l = genContVar $ uniformDistr0l l

-- | Random number generator for the uniform distribution above.
uniformPM1Gen :: (StatefulGen g m) => g -> m Double
uniformPM1Gen = genContVar uniformDistrPM1

genMarsaglia :: (StatefulGen g m) => g -> m (Double, Double)
genMarsaglia = \g -> do
  x <- uniformPM1Gen g
  y <- uniformPM1Gen g
  let s = x * x + y * y
  if (0 < s && s < 1)
    then
      let r = sqrt (-2 * log s / s)
       in return (x * r, y * r)
    else genMarsaglia g

-- | As we only need sin and cos values, normalized values will be sufficient.
genCosSin :: (StatefulGen g m) => g -> m (Double, Double)
genCosSin = \g -> do
  x <- uniformPM1Gen g
  y <- uniformPM1Gen g
  let s = x * x + y * y
  if (0 < s && s < 1)
    then
      let r_inv = sqrt (1 / s)
       in return (x * r_inv, y * r_inv)
    else genCosSin g

-- |  we only really need a sin value.
genSin :: (StatefulGen g m) => g -> m Double
genSin = \g -> do
  x <- uniformPM1Gen g
  y <- uniformPM1Gen g
  let s = x * x + y * y
  if (0 < s && s < 1)
    then
      let r_inv = sqrt (1 / s)
       in return $ y * r_inv
    else genSin g
