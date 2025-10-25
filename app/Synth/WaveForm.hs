module Synth.WaveForm where

import Synth.Types
import System.Random

sinWave :: WaveForm
sinWave = sin

squareWave :: WaveForm
squareWave t = if sin t >= 0 then 1 else -1

sawtoothWave :: WaveForm
sawtoothWave t = 2 * (t - fromIntegral (floor (1/2 + t)))

triangleWave :: WaveForm
triangleWave t = 2 * abs (2 * (t - fromIntegral (floor (t + 1/2)))) - 1

noiseWave :: WaveForm
noiseWave t = fst (randomFloat t)

{- 
    TODO Find a way to use the monadic RNG to get numbers instead of this, the
         function is determinsitic so for the same time, it will generate the
         same value
-}
randomFloat :: Float -> (Float, StdGen)
randomFloat t = randomR (-1.0 :: Float, 1.0 :: Float) gen
                where gen = mkStdGen (round ((t * t) / 67))