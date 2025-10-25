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
The seed is currently based on t, which is probably very bad but it kind of
works!

Also this "random noise" uses a pseudorandom number generator so it will
always have the same value for each sample taken at the same time
-}
randomFloat :: Float -> (Float, StdGen)
randomFloat t = randomR (-1.0 :: Float, 1.0 :: Float) gen
                where gen = mkStdGen (round ((t * t) / 67))

pianoWave :: WaveForm
pianoWave t = - (1 / 4 * sin (3 * pi * t)) + 1 / 4 * sin (pi * t) + sqrt 3 / 2 * cos (pi * t)

warmSynth :: WaveForm
warmSynth t = 0.5 * sinWave t + 0.3 * sawtoothWave t + 0.2 * triangleWave t