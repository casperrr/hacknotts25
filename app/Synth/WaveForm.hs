module Synth.WaveForm where

import Synth.Types

sinWave :: WaveForm
sinWave = sin

squareWave :: WaveForm
squareWave t = if sin t >= 0 then 1 else -1

sawtoothWave :: WaveForm
sawtoothWave t = 2 * (t - fromIntegral (floor (1/2 + t)))

triangleWave :: WaveForm
triangleWave t = 2 * abs (2 * (t - fromIntegral (floor (t + 1/2)))) - 1

noiseWave :: WaveForm
noiseWave t = undefined