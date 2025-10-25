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

-- The seed is currently based on t, which is probably very bad but it kind of works!
randomFloat :: Float -> (Float, StdGen)
randomFloat t = randomR (-1.0 :: Float, 1.0 :: Float) gen
                where gen = mkStdGen (round ((t * t) / 67))

--- Testing AI Response ---

type Harmonic = (Float, Float)

makeHarmonicWave :: [Harmonic] -> WaveForm
makeHarmonicWave hs t = sum [amp * sin (n * t) | (n, amp) <- hs]

-- A warm, mellow sound like a clarinet
clarinetWave :: WaveForm
clarinetWave = makeHarmonicWave [(1, 1.0), (3, 0.3), (5, 0.1), (7, 0.05)]

-- A bright, metallic sound like a piano
pianoWave :: WaveForm
pianoWave = makeHarmonicWave [(1, 1.0), (2, 0.7), (3, 0.4), (4, 0.3), (5, 0.15), (6, 0.1)]

-- A guitar-like waveform (plucked string has strong even and odd harmonics)
guitarWave :: WaveForm
guitarWave = makeHarmonicWave [(1, 1.0), (2, 0.8), (3, 0.4), (4, 0.2), (5, 0.1)]
