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

type Amplitude = Float
type Frequency = Float
-- Used to make sine waves in the expression Amplitude * sin(Frequency * t) (i.e. A*sin(w * t)
type Harmonic = (Frequency, Amplitude)

-- A*sin(n * t) is a function describing a sine wave that can be:
--  - Stretched vertically by A
--  - Stretched horizontally by n

-- Equivalent to Σ^N_{i=1} A_i*sin(n_i * t) where N is the number of lists
makeHarmonicWave :: [Harmonic] -> WaveForm
makeHarmonicWave hs t = sum [amp * sin (n * t) | (n, amp) <- hs]

-- To see the instrument waves visually, use this Desmos snapshot: https://www.desmos.com/calculator/ma5dimdqf1
clarinetWave :: WaveForm
clarinetWave = makeHarmonicWave [(1, 1.0), (3, 0.3), (5, 0.1), (7, 0.05)]

pianoWave :: WaveForm
pianoWave = makeHarmonicWave [(1, 1.0), (2, 0.7), (3, 0.4), (4, 0.3), (5, 0.15), (6, 0.1)]

guitarWave :: WaveForm
guitarWave = makeHarmonicWave [(1, 1.0), (2, 0.8), (3, 0.4), (4, 0.2), (5, 0.1)]