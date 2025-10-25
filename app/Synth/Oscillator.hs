module Synth.Oscillator where

import Synth.Types
import Synth.Constants
import Synth.WaveForm


oscillator :: Hz -> WaveForm -> Seconds -> Audio
oscillator hz wf t = map (wf . (*step)) [0.0 .. sampleRate * t]
    where
        step = (2 * pi * hz) / sampleRate

combineWave :: Audio -> Audio -> Audio
combineWave = zipWith (\a b -> max (-1) (min 1 ((a + b) / 2)))

bullshit :: Audio
bullshit = combineWave (oscillator pitchStd noiseWave 1) (oscillator pitchStd sinWave 1) ++ combineWave (oscillator (pitchStd*2) noiseWave 1) (oscillator (pitchStd*2) sinWave 1)

bullshit2 :: Audio
bullshit2 = combineWave (oscillator pitchStd sinWave 2) (oscillator pitchStd sinWave 1)

bullshit3 :: Audio
bullshit3 = oscillator pitchStd warmSynth 2