module Synth.Oscillator where

import Synth.Types
import Synth.Constants

oscillator :: Hz -> WaveForm -> Seconds -> Audio
oscillator hz wf t = map (wf . (*step)) [0.0 .. sampleRate * t]
    where
        step = (2 * pi * hz) / sampleRate