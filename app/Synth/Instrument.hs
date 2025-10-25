module Synth.Instrument where

import Synth.Types
import Synth.WaveForm
import Synth.Envelope

data Instrument = Instrument {
    waveform :: WaveForm,
    envelope :: ADSR,
    volume :: Volume
}

-- Predefined instruments
piano :: Instrument
piano = Instrument sinWave (ADSR 0.01 0.1 0.7 0.3) 0.8

organ :: Instrument
organ = Instrument sinWave (ADSR 0.01 0.0 1.0 0.1) 0.7

lead :: Instrument
lead = Instrument sawtoothWave (ADSR 0.05 0.2 0.6 0.2) 0.9

bass :: Instrument
bass = Instrument triangleWave (ADSR 0.01 0.15 0.8 0.3) 1.0

synth :: Instrument
synth = Instrument squareWave (ADSR 0.02 0.1 0.5 0.25) 0.85
