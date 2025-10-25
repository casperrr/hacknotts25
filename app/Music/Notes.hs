module Music.Notes where

import Synth.Types
import Synth.Oscillator
import Music.Types
import Music.Constants

note :: WaveForm -> Pitch -> Beats -> Velocity -> Audio
note wf p b v = map (* v) (oscillator (midi p) wf (beatToSeconds b))

beatToSeconds :: Beats -> Seconds
beatToSeconds beats = beats * (60.0 / bpm)

pitch :: Pitch -> Hz
pitch n = pitchStd*(2**(1.0/12.0))**n

-- Converts midi format to frequency in Hz
midi :: Pitch -> Hz
midi n = pitchStd*(2**((n-69)/12.0))