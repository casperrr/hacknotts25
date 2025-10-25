module Synth.Types where

------- Types -------
type Sample = Float
type Audio = [Sample]
type Hz = Float
type Seconds = Float
type WaveForm = Float -> Sample
type Volume = Float