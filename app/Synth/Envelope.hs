module Synth.Envelope where

import Synth.Types
import Synth.Constants

data ADSR = ADSR {
    attack :: Seconds,   -- Time to reach peak
    decay :: Seconds,    -- Time to reach sustain level
    sustain :: Volume,   -- Sustain level (0.0 to 1.0)
    release :: Seconds   -- Time to fade to 0
} deriving (Show, Eq)

-- Default ADSR envelope
defaultADSR :: ADSR
defaultADSR = ADSR 0.01 0.1 0.7 0.2

-- Apply envelope to audio
applyEnvelope :: ADSR -> Seconds -> Audio -> Audio
applyEnvelope (ADSR a d s r) duration samples = zipWith (*) samples envelope
    where
        numSamples = length samples
        envelope = attackPhase ++ decayPhase ++ sustainPhase ++ releasePhase
        
        attackSamples = floor (a * sampleRate)
        decaySamples = floor (d * sampleRate)
        releaseSamples = floor (r * sampleRate)
        sustainSamples = max 0 (numSamples - attackSamples - decaySamples - releaseSamples)
        
        attackPhase = [realToFrac i / realToFrac attackSamples | i <- [0..attackSamples-1]]
        decayPhase = [1.0 - (1.0 - s) * realToFrac i / realToFrac decaySamples | i <- [0..decaySamples-1]]
        sustainPhase = replicate sustainSamples s
        releasePhase = [s * (1.0 - realToFrac i / realToFrac releaseSamples) | i <- [0..releaseSamples-1]]
