module Music.Util where

import Synth.Types
import Music.Types

-- Enables polyphonic mix + prevents clipping.
mix :: [Audio] -> Audio
mix tracks = foldr1 (zipWith (+)) tracks

poly :: [Audio] -> Audio
poly tracks = map (/n) (mix tracks)
    where
        n = fromIntegral (length tracks)

midiToHz :: Pitch -> Hz
midiToHz p = 440.0 * (2 ** ((p - 69) / 12))