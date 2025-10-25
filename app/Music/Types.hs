module Music.Types where

--- Define some music types here ---

type Pitch = Float
type Beats = Float
type Velocity = Float

data Music = Note Pitch Beats Velocity
           | Rest Beats
           | Seq Music Music -- notes one after another 
           | Par Music Music -- notes simultaniously
           | Track Int Music -- Track number/instrument
           | Tempo Float Music -- Set tempo (BPM) for this section
           deriving (Show, Eq)

-- Helper to create chords
chord :: [Music] -> Music
chord [] = Rest 0.0
chord [m] = m
chord ms = foldr1 Par ms

-- Helper to create sequences
sequence' :: [Music] -> Music
sequence' [] = Rest 0.0
sequence' [m] = m
sequence' ms = foldr1 Seq ms