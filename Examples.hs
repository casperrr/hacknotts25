-- Example usage of the Hacknotts25 Synthesizer
-- This file shows various ways to create music using the synthesizer

module Examples where

import Music.Types
import Music.Render
import Synth.Instrument
import Synth.Util
import Synth.Envelope
import Synth.WaveForm

-- ============================================================================
-- EXAMPLE 1: Simple Notes and Scales
-- ============================================================================

-- A simple note: A4 (pitch 0), 1 beat long, full velocity
simpleNote :: Music
simpleNote = Note 0 1.0 1.0

-- A major scale
majorScale :: Music
majorScale = sequence' [
    Note 0 0.5 1.0,   -- A
    Note 2 0.5 1.0,   -- B
    Note 4 0.5 1.0,   -- C#
    Note 5 0.5 1.0,   -- D
    Note 7 0.5 1.0,   -- E
    Note 9 0.5 1.0,   -- F#
    Note 11 0.5 1.0,  -- G#
    Note 12 1.0 1.0   -- A (octave higher)
    ]

-- A minor scale
minorScale :: Music
minorScale = sequence' [
    Note 0 0.5 1.0,   -- A
    Note 2 0.5 1.0,   -- B
    Note 3 0.5 1.0,   -- C
    Note 5 0.5 1.0,   -- D
    Note 7 0.5 1.0,   -- E
    Note 8 0.5 1.0,   -- F
    Note 10 0.5 1.0,  -- G
    Note 12 1.0 1.0   -- A (octave higher)
    ]

-- ============================================================================
-- EXAMPLE 2: Chords
-- ============================================================================

-- A major chord (A, C#, E)
aMajorChord :: Music
aMajorChord = chord [
    Note 0 2.0 0.8,   -- A
    Note 4 2.0 0.7,   -- C#
    Note 7 2.0 0.75   -- E
    ]

-- D major chord (D, F#, A)
dMajorChord :: Music
dMajorChord = chord [
    Note 5 2.0 0.8,   -- D
    Note 9 2.0 0.7,   -- F#
    Note 12 2.0 0.75  -- A
    ]

-- B minor chord (B, D, F#)
bMinorChord :: Music
bMinorChord = chord [
    Note 2 2.0 0.8,   -- B
    Note 6 2.0 0.7,   -- D (B+4 semitones)
    Note 9 2.0 0.75   -- F#
    ]

-- Chord progression
progression :: Music
progression = sequence' [
    aMajorChord,
    dMajorChord,
    bMinorChord,
    aMajorChord
    ]

-- ============================================================================
-- EXAMPLE 3: Melodies with Rhythm
-- ============================================================================

-- Twinkle Twinkle Little Star (first line)
twinkleTwinkle :: Music
twinkleTwinkle = sequence' [
    Note 0 1.0 1.0,   -- Twin-
    Note 0 1.0 0.9,   -- kle
    Note 7 1.0 1.0,   -- twin-
    Note 7 1.0 0.9,   -- kle
    Note 9 1.0 1.0,   -- lit-
    Note 9 1.0 0.9,   -- tle
    Note 7 2.0 1.0    -- star
    ]

-- Melody with varied dynamics (velocities)
expressiveMelody :: Music
expressiveMelody = sequence' [
    Note 0 0.5 0.6,   -- soft
    Note 2 0.5 0.7,
    Note 4 0.5 0.8,
    Note 5 0.5 0.9,
    Note 7 1.0 1.0,   -- loud
    Note 5 0.5 0.9,
    Note 4 0.5 0.8,
    Note 2 0.5 0.7,
    Note 0 1.0 0.6    -- soft again
    ]

-- ============================================================================
-- EXAMPLE 4: Melody with Accompaniment
-- ============================================================================

-- Simple bass line
bassLine :: Music
bassLine = sequence' [
    Note (-12) 2.0 0.8,  -- A (one octave lower)
    Note (-7) 2.0 0.8,   -- D
    Note (-10) 2.0 0.8,  -- B
    Note (-12) 2.0 0.8   -- A
    ]

-- Combine melody with bass
songWithBass :: Music
songWithBass = Par
    (Track 0 twinkleTwinkle)
    (Track 1 bassLine)

-- ============================================================================
-- EXAMPLE 5: Multi-track Composition
-- ============================================================================

-- Lead melody
leadMelody :: Music
leadMelody = sequence' [
    Note 12 1.0 0.9,
    Note 14 0.5 0.8,
    Note 12 0.5 0.9,
    Note 11 1.0 1.0,
    Rest 0.5,
    Note 9 1.5 0.85,
    Note 7 2.0 0.9
    ]

-- Chord pad
padChords :: Music
padChords = sequence' [
    chord [Note 0 3.0 0.5, Note 4 3.0 0.45, Note 7 3.0 0.4],
    chord [Note 5 3.5 0.5, Note 9 3.5 0.45, Note 12 3.5 0.4]
    ]

-- Bass
deepBass :: Music
deepBass = sequence' [
    Note (-24) 1.5 0.7,
    Note (-24) 1.5 0.6,
    Note (-19) 1.75 0.7,
    Note (-19) 1.75 0.6
    ]

-- Full arrangement
fullSong :: Music
fullSong = chord [
    Track 0 leadMelody,
    Track 1 padChords,
    Track 2 deepBass
    ]

-- ============================================================================
-- EXAMPLE 6: Using Different Instruments
-- ============================================================================

-- Create a custom instrument
customSynth :: Instrument
customSynth = Instrument {
    waveform = sawtoothWave,
    envelope = ADSR {
        attack = 0.03,
        decay = 0.15,
        sustain = 0.65,
        release = 0.25
    },
    volume = 0.85
}

-- ============================================================================
-- EXAMPLE 7: Tempo Changes
-- ============================================================================

-- Music with tempo change
dynamicTempo :: Music
dynamicTempo = sequence' [
    Tempo 60 (sequence' [Note 0 1.0 1.0, Note 2 1.0 1.0]),  -- Slow (60 BPM)
    Tempo 120 (sequence' [Note 4 1.0 1.0, Note 5 1.0 1.0]), -- Normal (120 BPM)
    Tempo 180 (sequence' [Note 7 1.0 1.0, Note 9 1.0 1.0])  -- Fast (180 BPM)
    ]

-- ============================================================================
-- RENDERING EXAMPLES
-- ============================================================================

-- Render functions to generate WAV files
renderExamples :: IO ()
renderExamples = do
    -- Render major scale with piano
    saveWav "scale-major.wav" $ render piano majorScale
    
    -- Render chord progression with organ
    saveWav "progression.wav" $ render organ progression
    
    -- Render melody with lead synth
    saveWav "lead-melody.wav" $ render lead leadMelody
    
    -- Render full song with multiple instruments
    let multiTrackAudio = renderMultiTrack [
            (0, lead),
            (1, organ),
            (2, bass)
            ] fullSong
    saveWav "full-song.wav" multiTrackAudio
    
    -- Render with custom instrument
    saveWav "custom-synth.wav" $ render customSynth expressiveMelody
    
    putStrLn "All examples rendered!"

-- ============================================================================
-- PITCH REFERENCE
-- ============================================================================
-- Pitch is in semitones from A4 (440 Hz)
-- 
-- Octave 3:
--   A3  = -12
--   B3  = -10
--   
-- Octave 4 (reference):
--   A4  = 0   (440 Hz)
--   B4  = 2
--   C4  = 3
--   C#4 = 4
--   D4  = 5
--   E4  = 7
--   F4  = 8
--   F#4 = 9
--   G4  = 10
--   G#4 = 11
--   
-- Octave 5:
--   A5  = 12
--   B5  = 14
--   C5  = 15
-- ============================================================================
