module Main where

import Synth.Constants
import Synth.Oscillator
import Synth.Types
import Synth.Util
import Synth.WaveForm
import Synth.Instrument
import Music.Types
import Music.Render
import Music.MIDI

-- Example: Simple melody using Music notation
melody :: Music
melody = sequence' [
    Note 0 0.5 1.0,      -- A4
    Note 2 0.5 0.9,      -- B4
    Note 4 0.5 0.95,     -- C#5
    Note 5 1.0 1.0,      -- D5
    Rest 0.25,
    Note 4 0.5 0.9,      -- C#5
    Note 2 0.5 0.85,     -- B4
    Note 0 1.0 1.0       -- A4
    ]

-- Example: Chord progression
chordProgression :: Music
chordProgression = sequence' [
    chord [Note 0 2.0 0.8, Note 4 2.0 0.7, Note 7 2.0 0.75],  -- A major
    chord [Note 5 2.0 0.8, Note 9 2.0 0.7, Note 12 2.0 0.75], -- D major
    chord [Note 2 2.0 0.8, Note 6 2.0 0.7, Note 9 2.0 0.75],  -- B minor
    chord [Note 0 2.0 0.8, Note 4 2.0 0.7, Note 7 2.0 0.75]   -- A major
    ]

-- Example: Multi-track composition
composition :: Music
composition = Par 
    (Track 0 melody)                    -- Piano melody
    (Track 1 chordProgression)          -- Organ chords

main :: IO ()
main = do
    putStrLn "Hacknotts25 Synthesizer"
    putStrLn "========================\n"
    
    -- Example 1: Simple melody with piano
    putStrLn "Generating melody with piano..."
    let audio1 = render piano melody
    saveWav "melody.wav" audio1
    putStrLn "✓ Saved to melody.wav\n"
    
    -- Example 2: Chord progression with organ
    putStrLn "Generating chords with organ..."
    let audio2 = render organ chordProgression
    saveWav "chords.wav" audio2
    putStrLn "✓ Saved to chords.wav\n"
    
    -- Example 3: Multi-track with different instruments
    putStrLn "Generating multi-track composition..."
    let audio3 = renderMultiTrack [(0, piano), (1, organ)] composition
    saveWav "composition.wav" audio3
    putStrLn "✓ Saved to composition.wav\n"
    
    -- Example 4: Parse MIDI file if it exists (requires MIDI packages)
    -- putStrLn "Attempting to parse MIDI file..."
    -- midiMusic <- parseMIDI "dohotgirlslikechordsguitarsolo.mid"
    -- let audio4 = render piano midiMusic
    -- saveWav "midi-output.wav" audio4
    -- putStrLn "✓ Parsed MIDI and saved to midi-output.wav\n"
    
    putStrLn "All done! 🎵"

