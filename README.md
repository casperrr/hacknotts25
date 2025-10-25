# Hacknotts25 Synthesizer

A Haskell-based software synthesizer that can generate audio from musical notation or MIDI files.

## Features

- **Musical Notation**: Define music using a simple, composable data type
- **Multiple Instruments**: Piano, Organ, Lead, Bass, and Synth with ADSR envelopes
- **MIDI Support**: Parse MIDI files and convert them to audio
- **Waveforms**: Sine, Square, Sawtooth, Triangle, and Noise waves
- **Multi-track**: Compose music with multiple instruments on different tracks

## Project Structure

```
app/
├── Main.hs                  # Example usage
├── Music/
│   ├── Types.hs            # Musical data types (Note, Rest, Seq, Par)
│   ├── Constants.hs        # Musical constants (BPM, pitch standard)
│   ├── Render.hs           # Convert Music to Audio
│   └── MIDI.hs             # MIDI file parser
└── Synth/
    ├── Types.hs            # Synthesis types (Audio, Hz, WaveForm, etc.)
    ├── Constants.hs        # Synthesis constants (sample rate, volume)
    ├── Oscillator.hs       # Core oscillator function
    ├── WaveForm.hs         # Waveform generators
    ├── Envelope.hs         # ADSR envelope
    ├── Instrument.hs       # Instrument definitions
    └── Util.hs             # WAV file export
```

## Building

```bash
cabal build
```

## Running

```bash
cabal run
```

This will generate several example WAV files:
- `melody.wav` - Simple piano melody
- `chords.wav` - Chord progression with organ
- `composition.wav` - Multi-track composition
- `midi-output.wav` - Parsed from MIDI file

## Usage Examples

### Creating Music

```haskell
import Music.Types
import Music.Render
import Synth.Instrument

-- Define a simple melody
melody :: Music
melody = Seq 
    (Note 0 1.0 1.0)      -- A4 for 1 beat at full velocity
    (Note 4 1.0 0.8)      -- C#5 for 1 beat at 80% velocity

-- Create a chord
chord :: Music
chord = Par 
    (Note 0 2.0 0.8)      -- A4
    (Par (Note 4 2.0 0.7) -- C#5
         (Note 7 2.0 0.7)) -- E5

-- Render to audio
audio = render piano melody
```

### Music Data Type

The `Music` type is compositional:

- `Note Pitch Beats Velocity` - A single note
  - `Pitch`: Semitones from A4 (0 = A4, 12 = A5, -12 = A3)
  - `Beats`: Duration in beats
  - `Velocity`: Volume (0.0 to 1.0)

- `Rest Beats` - Silence for a duration

- `Seq Music Music` - Play two pieces sequentially

- `Par Music Music` - Play two pieces simultaneously (chords/harmony)

- `Track Int Music` - Assign music to a track (for multi-instrument rendering)

- `Tempo Float Music` - Change tempo for a section

### Helper Functions

```haskell
-- Create a chord from a list of notes
chord :: [Music] -> Music

-- Create a sequence from a list of music
sequence' :: [Music] -> Music
```

### Instruments

Predefined instruments with different timbres and ADSR envelopes:

- `piano` - Sine wave with natural decay
- `organ` - Sine wave with sustained sound
- `lead` - Sawtooth wave for bright leads
- `bass` - Triangle wave for deep bass
- `synth` - Square wave for classic synth sound

### MIDI Files

```haskell
import Music.MIDI
import Music.Render
import Synth.Instrument

main :: IO ()
main = do
    music <- parseMIDI "song.mid"
    let audio = render piano music
    saveWav "output.wav" audio
```

### Multi-track Rendering

```haskell
import Music.Render

-- Define instruments for each track
instruments = [(0, piano), (1, bass), (2, lead)]

-- Compose music with tracks
composition = Par
    (Track 0 melody)
    (Track 1 bassline)

-- Render
audio = renderMultiTrack instruments composition
```

## Creating Custom Instruments

```haskell
import Synth.Instrument
import Synth.WaveForm

myInstrument :: Instrument
myInstrument = Instrument {
    waveform = sawtoothWave,
    envelope = ADSR {
        attack = 0.05,    -- 50ms attack
        decay = 0.2,      -- 200ms decay
        sustain = 0.6,    -- 60% sustain level
        release = 0.3     -- 300ms release
    },
    volume = 0.9
}
```

## Architecture

The synthesizer follows a modular architecture:

1. **Music Layer**: Abstract musical representation
   - Define music using compositional data types
   - Independent of synthesis implementation

2. **Synthesis Layer**: Audio generation
   - Oscillators generate raw waveforms
   - Envelopes shape the amplitude over time
   - Instruments combine waveforms with envelopes

3. **Rendering Layer**: Convert music to audio
   - Converts pitch to frequency
   - Applies timing based on BPM
   - Mixes multiple tracks

4. **I/O Layer**: 
   - MIDI parsing (input)
   - WAV export (output)

## Dependencies

- `base` - Standard Haskell library
- `bytestring` - For WAV file generation
- `random` - For noise waveform
- `midi` - MIDI file parsing
- `event-list` - Event handling for MIDI
- `non-negative` - Non-negative numbers for MIDI

## License

MIT
