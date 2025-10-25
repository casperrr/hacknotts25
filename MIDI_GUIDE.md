# MIDI Support Implementation Guide

This guide explains how to add full MIDI file parsing support to your synthesizer.

## Overview

The synthesizer already has the infrastructure for MIDI support. To enable it, you need to:

1. Install the required packages
2. Replace the stub `Music/MIDI.hs` with the full implementation

## Step 1: Install Dependencies

Add these packages to your `hacknotts25-synth.cabal` file:

```cabal
build-depends:    base ^>=4.18.3.0,
                  bytestring,
                  random,
                  midi,           -- MIDI file parsing
                  event-list,     -- Event handling
                  non-negative    -- Non-negative numbers
```

Then install them:

```bash
cabal update
cabal install midi event-list non-negative
```

## Step 2: Full MIDI Parser Implementation

Replace `app/Music/MIDI.hs` with this complete implementation:

```haskell
{-# LANGUAGE RecordWildCards #-}
module Music.MIDI where

import Music.Types
import qualified Sound.MIDI.File as MF
import qualified Sound.MIDI.File.Load as MFL
import qualified Sound.MIDI.File.Event as ME
import qualified Sound.MIDI.File.Event.Meta as MM
import qualified Sound.MIDI.Message.Channel as MC
import qualified Sound.MIDI.Message.Channel.Voice as MV
import Data.EventList.Relative.TimeBody as RTB
import qualified Numeric.NonNegative.Wrapper as NN
import qualified Data.List

-- State to track active notes during parsing
data ParseState = ParseState {
    activeNotes :: [(Int, Float, Float)],  -- (midiPitch, startTime, velocity)
    currentTime :: Float,
    ticksPerBeat :: Int,
    tempo :: Float  -- microseconds per quarter note
}

initialState :: Int -> ParseState
initialState tpb = ParseState [] 0.0 tpb 500000  -- 120 BPM default

-- Convert MIDI note number to pitch (semitones from A4)
midiNoteToPitch :: Int -> Pitch
midiNoteToPitch n = fromIntegral (n - 69)  -- 69 is A4 in MIDI

-- Convert MIDI ticks to beats
ticksToBeats :: ParseState -> NN.Int -> Beats
ticksToBeats ParseState{..} ticks = 
    realToFrac (NN.toNumber ticks) / realToFrac ticksPerBeat

-- Convert MIDI velocity to our velocity (0.0 to 1.0)
midiVelocity :: Int -> Velocity
midiVelocity v = realToFrac v / 127.0

-- Intermediate representation of a note event
data NoteEvent = NoteEvent {
    notePitch :: Pitch,
    noteStart :: Float,
    noteDuration :: Float,
    noteVelocity :: Velocity
} deriving (Show, Eq)

-- Parse a single MIDI track into a list of note events
parseTrackEvents :: ParseState -> RTB.T NN.Int ME.T -> [NoteEvent]
parseTrackEvents state events = go state events []
    where
        go :: ParseState -> RTB.T NN.Int ME.T -> [NoteEvent] -> [NoteEvent]
        go st evts acc = case RTB.viewL evts of
            Nothing -> 
                -- End of track, close any remaining active notes
                let finalTime = currentTime st
                    finalNotes = map (\(p, start, vel) -> 
                        NoteEvent (midiNoteToPitch p) start (finalTime - start) vel) 
                        (activeNotes st)
                in reverse acc ++ finalNotes
            
            Just ((dt, event), rest) ->
                let newTime = currentTime st + ticksToBeats st dt
                    st' = st { currentTime = newTime }
                in case event of
                    ME.MIDIEvent (MC.Cons _chan (MC.Voice voice)) ->
                        case voice of
                            MV.NoteOn pitch vel ->
                                if MV.fromVelocity vel == 0 then
                                    -- Velocity 0 is a note off
                                    handleNoteOff st' rest acc (MV.fromPitch pitch)
                                else
                                    let p = MV.fromPitch pitch
                                        v = midiVelocity (MV.fromVelocity vel)
                                        newSt = st' { activeNotes = (p, newTime, v) : activeNotes st' }
                                    in go newSt rest acc
                            
                            MV.NoteOff pitch _vel ->
                                handleNoteOff st' rest acc (MV.fromPitch pitch)
                            
                            _ -> go st' rest acc
                    
                    ME.MetaEvent (MM.SetTempo newTempo) ->
                        go (st' { tempo = realToFrac newTempo }) rest acc
                    
                    _ -> go st' rest acc
        
        handleNoteOff :: ParseState -> RTB.T NN.Int ME.T -> [NoteEvent] -> Int -> [NoteEvent]
        handleNoteOff st rest acc midiPitch =
            case lookup midiPitch [(p, (start, vel)) | (p, start, vel) <- activeNotes st] of
                Just (startTime, velocity) ->
                    let duration = currentTime st - startTime
                        note = NoteEvent (midiNoteToPitch midiPitch) startTime duration velocity
                        newSt = st { activeNotes = Data.List.filter (\(p, _, _) -> p /= midiPitch) (activeNotes st) }
                    in go newSt rest (note : acc)
                Nothing -> go st rest acc

-- Convert list of note events to Music
eventsToMusic :: [NoteEvent] -> Music
eventsToMusic [] = Rest 0.0
eventsToMusic events = 
    let sortedEvents = Data.List.sortBy (\a b -> compare (noteStart a) (noteStart b)) events
        grouped = groupSimultaneous sortedEvents
    in buildMusic grouped
    where
        -- Group notes that start at the same time (within 1ms)
        groupSimultaneous :: [NoteEvent] -> [[NoteEvent]]
        groupSimultaneous [] = []
        groupSimultaneous (e:es) = 
            let (same, rest) = Data.List.span (\x -> abs (noteStart x - noteStart e) < 0.001) es
            in (e:same) : groupSimultaneous rest
        
        -- Build Music from grouped events
        buildMusic :: [[NoteEvent]] -> Music
        buildMusic [] = Rest 0.0
        buildMusic [group] = groupToMusic group
        buildMusic (group:groups) = 
            let currentMusic = groupToMusic group
                restMusic = buildMusic groups
                -- Calculate gap between groups
                currentEnd = maximum [noteStart e + noteDuration e | e <- group]
                nextStart = minimum [noteStart e | e <- head groups]
                gap = nextStart - currentEnd
                withRest = if gap > 0.001 
                          then Seq currentMusic (Rest gap)
                          else currentMusic
            in Seq withRest restMusic
        
        groupToMusic :: [NoteEvent] -> Music
        groupToMusic [] = Rest 0.0
        groupToMusic [e] = Note (notePitch e) (noteDuration e) (noteVelocity e)
        groupToMusic (e:es) = Par (Note (notePitch e) (noteDuration e) (noteVelocity e))
                                  (groupToMusic es)

-- Parse full MIDI file
parseMIDI :: FilePath -> IO Music
parseMIDI path = do
    result <- MFL.fromFile path
    let (MF.Cons _fileType division tracksList) = result
        tpb = case division of
                MF.Ticks t -> NN.toNumber t
                _ -> 480  -- default ticks per beat
        state = initialState tpb
        -- Extract track bodies and parse
        trackBodies = map (\(MF.Track body) -> body) tracksList
        parsedTracks = zipWith (\i t -> 
            Track i (eventsToMusic (parseTrackEvents state t))) [0::Int ..] trackBodies
    return $ foldr1 Par parsedTracks
```

## Step 3: Update Main.hs

Uncomment the MIDI parsing code in `app/Main.hs`:

```haskell
-- Example 4: Parse MIDI file
putStrLn "Attempting to parse MIDI file..."
midiMusic <- parseMIDI "dohotgirlslikechordsguitarsolo.mid"
let audio4 = render piano midiMusic
saveWav "midi-output.wav" audio4
putStrLn "✓ Parsed MIDI and saved to midi-output.wav\n"
```

## Usage

Once installed, you can parse any MIDI file:

```haskell
import Music.MIDI
import Music.Render
import Synth.Instrument

main :: IO ()
main = do
    -- Parse MIDI file
    music <- parseMIDI "song.mid"
    
    -- Render with piano
    let audio = render piano music
    saveWav "output.wav" audio
    
    -- Or render multi-track with different instruments
    let instruments = [
            (0, piano),   -- Track 0: Piano
            (1, organ),   -- Track 1: Organ
            (2, bass)     -- Track 2: Bass
        ]
    let multiAudio = renderMultiTrack instruments music
    saveWav "multi-output.wav" multiAudio
```

## How It Works

### 1. **Parsing Process**

The MIDI parser goes through these steps:

1. Load the MIDI file using `Sound.MIDI.File.Load.fromFile`
2. Extract timing information (ticks per beat)
3. For each track:
   - Parse note on/off events
   - Track active notes with their start times
   - When a note off occurs, create a `NoteEvent` with duration
   - Handle tempo changes
4. Convert `NoteEvent` list to `Music` structure

### 2. **Note Tracking**

The parser maintains a `ParseState` that tracks:
- Currently active (playing) notes
- Current time in beats
- Tempo (for timing calculations)
- Ticks per beat (from MIDI file header)

### 3. **Event Conversion**

MIDI events are converted to our `Music` type:
- **Note On** → Start tracking the note
- **Note Off** → Create a `Note` with calculated duration
- **Tempo Change** → Update tempo in state
- Other events → Ignored (can be extended)

### 4. **Time Alignment**

Notes are grouped by start time to create:
- **Sequential music** (`Seq`) for notes that play one after another
- **Parallel music** (`Par`) for chords (notes starting simultaneously)
- **Rests** for gaps between notes

## MIDI Note Mapping

MIDI notes are converted to our pitch system:

| MIDI Note | Note Name | Our Pitch | Frequency |
|-----------|-----------|-----------|-----------|
| 57        | A3        | -12       | 220 Hz    |
| 60        | C4        | -9        | 261 Hz    |
| 64        | E4        | -5        | 329 Hz    |
| 69        | A4        | 0         | 440 Hz    |
| 72        | C5        | 3         | 523 Hz    |
| 76        | E5        | 7         | 659 Hz    |
| 81        | A5        | 12        | 880 Hz    |

## Limitations & Future Improvements

### Current Limitations

1. **Tracks vs Channels**: Currently uses track numbers. MIDI channels are ignored.
2. **Control Changes**: Pedals, modulation, pitch bend are not parsed.
3. **Program Changes**: Instrument changes in MIDI are ignored.
4. **Multiple Notes**: If same note is played simultaneously, only last one is tracked.

### Possible Improvements

1. **Channel Support**: Map MIDI channels to different instruments
2. **Velocity Curves**: Apply more realistic velocity-to-volume mapping
3. **Tempo Curves**: Support gradual tempo changes (ritardando, accelerando)
4. **Expression**: Parse CC#11 (expression) for dynamic changes
5. **Sustain Pedal**: Parse CC#64 to extend note durations
6. **Pitch Bend**: Modify pitch based on pitch bend messages

## Testing

Test your MIDI parser with simple files first:

```bash
# Create a simple test MIDI in MuseScore, FL Studio, etc.
# Save as test.mid with just a few notes

# Then in your Haskell code:
music <- parseMIDI "test.mid"
print music  -- Check the parsed structure
```

## Troubleshooting

**Package installation fails:**
```bash
cabal update
cabal install --lib midi event-list non-negative
```

**Build errors:**
- Make sure all imports are qualified (`qualified Data.List`)
- Check GHC version compatibility (midi package works with GHC 9.x)

**Parsed music sounds wrong:**
- Check MIDI timing (some files use different time divisions)
- Verify tempo is parsed correctly
- Ensure note on/off events are properly paired

## Resources

- [MIDI Package Documentation](https://hackage.haskell.org/package/midi)
- [MIDI Specification](https://www.midi.org/specifications)
- [Event-List Package](https://hackage.haskell.org/package/event-list)
