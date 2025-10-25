{-# LANGUAGE RecordWildCards #-}
module Music.MIDI where

import Music.Types
import Sound.MIDI.File hiding (Track)  -- Import all except Track type name
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
ticksToBeats :: ParseState -> NN.Integer -> Beats
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
parseTrackEvents :: ParseState -> RTB.T NN.Integer ME.T -> [NoteEvent]
parseTrackEvents state events = go state events []
    where
        go :: ParseState -> RTB.T NN.Integer ME.T -> [NoteEvent] -> [NoteEvent]
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
        
        handleNoteOff :: ParseState -> RTB.T NN.Integer ME.T -> [NoteEvent] -> Int -> [NoteEvent]
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
    putStrLn $ "Parsing MIDI file: " ++ path
    result <- MFL.fromFile path
    -- result is already of type MF.T which is Cons fileType division tracks
    let Cons _fileType division tracksList = result
        tpb = case division of
                Ticks t -> NN.toNumber t
                _ -> 480  -- default ticks per beat
        state = initialState tpb
        -- tracksList has type [MF.Track]
        -- We need to extract the body from each track
        parsedTracks = zipWith (\i trackEvents ->
            let music = eventsToMusic (parseTrackEvents state trackEvents)
            in if music == Rest 0.0 then Rest 0.0 else Music.Types.Track i music)
            [0::Int ..] tracksList
        nonEmptyTracks = Data.List.filter (/= Rest 0.0) parsedTracks
    putStrLn $ "Parsed " ++ show (length tracksList) ++ " tracks"
    return $ if Prelude.null nonEmptyTracks 
             then Rest 0.0 
             else foldr1 Par nonEmptyTracks

