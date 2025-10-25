module Music.Render where

import Synth.Types
import Synth.Constants (sampleRate)
import Synth.Oscillator
import Synth.Envelope
import Synth.Instrument
import Music.Types
import Music.Constants (bpm, pitchStd)

-- Convert pitch (semitones from A4) to frequency
pitchToHz :: Pitch -> Hz
pitchToHz p = pitchStd * (2 ** (p / 12.0))

-- Convert beats to seconds
beatsToSeconds :: Float -> Beats -> Seconds
beatsToSeconds currentBpm beats = (beats * 60.0) / currentBpm

-- Render music with an instrument
render :: Instrument -> Music -> Audio
render = renderWithBpm bpm

-- Render music with an instrument and specific BPM
renderWithBpm :: Float -> Instrument -> Music -> Audio
renderWithBpm currentBpm inst music = case music of
    Note pitch beats vel -> 
        let hz = pitchToHz pitch
            duration = beatsToSeconds currentBpm beats
            wf = waveform inst
            env = envelope inst
            vol = volume inst * vel
            audio = oscillator hz wf duration
            enveloped = applyEnvelope env duration audio
        in map (* vol) enveloped

    Rest beats -> 
        let duration = beatsToSeconds currentBpm beats
            numSamples = floor (sampleRate * duration)
        in replicate numSamples 0.0

    Seq m1 m2 -> 
        renderWithBpm currentBpm inst m1 ++ renderWithBpm currentBpm inst m2

    Par m1 m2 -> 
        let a1 = renderWithBpm currentBpm inst m1
            a2 = renderWithBpm currentBpm inst m2
            maxLen = max (length a1) (length a2)
            a1' = a1 ++ replicate (maxLen - length a1) 0.0
            a2' = a2 ++ replicate (maxLen - length a2) 0.0
        in zipWith (+) a1' a2'
    
    Track _ m -> 
        -- For now, ignore track number and just render the music
        renderWithBpm currentBpm inst m
    
    Tempo newBpm m -> 
        -- Render with new tempo
        renderWithBpm newBpm inst m

-- Render with multiple instruments (one per track)
renderMultiTrack :: [(Int, Instrument)] -> Music -> Audio
renderMultiTrack trackInstruments music = renderMT music
    where
        defaultInst = piano
        
        getInstrument :: Int -> Instrument
        getInstrument trackNum = case lookup trackNum trackInstruments of
            Just inst -> inst
            Nothing -> defaultInst
        
        renderMT :: Music -> Audio
        renderMT (Note pitch beats vel) = render defaultInst (Note pitch beats vel)
        renderMT (Rest beats) = render defaultInst (Rest beats)
        renderMT (Seq m1 m2) = renderMT m1 ++ renderMT m2
        renderMT (Par m1 m2) = 
            let a1 = renderMT m1
                a2 = renderMT m2
                maxLen = max (length a1) (length a2)
                a1' = a1 ++ replicate (maxLen - length a1) 0.0
                a2' = a2 ++ replicate (maxLen - length a2) 0.0
            in zipWith (+) a1' a2'
        renderMT (Track trackNum m) = render (getInstrument trackNum) m
        renderMT (Tempo _ m) = renderMT m
