#!/usr/bin/env cabal
{- cabal:
build-depends: base, midi, event-list, non-negative
-}

-- Quick tool to inspect MIDI file structure
import qualified Sound.MIDI.File.Load as MFL
import qualified Sound.MIDI.File as MF
import qualified Numeric.NonNegative.Wrapper as NN

main :: IO ()
main = do
    result <- MFL.fromFile "dohotgirlslikechordsguitarsolo.mid"
    let MF.Cons fileType division tracks = result
    
    putStrLn $ "MIDI File Information"
    putStrLn $ "====================="
    putStrLn $ "File Type: " ++ show fileType
    putStrLn $ "Division: " ++ show division
    putStrLn $ "Number of Tracks: " ++ show (length tracks)
    
    let tpb = case division of
                MF.Ticks t -> NN.toNumber t
                _ -> 480
    putStrLn $ "Ticks per beat: " ++ show tpb
    putStrLn $ "\n✓ MIDI file parsed successfully!"
