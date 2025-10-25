module Music.MIDI where

import Music.Types

-- MIDI parsing - placeholder implementation
-- To use this, install the 'midi' package and implement the parser
-- For now, this provides a stub implementation

parseMIDI :: FilePath -> IO Music
parseMIDI path = do
    putStrLn $ "MIDI parsing not yet fully implemented for: " ++ path
    putStrLn "Install 'midi', 'event-list', and 'non-negative' packages to enable MIDI support"
    -- Return a simple default music
    return $ Note 0 1.0 1.0

-- When you have the MIDI package installed, you can implement this properly
-- See the full implementation in the documentation

