module Synth.Util where

import Data.ByteString.Builder ( floatLE, toLazyByteString )
import Data.ByteString.Lazy (writeFile)
import Prelude hiding (writeFile)

import Synth.Types
import Data.Int ( Int16 )

------- Utility Functions -------

-- Sample = [Float 32]
-- We need to clamp samples to the range [-1.0, 1.0]
-- Wav file uses 16-bit PCM, (16-bit int)

clamp :: Sample -> Sample
clamp x = max (-1.0) (min 1.0 x)

floatToInt16 :: Float -> Int16
floatToInt16 s = round $ clamp $ s * 32767.0

----- Writing to WAV files -----

-- First write raw bin to .bin file

saveRaw :: FilePath -> Audio -> IO ()
saveRaw path audio = writeFile path $ toLazyByteString $ foldMap floatLE audio

-- to play audio run:
-- ffplay -showmode 1 -f f32le -ar sampleRate "output.bin