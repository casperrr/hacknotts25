module Synth.Util where

import Data.ByteString.Builder ( floatLE, toLazyByteString, string8, word32LE, word16LE, int16LE )
import Data.ByteString.Lazy (writeFile)
import Prelude hiding (writeFile)
import Data.Int ( Int16 )
import Data.Word ( Word32 )

import Synth.Types
import Synth.Constants

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
saveWav :: FilePath -> Audio -> IO ()
saveWav filePath samples = writeFile filePath $ toLazyByteString builder
    where
        builder = mconcat [
            ---- RIFF Header ----
            string8 "RIFF", -- ChunkID
            word32LE (4 + (8 + 16) + (8 + dataBytes)), -- ChunkSize, Overall size of file 32-Int
            string8 "WAVE", -- Format
            ---- Format Chunk ----
            string8 "fmt ", -- Subchunk1ID
            word32LE 16, -- Subchunk1Size, PCM = 16
            word16LE 1, -- AudioFormat, PCM = 1
            word16LE 1, -- NumChannels, Mono = 1
            word32LE sr, -- SampleRate
            word32LE (sr * (16 `div` 8)), -- ByteRate = SampleRate * NumChannels * BitsPerSample/8
            word16LE (16 `div` 8), -- BlockAlign = NumChannels * BitsPerSample/8
            word16LE 16, -- BitsPerSample
            ---- Data Chunk ----
            string8 "data", -- Subchunk2ID
            word32LE dataBytes, -- Subchunk2Size = NumSamples * NumChannels * BitsPerSample/8
            foldMap (int16LE . floatToInt16) samples -- Actual sound data
         ]

        -- The size of the data section in bytes
        dataBytes :: Word32
        dataBytes = fromIntegral $ length samples * (16 `div` 8)

        sr :: Word32
        sr = round sampleRate