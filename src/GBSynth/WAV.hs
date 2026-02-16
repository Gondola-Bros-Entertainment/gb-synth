-- | 16-bit mono PCM WAV file writer.
--
-- All sounds use a fixed sample rate of 22050 Hz.
module GBSynth.WAV
  ( -- * Constants
    sampleRate,

    -- * Conversions
    msToSamples,
    toSample,

    -- * IO
    writeWav,
  )
where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as B
import qualified Data.ByteString.Lazy as BL
import Data.Int (Int16)
import Data.Word (Word16, Word32)

-- | Sample rate for all generated sounds (Hz).
sampleRate :: Int
sampleRate = 22050

-- | Duration in milliseconds to sample count.
msToSamples :: Int -> Int
msToSamples ms = sampleRate * ms `div` 1000

-- | Clamp a value to [-1, 1] and encode as signed 16-bit sample.
toSample :: Double -> Int16
toSample v = round (maxAmplitude * max (-1.0) (min 1.0 v))
  where
    maxAmplitude :: Double
    maxAmplitude = 32767.0

-- | Write a mono 16-bit PCM WAV file.
writeWav :: FilePath -> [Int16] -> IO ()
writeWav path samples = do
  let numSamples = length samples
      dataSize = fromIntegral numSamples * bytesPerSample :: Word32
      fileSize = dataSize + riffHeaderSize
      header =
        B.byteString (BS.pack [0x52, 0x49, 0x46, 0x46]) -- "RIFF"
          <> B.word32LE fileSize
          <> B.byteString (BS.pack [0x57, 0x41, 0x56, 0x45]) -- "WAVE"
          <> B.byteString (BS.pack [0x66, 0x6D, 0x74, 0x20]) -- "fmt "
          <> B.word32LE fmtChunkSize
          <> B.word16LE pcmFormat
          <> B.word16LE monoChannels
          <> B.word32LE (fromIntegral sampleRate)
          <> B.word32LE (fromIntegral sampleRate * bytesPerSample) -- byte rate
          <> B.word16LE (fromIntegral bytesPerSample) -- block align
          <> B.word16LE bitsPerSample
          <> B.byteString (BS.pack [0x64, 0x61, 0x74, 0x61]) -- "data"
          <> B.word32LE dataSize
          <> mconcat (map B.int16LE samples)
  BL.writeFile path (B.toLazyByteString header)
  where
    bytesPerSample :: Word32
    bytesPerSample = 2

    riffHeaderSize :: Word32
    riffHeaderSize = 36

    fmtChunkSize :: Word32
    fmtChunkSize = 16

    pcmFormat :: Word16
    pcmFormat = 1

    monoChannels :: Word16
    monoChannels = 1

    bitsPerSample :: Word16
    bitsPerSample = 16
