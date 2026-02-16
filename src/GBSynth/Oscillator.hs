-- | Waveform oscillators and frequency utilities.
--
-- Each oscillator produces normalised @[-1, 1]@ output with a running
-- phase accumulator.
module GBSynth.Oscillator
  ( -- * Waveform type
    Waveform (..),

    -- * Oscillator
    oscillate,

    -- * Frequency
    noteFreq,
  )
where

import Data.Word (Word32)
import GBSynth.WAV (sampleRate)

-- | Available waveform shapes.
data Waveform
  = Sine
  | Square
  | Triangle
  | Sawtooth
  | Noise
  deriving (Show, Eq)

-- | Generate samples for the given waveform, frequency, and duration.
--
-- Uses a running phase accumulator so multi-cycle output stays coherent.
oscillate :: Waveform -> Double -> Int -> [Double]
oscillate Sine freq n = sineOsc freq n
oscillate Square freq n = squareOsc freq n
oscillate Triangle freq n = triangleOsc freq n
oscillate Sawtooth freq n = sawtoothOsc freq n
oscillate Noise _freq n = noiseOsc n

-- | MIDI note number to frequency in Hz (A4 = 440 Hz).
noteFreq :: Int -> Double
noteFreq midi = concertA * (2.0 ** (fromIntegral (midi - concertAMidi) / semitones))
  where
    concertA :: Double
    concertA = 440.0
    concertAMidi :: Int
    concertAMidi = 69
    semitones :: Double
    semitones = 12.0

-- ---------------------------------------------------------------------------
-- Oscillator implementations
-- ---------------------------------------------------------------------------

-- | Sine wave via phase accumulator.
sineOsc :: Double -> Int -> [Double]
sineOsc freq n =
  let srf = fromIntegral sampleRate
      delta = freq / srf
      phases = take n (tail (scanl (+) 0.0 (repeat delta)))
   in map (\ph -> sin (ph * twoPi)) phases

-- | Square wave: +1 for first half of cycle, -1 for second half.
squareOsc :: Double -> Int -> [Double]
squareOsc freq n =
  let srf = fromIntegral sampleRate
   in [ let phase = fromIntegral i * freq / srf
            frac = phase - fromIntegral (floor phase :: Int)
         in if frac < 0.5 then 1.0 else -1.0
      | i <- [0 .. n - 1]
      ]

-- | Triangle wave: linear ramp between -1 and 1.
triangleOsc :: Double -> Int -> [Double]
triangleOsc freq n =
  let srf = fromIntegral sampleRate
   in [ let phase = fromIntegral i * freq / srf
         in 2.0 * abs (2.0 * (phase - fromIntegral (floor (phase + 0.5) :: Int))) - 1.0
      | i <- [0 .. n - 1]
      ]

-- | Sawtooth wave: linear ramp from -1 to 1.
sawtoothOsc :: Double -> Int -> [Double]
sawtoothOsc freq n =
  let srf = fromIntegral sampleRate
   in [ let phase = fromIntegral i * freq / srf
         in 2.0 * (phase - fromIntegral (floor phase :: Int)) - 1.0
      | i <- [0 .. n - 1]
      ]

-- | White noise via LCG pseudo-random generator.
noiseOsc :: Int -> [Double]
noiseOsc n =
  let seeds = take n (iterate lcgStep noiseSeed)
   in map (\s -> fromIntegral s / maxLcg * 2.0 - 1.0) seeds
  where
    noiseSeed :: Word32
    noiseSeed = 42

    maxLcg :: Double
    maxLcg = 2147483647.0

    lcgStep :: Word32 -> Word32
    lcgStep x = x * lcgMultiplier + lcgIncrement
      where
        lcgMultiplier :: Word32
        lcgMultiplier = 1103515245
        lcgIncrement :: Word32
        lcgIncrement = 12345

-- | 2 * pi, cached for readability.
twoPi :: Double
twoPi = 2.0 * pi
