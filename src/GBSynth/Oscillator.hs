-- | Waveform oscillators and frequency utilities.
--
-- Each oscillator produces normalised @[-1, 1]@ output.
-- Tonal waveforms use direct phase computation from the sample index;
-- noise uses an LCG pseudo-random generator.
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
--
-- The Game Boy had two square-wave channels with variable duty cycle
-- (12.5%, 25%, 50%, 75%). Use 'Pulse' to access all four; 'Square'
-- is equivalent to @Pulse 0.5@.
data Waveform
  = Sine
  | Square
  | Triangle
  | Sawtooth
  | Noise
  | -- | Pulse wave with configurable duty cycle (0.0–1.0).
    --   The duty cycle is the fraction of each period spent high.
    --   GB standard values: 0.125, 0.25, 0.5, 0.75.
    Pulse !Double
  deriving (Show, Eq)

-- | Generate samples for the given waveform, frequency, and duration.
oscillate :: Waveform -> Double -> Int -> [Double]
oscillate Sine freq n = sineOsc freq n
oscillate Square freq n = pulseOsc defaultDuty freq n
oscillate Triangle freq n = triangleOsc freq n
oscillate Sawtooth freq n = sawtoothOsc freq n
oscillate Noise _freq n = noiseOsc n
oscillate (Pulse duty) freq n = pulseOsc duty freq n

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
      phases = take n (drop 1 (scanl (+) 0.0 (repeat delta)))
   in map (\ph -> sin (ph * twoPi)) phases

-- | Pulse wave: +1 for the duty fraction of each cycle, -1 for the rest.
--
-- 'Square' delegates here with duty = 0.5.  GB duty values:
-- 0.125 (thin buzz), 0.25 (nasal), 0.5 (hollow), 0.75 (nasal inverted).
pulseOsc :: Double -> Double -> Int -> [Double]
pulseOsc duty freq n =
  let srf = fromIntegral sampleRate
   in [ let phase = fromIntegral i * freq / srf
            frac = phase - fromIntegral (floor phase :: Int)
         in if frac < duty then 1.0 else -1.0
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
    maxLcg = 4294967295.0

    lcgStep :: Word32 -> Word32
    lcgStep x = x * lcgMultiplier + lcgIncrement
      where
        lcgMultiplier :: Word32
        lcgMultiplier = 1103515245
        lcgIncrement :: Word32
        lcgIncrement = 12345

-- ---------------------------------------------------------------------------
-- Constants
-- ---------------------------------------------------------------------------

-- | Default duty cycle for 'Square' (50%).
defaultDuty :: Double
defaultDuty = 0.5

-- | 2 * pi, cached for readability.
twoPi :: Double
twoPi = 2.0 * pi
