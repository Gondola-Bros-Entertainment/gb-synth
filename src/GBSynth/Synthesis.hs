-- | Low-level synthesis building blocks for sound effects.
--
-- These are the reusable primitives that every SFX generator needs:
-- frequency sweeps, noise bursts, envelopes, and simple waveforms.
-- All pure — each function produces @[Double]@ samples in @[-1, 1]@.
module GBSynth.Synthesis
  ( -- * Envelopes
    expDecay,
    attackDecay,

    -- * Swept oscillators
    sineSweep,
    sineSweepAD,

    -- * Noise
    noiseBurst,

    -- * Simple waveforms
    squareWaveDecay,

    -- * Utilities
    silence,
  )
where

import GBSynth.WAV (sampleRate)

-- | Exponential decay envelope.
--
-- @expDecay rate t@ returns @exp(-rate * t)@ — 1.0 at @t=0@,
-- approaching 0 as @t@ increases.
expDecay :: Double -> Double -> Double
expDecay rate t = exp (negate rate * t)

-- | Attack-decay envelope: linear ramp up, then exponential decay.
--
-- @attackDecay attackSec decayRate t@ ramps linearly from 0 to 1 over
-- @attackSec@ seconds, then decays exponentially at @decayRate@.
attackDecay :: Double -> Double -> Double -> Double
attackDecay attackSec decayRate t
  | t < attackSec = t / attackSec
  | otherwise = expDecay decayRate (t - attackSec)

-- | Sine wave swept from @startHz@ to @endHz@ with exponential decay.
--
-- Uses a phase accumulator for coherent multi-cycle output.
-- The frequency interpolates linearly from start to end over the
-- duration, while the amplitude decays at the given rate.
sineSweep :: Double -> Double -> Double -> Int -> [Double]
sineSweep startHz endHz decayRate n =
  let nf = fromIntegral n
      srf = fromIntegral sampleRate
      deltas =
        [ (startHz + (endHz - startHz) * (fromIntegral i / nf)) / srf
        | i <- [0 .. n - 1]
        ]
      phases = drop 1 (scanl (+) 0.0 deltas)
   in zipWith
        ( \i ph ->
            let t = fromIntegral i / nf
                amp = expDecay decayRate t
             in sin (ph * twoPi) * amp
        )
        [(0 :: Int) ..]
        phases

-- | Sine sweep with attack-decay envelope.
--
-- Like 'sineSweep', but uses 'attackDecay' instead of pure exponential
-- decay. The @attackMs@ parameter specifies the attack time in
-- milliseconds.
sineSweepAD :: Double -> Double -> Double -> Double -> Int -> [Double]
sineSweepAD startHz endHz attackMs decayRate n =
  let nf = fromIntegral n
      srf = fromIntegral sampleRate
      attackSec = attackMs / msPerSec
      deltas =
        [ (startHz + (endHz - startHz) * (fromIntegral i / nf)) / srf
        | i <- [0 .. n - 1]
        ]
      phases = drop 1 (scanl (+) 0.0 deltas)
   in zipWith
        ( \i ph ->
            let t = fromIntegral i / fromIntegral sampleRate
                amp = attackDecay attackSec decayRate t
             in sin (ph * twoPi) * amp
        )
        [(0 :: Int) ..]
        phases
  where
    msPerSec :: Double
    msPerSec = 1000.0

-- | White noise burst with exponential decay.
--
-- Uses an LCG pseudo-random generator seeded per-sample for
-- deterministic output. The decay rate controls how quickly the
-- noise fades.
noiseBurst :: Double -> Int -> [Double]
noiseBurst decayRate n =
  [ let t = fromIntegral i / fromIntegral n
        amp = expDecay decayRate t
        raw = fromIntegral (lcg i) / lcgMax :: Double
     in (raw * 2.0 - 1.0) * amp
  | i <- [0 .. n - 1]
  ]
  where
    lcg :: Int -> Int
    lcg seed =
      let step x = (x * lcgMultiplier + lcgIncrement) `mod` lcgModulus
       in step (step (step (step (step (seed + lcgSeed)))))

    lcgMultiplier :: Int
    lcgMultiplier = 1103515245

    lcgIncrement :: Int
    lcgIncrement = 12345

    lcgModulus :: Int
    lcgModulus = 2147483648

    lcgSeed :: Int
    lcgSeed = 42

    lcgMax :: Double
    lcgMax = 2147483647.0

-- | Square wave with exponential decay.
--
-- Produces a square wave at the given frequency, with amplitude
-- shaped by exponential decay. Useful for percussive clicks and
-- alarm pulses.
squareWaveDecay :: Double -> Double -> Int -> [Double]
squareWaveDecay freq decayRate n =
  [ let t = fromIntegral i / fromIntegral sampleRate
        phase = t * freq
        amp = expDecay decayRate t
        val = if phase - fromIntegral (floor phase :: Int) < squareDuty then amp else negate amp
     in val
  | i <- [0 .. n - 1]
  ]
  where
    squareDuty :: Double
    squareDuty = 0.5

-- | Generate silence (all zeros) for the given number of samples.
silence :: Int -> [Double]
silence n = replicate n 0.0

-- ---------------------------------------------------------------------------
-- Internal constants
-- ---------------------------------------------------------------------------

-- | Two times pi.
twoPi :: Double
twoPi = 2.0 * pi
