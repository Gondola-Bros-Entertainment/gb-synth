-- | Post-processing audio effects.
--
-- Pure transformations on @[Double]@ signal buffers. Apply after
-- synthesis, before final normalisation and conversion to @Int16@.
module GBSynth.Effects
  ( -- * Effects
    bitCrush,
    echo,
    fadeIn,
    fadeOut,
    reverseSignal,
    mix,
  )
where

import Data.List (foldl')

-- | Reduce bit depth for lo-fi retro crunch.
--
-- @bitCrush bits signal@ quantises each sample to @bits@ levels.
-- Lower values produce harsher distortion (4–8 is the retro sweet
-- spot).
bitCrush :: Int -> [Double] -> [Double]
bitCrush bits signal =
  let clampedBits = max 1 bits
      levels = (2 :: Double) ** fromIntegral clampedBits
   in map (\s -> fromIntegral (floor (s * levels) :: Int) / levels) signal

-- | Simple delay-line echo.
--
-- @echo delaySamples decay signal@ repeats the signal with the given
-- delay, mixing each repetition at @decay@ of the previous amplitude.
-- The output is longer than the input by @delaySamples@.
echo :: Int -> Double -> [Double] -> [Double]
echo delaySamples decay signal =
  let len = length signal
      outLen = len + delaySamples
      base = signal ++ replicate delaySamples 0.0
      delayed = replicate delaySamples 0.0 ++ map (* decay) signal ++ replicate (max 0 (outLen - delaySamples - len)) 0.0
   in zipWith (+) base (take outLen delayed)

-- | Linear fade in over the first @n@ samples.
--
-- Samples beyond @n@ are unchanged.
fadeIn :: Int -> [Double] -> [Double]
fadeIn n signal =
  let fadeDur = max 1 n
   in zipWith
        ( \i s ->
            if i < fadeDur
              then s * (fromIntegral i / fromIntegral fadeDur)
              else s
        )
        [(0 :: Int) ..]
        signal

-- | Linear fade out over the last @n@ samples.
--
-- Samples before the fade region are unchanged.
fadeOut :: Int -> [Double] -> [Double]
fadeOut n signal =
  let len = length signal
      fadeDur = max 1 n
      fadeStart = max 0 (len - fadeDur)
   in zipWith
        ( \i s ->
            if i >= fadeStart
              then s * (fromIntegral (len - 1 - i) / fromIntegral fadeDur)
              else s
        )
        [(0 :: Int) ..]
        signal

-- | Reverse an audio signal.
--
-- Useful for cymbal swells, reversed impacts, and similar effects.
reverseSignal :: [Double] -> [Double]
reverseSignal = reverse

-- | Mix multiple signals together with equal gain.
--
-- Shorter signals are zero-padded to match the longest. The output
-- is the sum of all inputs (no normalisation — apply 'normalizeSignal'
-- afterwards if needed).
mix :: [[Double]] -> [Double]
mix [] = []
mix signals =
  let maxLen = foldl' (\acc s -> max acc (length s)) 0 signals
      padded = map (\s -> s ++ replicate (maxLen - length s) 0.0) signals
   in foldl' (zipWith (+)) (replicate maxLen 0.0) padded
