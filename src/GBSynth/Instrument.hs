-- | Instrument types for the gb-synth sequencer.
--
-- Two kinds of instrument:
--
--   * 'Synth' — oscillator + ADSR envelope + gain (for tonal parts)
--   * 'Sample' — pre-rendered waveform buffer + gain (for drums/FX)
--
-- 'renderNote' handles both: synthesised notes are generated on the fly,
-- sample instruments are truncated or zero-padded to fit the requested
-- duration — exactly how hardware trackers (MOD\/XM\/IT) work.
module GBSynth.Instrument
  ( -- * Types
    Instrument (..),

    -- * Rendering
    renderNote,

    -- * Presets
    bass,
    lead,
    pad,
  )
where

import GBSynth.Envelope (ADSR (..), longPad, shortPluck)
import GBSynth.Oscillator (Waveform (..), noteFreq, oscillate)
import GBSynth.WAV (sampleRate)

-- | A synthesizer instrument.
data Instrument
  = -- | Oscillator + ADSR + gain — generates audio from a MIDI note.
    Synth !Waveform !ADSR !Double
  | -- | Pre-rendered sample buffer + gain — plays a fixed waveform.
    --   MIDI note number is ignored; the buffer is the sound.
    Sample ![Double] !Double
  deriving (Show, Eq)

-- | Render a single note.
--
-- For 'Synth', generates @dur@ samples shaped by oscillator, envelope,
-- and gain.  For 'Sample', outputs up to @dur@ samples from the buffer,
-- zero-padded if the buffer is shorter.
renderNote :: Instrument -> Int -> Int -> [Double]
renderNote (Synth wf adsr gain) midiNote dur =
  let freq = noteFreq midiNote
      raw = oscillate wf freq dur
      env = renderEnvelopeForNote adsr dur
   in zipWith (\r e -> r * e * gain) raw env
renderNote (Sample buffer gain) _midiNote dur =
  let bufLen = length buffer
      trimmed = take dur buffer
      padLen = max 0 (dur - bufLen)
   in map (* gain) (trimmed ++ replicate padLen 0.0)

-- | Render the envelope for a note, using 80% of duration as note-on.
renderEnvelopeForNote :: ADSR -> Int -> [Double]
renderEnvelopeForNote (ADSR attack decay sustain release) totalSamples =
  let noteOnSamples = totalSamples * noteOnFraction `div` noteOnDivisor
   in [envAt i noteOnSamples | i <- [0 .. totalSamples - 1]]
  where
    noteOnFraction :: Int
    noteOnFraction = 4

    noteOnDivisor :: Int
    noteOnDivisor = 5

    attackSamples :: Int
    attackSamples = round (attack * srf)

    decaySamples :: Int
    decaySamples = round (decay * srf)

    srf :: Double
    srf = fromIntegral sampleRate

    envAt :: Int -> Int -> Double
    envAt i noteOn
      | i < attackSamples =
          if attackSamples > 0
            then fromIntegral i / fromIntegral attackSamples
            else 1.0
      | i < attackSamples + decaySamples =
          let elapsed = fromIntegral (i - attackSamples) / fromIntegral decaySamples
           in 1.0 + (sustain - 1.0) * elapsed
      | i < noteOn =
          sustain
      | otherwise =
          let relSamples = max 1 (round (release * srf) :: Int)
              elapsed = fromIntegral (i - noteOn) / fromIntegral relSamples
           in max 0.0 (sustain * (1.0 - elapsed))

-- ---------------------------------------------------------------------------
-- Presets
-- ---------------------------------------------------------------------------

-- | Square wave bass with short pluck envelope.
bass :: Instrument
bass = Synth Square shortPluck bassGain
  where
    bassGain :: Double
    bassGain = 0.6

-- | Sine wave lead with short pluck envelope.
lead :: Instrument
lead = Synth Sine shortPluck leadGain
  where
    leadGain :: Double
    leadGain = 0.5

-- | Sine wave pad with long attack and sustain.
pad :: Instrument
pad = Synth Sine longPad padGain
  where
    padGain :: Double
    padGain = 0.4
