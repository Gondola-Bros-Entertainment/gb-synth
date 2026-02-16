-- | ADSR envelope generator.
--
-- Four-stage envelope: Attack (linear 0→1), Decay (exponential 1→sustain),
-- Sustain (hold), Release (exponential sustain→0).
module GBSynth.Envelope
  ( -- * Types
    ADSR (..),

    -- * Rendering
    renderEnvelope,

    -- * Presets
    percussive,
    shortPluck,
    longPad,
    organ,
  )
where

-- | ADSR envelope parameters.
data ADSR = ADSR
  { -- | Seconds for linear ramp from 0 to 1
    adsrAttack :: !Double,
    -- | Seconds for exponential decay from 1 to sustain level
    adsrDecay :: !Double,
    -- | Hold level during sustain phase (0.0–1.0)
    adsrSustain :: !Double,
    -- | Seconds for exponential decay from sustain to 0
    adsrRelease :: !Double
  }
  deriving (Show, Eq)

-- | Render an envelope curve.
--
-- @renderEnvelope adsr noteOnSamples totalSamples@ produces a list of
-- @totalSamples@ amplitude values. The attack+decay+sustain occupy
-- @noteOnSamples@, and the release fills the remainder.
renderEnvelope :: ADSR -> Int -> Int -> [Double]
renderEnvelope adsr noteOnSamples totalSamples =
  [envelopeAt adsr noteOnSamples i | i <- [0 .. totalSamples - 1]]

-- | Compute the envelope amplitude at sample index @i@.
envelopeAt :: ADSR -> Int -> Int -> Double
envelopeAt (ADSR attack decay sustain release) noteOnSamples i
  | i < attackSamples =
      -- Attack: linear ramp 0 → 1
      if attackSamples > 0
        then fromIntegral i / fromIntegral attackSamples
        else 1.0
  | i < attackSamples + decaySamples =
      -- Decay: exponential 1 → sustain
      let elapsed = fromIntegral (i - attackSamples) / fromIntegral decaySamples
       in 1.0 + (sustain - 1.0) * elapsed
  | i < noteOnSamples =
      -- Sustain: hold at sustain level
      sustain
  | otherwise =
      -- Release: exponential sustain → 0
      let releaseSamples = max 1 (secondsToSamples release)
          elapsed = fromIntegral (i - noteOnSamples) / fromIntegral releaseSamples
          level = sustain * (1.0 - elapsed)
       in max 0.0 level
  where
    attackSamples = secondsToSamples attack
    decaySamples = secondsToSamples decay

-- | Convert seconds to sample count (at 22050 Hz).
secondsToSamples :: Double -> Int
secondsToSamples s = round (s * sampleRateF)
  where
    sampleRateF :: Double
    sampleRateF = 22050.0

-- ---------------------------------------------------------------------------
-- Presets
-- ---------------------------------------------------------------------------

-- | Fast attack, no sustain, short release. Good for drums and clicks.
percussive :: ADSR
percussive =
  ADSR
    { adsrAttack = 0.005,
      adsrDecay = 0.05,
      adsrSustain = 0.0,
      adsrRelease = 0.05
    }

-- | Fast attack, short decay, low sustain. Good for bass and arpeggios.
shortPluck :: ADSR
shortPluck =
  ADSR
    { adsrAttack = 0.005,
      adsrDecay = 0.15,
      adsrSustain = 0.3,
      adsrRelease = 0.1
    }

-- | Slow attack, no decay, high sustain, long release. Good for pads.
longPad :: ADSR
longPad =
  ADSR
    { adsrAttack = 0.08,
      adsrDecay = 0.0,
      adsrSustain = 1.0,
      adsrRelease = 0.3
    }

-- | Instant attack, no decay, full sustain, short release. Organ-like.
organ :: ADSR
organ =
  ADSR
    { adsrAttack = 0.001,
      adsrDecay = 0.0,
      adsrSustain = 1.0,
      adsrRelease = 0.05
    }
