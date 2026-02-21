-- | Tracker-style step pattern.
--
-- A 'Pattern' is a fixed-length grid of 'NoteEvent' steps, typically
-- at 16th-note resolution (4 steps per beat).
module GBSynth.Pattern
  ( -- * Types
    NoteEvent (..),
    Pattern (..),

    -- * Constructors
    fromNotes,
    fromHits,
    restPattern,
  )
where

-- | A single step in a pattern.
data NoteEvent
  = -- | Play a note: MIDI note number, velocity (0.0–1.0)
    NoteOn !Int !Double
  | -- | Silence this step (note off)
    NoteOff
  | -- | Rest — let previous note ring
    Rest
  deriving (Show, Eq)

-- | A fixed-length sequence of note events.
data Pattern = Pattern
  { -- | Number of steps in this pattern
    patSteps :: !Int,
    -- | Events for each step (length must equal 'patSteps')
    patEvents :: ![NoteEvent]
  }
  deriving (Show, Eq)

-- | Build a pattern from a list of optional MIDI notes.
--
-- @Nothing@ becomes 'Rest', @Just n@ becomes @NoteOn n 1.0@ (full velocity).
fromNotes :: [Maybe Int] -> Pattern
fromNotes notes =
  Pattern
    { patSteps = length notes,
      patEvents = map toEvent notes
    }
  where
    toEvent :: Maybe Int -> NoteEvent
    toEvent Nothing = Rest
    toEvent (Just n) = NoteOn n fullVelocity

    fullVelocity :: Double
    fullVelocity = 1.0

-- | Build a percussion pattern from hit positions.
--
-- @fromHits totalSteps hitSteps@ creates a pattern of @totalSteps@ length
-- with 'NoteOn' at each index in @hitSteps@ and 'NoteOff' elsewhere.
-- The MIDI note is fixed at 0 (ignored by 'GBSynth.Instrument.Sample' instruments).
fromHits :: Int -> [Int] -> Pattern
fromHits len hits =
  Pattern
    { patSteps = len,
      patEvents = map toHit [0 .. len - 1]
    }
  where
    toHit :: Int -> NoteEvent
    toHit i
      | i `elem` hits = NoteOn 0 fullVelocity
      | otherwise = NoteOff

    fullVelocity :: Double
    fullVelocity = 1.0

-- | A silent pattern of the given length.
restPattern :: Int -> Pattern
restPattern n =
  Pattern
    { patSteps = n,
      patEvents = replicate n Rest
    }
