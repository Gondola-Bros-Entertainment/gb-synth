-- | Song structure: tracks, sections, and arrangement.
--
-- A 'Song' is a sequence of 'Section's, each containing parallel 'Track's.
-- This gives structured music with intro/verse/chorus/outro progressions.
module GBSynth.Song
  ( -- * Types
    Track (..),
    Section (..),
    Song (..),

    -- * Smart constructors
    track,
    section,
    song,
  )
where

import GBSynth.Instrument (Instrument)
import GBSynth.Pattern (Pattern)

-- | A single track: one instrument playing one pattern.
data Track = Track
  { -- | Instrument to render with
    trkInstrument :: !Instrument,
    -- | Note pattern
    trkPattern :: !Pattern,
    -- | Mix level (0.0–1.0)
    trkGain :: !Double
  }
  deriving (Show, Eq)

-- | A song section (e.g. intro, verse, chorus).
data Section = Section
  { -- | Human-readable label (e.g. "intro", "verse")
    secName :: !String,
    -- | How many times to play this section
    secRepeats :: !Int,
    -- | Parallel tracks in this section
    secTracks :: ![Track]
  }
  deriving (Show, Eq)

-- | A complete song.
data Song = Song
  { -- | Tempo in beats per minute
    songTempo :: !Int,
    -- | Grid resolution (e.g. 4 = 16th notes)
    songStepsPerBeat :: !Int,
    -- | Ordered sequence of sections
    songSections :: ![Section]
  }
  deriving (Show, Eq)

-- | Build a track with the given instrument, pattern, and gain.
track :: Instrument -> Pattern -> Double -> Track
track = Track

-- | Build a named section with repeat count and tracks.
section :: String -> Int -> [Track] -> Section
section = Section

-- | Build a song at the given tempo and grid resolution.
song :: Int -> Int -> [Section] -> Song
song = Song
