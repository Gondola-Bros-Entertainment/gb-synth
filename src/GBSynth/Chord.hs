-- | Programmatic chord construction from MIDI note numbers.
--
-- Build chords, inversions, and progressions without hand-coding
-- MIDI arrays. Replaces patterns like @[(57, [57, 60, 64])]@ with
-- @chord 57 Minor@.
--
-- Supports triads (Major, Minor, Diminished, Augmented, Sus2, Sus4)
-- and seventh chords (Dominant7, Major7, Minor7, Diminished7).
module GBSynth.Chord
  ( -- * Types
    Quality (..),

    -- * Construction
    chord,
    inversion,
    chordProgression,

    -- * Common progressions
    pop1564,
    blues145,
    minorClassic,
  )
where

-- | Chord quality — determines the intervals above the root.
data Quality
  = -- | Root, major third (+4), perfect fifth (+7)
    Major
  | -- | Root, minor third (+3), perfect fifth (+7)
    Minor
  | -- | Root, minor third (+3), diminished fifth (+6)
    Diminished
  | -- | Root, major third (+4), augmented fifth (+8)
    Augmented
  | -- | Root, major second (+2), perfect fifth (+7)
    Sus2
  | -- | Root, perfect fourth (+5), perfect fifth (+7)
    Sus4
  | -- | Root, major third (+4), perfect fifth (+7), minor seventh (+10)
    Dominant7
  | -- | Root, major third (+4), perfect fifth (+7), major seventh (+11)
    Major7
  | -- | Root, minor third (+3), perfect fifth (+7), minor seventh (+10)
    Minor7
  | -- | Root, minor third (+3), diminished fifth (+6), diminished seventh (+9)
    Diminished7
  deriving (Show, Eq)

-- | Build a chord from a root MIDI note and quality.
--
-- Returns a list of MIDI note numbers (root position).
--
-- >>> chord 60 Major
-- [60, 64, 67]
--
-- >>> chord 57 Minor
-- [57, 60, 64]
--
-- >>> chord 60 Dominant7
-- [60, 64, 67, 70]
chord :: Int -> Quality -> [Int]
chord root quality = map (root +) (intervals quality)

-- | Intervals (semitones above root) for each chord quality.
intervals :: Quality -> [Int]
intervals Major = [0, majorThird, perfectFifth]
intervals Minor = [0, minorThird, perfectFifth]
intervals Diminished = [0, minorThird, diminishedFifth]
intervals Augmented = [0, majorThird, augmentedFifth]
intervals Sus2 = [0, majorSecond, perfectFifth]
intervals Sus4 = [0, perfectFourth, perfectFifth]
intervals Dominant7 = [0, majorThird, perfectFifth, minorSeventh]
intervals Major7 = [0, majorThird, perfectFifth, majorSeventh]
intervals Minor7 = [0, minorThird, perfectFifth, minorSeventh]
intervals Diminished7 = [0, minorThird, diminishedFifth, diminishedSeventh]

-- | Apply an inversion to a chord voicing.
--
-- Inversion 1 moves the lowest note up an octave.
-- Inversion 2 moves the two lowest notes up an octave.
-- Inversion 0 (or negative) returns the chord unchanged.
--
-- >>> inversion 1 [60, 64, 67]
-- [64, 67, 72]
inversion :: Int -> [Int] -> [Int]
inversion n notes
  | n <= 0 = notes
  | otherwise = case notes of
      [] -> []
      (lowest : rest) -> inversion (n - 1) (rest ++ [lowest + semitonesPerOctave])

-- | Build a chord progression from root notes and qualities.
--
-- Returns pairs of @(root, [midiNotes])@ — ready to plug into
-- pattern construction.
--
-- >>> chordProgression [(60, Major), (65, Major)]
-- [(60, [60, 64, 67]), (65, [65, 69, 72])]
chordProgression :: [(Int, Quality)] -> [(Int, [Int])]
chordProgression = map (\(root, quality) -> (root, chord root quality))

-- ---------------------------------------------------------------------------
-- Common progressions
-- ---------------------------------------------------------------------------

-- | I–V–vi–IV in C major (C G Am F) — the pop progression.
pop1564 :: [(Int, [Int])]
pop1564 =
  chordProgression
    [ (middleC, Major),
      (middleC + perfectFifth, Major),
      (middleC + majorSixth, Minor),
      (middleC + perfectFourth, Major)
    ]

-- | I–IV–V in C major (C F G) — the 12-bar blues foundation.
blues145 :: [(Int, [Int])]
blues145 =
  chordProgression
    [ (middleC, Major),
      (middleC + perfectFourth, Major),
      (middleC + perfectFifth, Major)
    ]

-- | i–iv–V in A minor (Am Dm E) — classic minor progression.
minorClassic :: [(Int, [Int])]
minorClassic =
  chordProgression
    [ (concertAMidi, Minor),
      (concertAMidi + perfectFourth, Minor),
      (concertAMidi + perfectFifth, Major)
    ]

-- ---------------------------------------------------------------------------
-- Interval constants (semitones)
-- ---------------------------------------------------------------------------

-- | Major second: 2 semitones.
majorSecond :: Int
majorSecond = 2

-- | Minor third: 3 semitones.
minorThird :: Int
minorThird = 3

-- | Major third: 4 semitones.
majorThird :: Int
majorThird = 4

-- | Perfect fourth: 5 semitones.
perfectFourth :: Int
perfectFourth = 5

-- | Diminished fifth (tritone): 6 semitones.
diminishedFifth :: Int
diminishedFifth = 6

-- | Perfect fifth: 7 semitones.
perfectFifth :: Int
perfectFifth = 7

-- | Augmented fifth: 8 semitones.
augmentedFifth :: Int
augmentedFifth = 8

-- | Major sixth: 9 semitones.
majorSixth :: Int
majorSixth = 9

-- | Diminished seventh: 9 semitones (enharmonic with major sixth).
diminishedSeventh :: Int
diminishedSeventh = 9

-- | Minor seventh: 10 semitones.
minorSeventh :: Int
minorSeventh = 10

-- | Major seventh: 11 semitones.
majorSeventh :: Int
majorSeventh = 11

-- | Semitones per octave.
semitonesPerOctave :: Int
semitonesPerOctave = 12

-- | MIDI note number for middle C (C4).
middleC :: Int
middleC = 60

-- | MIDI note number for concert A (A3 = 57).
concertAMidi :: Int
concertAMidi = 57
