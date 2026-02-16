-- | Song rendering pipeline: Song → [Int16].
--
-- Renders each section's tracks into a sparse 'SampleMap', mixes them
-- with per-track gain, repeats sections, concatenates, normalizes,
-- and converts to 16-bit PCM.
module GBSynth.Render
  ( -- * Rendering
    renderSong,
    renderSfx,

    -- * Mixing utilities
    SampleMap,
    mixAt,
    renderBuffer,
    layerWeighted,
    normalizeSignal,
  )
where

import Data.Int (Int16)
import Data.List (foldl')
import qualified Data.Map.Strict as Map
import GBSynth.Instrument (renderNote)
import GBSynth.Pattern (NoteEvent (..), Pattern (..))
import GBSynth.Song (Section (..), Song (..), Track (..))
import GBSynth.WAV (sampleRate, toSample)

-- | Sparse sample buffer for efficient random-access mixing.
type SampleMap = Map.Map Int Double

-- | Mix samples into a sparse buffer at a given sample offset.
mixAt :: SampleMap -> Int -> [Double] -> SampleMap
mixAt buf offset samples =
  foldl' (\m (i, v) -> Map.insertWith (+) i v m) buf (zip [offset ..] samples)

-- | Convert a sparse sample map to a dense list of the given length.
renderBuffer :: Int -> SampleMap -> [Double]
renderBuffer len buf = [Map.findWithDefault 0.0 i buf | i <- [0 .. len - 1]]

-- | Layer signals with explicit per-signal gain.
layerWeighted :: [(Double, [Double])] -> [Double]
layerWeighted pairs =
  let maxLen = maximum (map (length . snd) pairs)
      padded = map (\(g, s) -> map (* g) (s ++ replicate (maxLen - length s) 0.0)) pairs
   in foldl1 (zipWith (+)) padded

-- | Normalize a signal and scale to the given peak level.
normalizeSignal :: Double -> [Double] -> [Int16]
normalizeSignal level signal =
  let peak = maximum (map abs signal)
      normalized = if peak > minPeak then map (/ peak) signal else signal
   in map (toSample . (* level)) normalized
  where
    minPeak :: Double
    minPeak = 0.001

-- | Render SFX: layer weighted signals, normalize, convert to Int16.
renderSfx :: Double -> [(Double, [Double])] -> [Int16]
renderSfx level pairs = normalizeSignal level (layerWeighted pairs)

-- | Render a complete song to 16-bit PCM samples.
renderSong :: Song -> [Int16]
renderSong (Song tempo stepsPerBeat sections) =
  let samplesPerStep = sampleRate * secondsPerMinute `div` (tempo * stepsPerBeat)
      sectionAudio = concatMap (renderSection samplesPerStep) sections
   in normalizeSignal defaultMusicLevel sectionAudio
  where
    secondsPerMinute :: Int
    secondsPerMinute = 60

    defaultMusicLevel :: Double
    defaultMusicLevel = 0.8

-- | Render one section (all repeats).
renderSection :: Int -> Section -> [Double]
renderSection samplesPerStep (Section _name repeats tracks) =
  let onePass = renderSectionOnce samplesPerStep tracks
   in concat (replicate repeats onePass)

-- | Render a single pass of a section's parallel tracks.
renderSectionOnce :: Int -> [Track] -> [Double]
renderSectionOnce samplesPerStep tracks =
  case tracks of
    [] -> []
    _ ->
      let rendered = map (renderTrack samplesPerStep) tracks
          gains = map trkGain tracks
          maxLen = maximum (map length rendered)
          padded = map (\s -> s ++ replicate (maxLen - length s) 0.0) rendered
       in foldl1 (zipWith (+)) (zipWith (\g s -> map (* g) s) gains padded)

-- | Render a single track: step through the pattern, rendering notes.
--
-- Each 'NoteOn' sustains through subsequent 'Rest' steps until the next
-- 'NoteOn' or 'NoteOff'. This is standard tracker behaviour — 'Rest'
-- means "let the previous note ring".
renderTrack :: Int -> Track -> [Double]
renderTrack samplesPerStep (Track inst pat _gain) =
  let steps = patEvents pat
      totalSamples = patSteps pat * samplesPerStep
      noteSpans = extractNotes 0 steps
      buf = foldl' addNote Map.empty noteSpans
   in renderBuffer totalSamples buf
  where
    -- \| Walk the event list and produce (stepIndex, midi, velocity, durationSteps)
    -- tuples. A note's duration is 1 + the number of trailing Rests.
    extractNotes :: Int -> [NoteEvent] -> [(Int, Int, Double, Int)]
    extractNotes _ [] = []
    extractNotes idx (NoteOn midi vel : rest) =
      let dur = 1 + countSustain rest
       in (idx, midi, vel, dur) : extractNotes (idx + 1) rest
    extractNotes idx (_ : rest) = extractNotes (idx + 1) rest

    -- \| Count consecutive Rest events (note sustain continuation).
    countSustain :: [NoteEvent] -> Int
    countSustain (Rest : rest) = 1 + countSustain rest
    countSustain _ = 0

    addNote :: SampleMap -> (Int, Int, Double, Int) -> SampleMap
    addNote buf (stepIdx, midi, vel, durSteps) =
      let offset = stepIdx * samplesPerStep
          noteDur = durSteps * samplesPerStep
          noteSamples = renderNote inst midi noteDur
          scaled = map (* vel) noteSamples
       in mixAt buf offset scaled
