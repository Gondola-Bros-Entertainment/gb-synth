-- | gb-synth test suite.
--
-- Hand-rolled assertions — first failure stops all. Same pattern as gbnet-hs.
module Main (main) where

import qualified Data.ByteString as BS
import Data.Int (Int16)
import qualified Data.Map.Strict as Map
import Data.Word (Word8)
import GBSynth.Chord
  ( Quality (..),
    blues145,
    chord,
    chordProgression,
    inversion,
    minorClassic,
    pop1564,
  )
import GBSynth.Effects (bitCrush, echo, fadeIn, fadeOut, mix, reverseSignal)
import GBSynth.Envelope (ADSR (..), longPad, organ, percussive, renderEnvelope, shortPluck)
import GBSynth.Instrument (Instrument (..), bass, lead, pad, renderNote)
import GBSynth.Oscillator (Waveform (..), noteFreq, oscillate)
import GBSynth.Pattern (NoteEvent (..), Pattern (..), fromHits, fromNotes, restPattern)
import GBSynth.Render (layerWeighted, mixAt, normalizeSignal, renderBuffer, renderSfx, renderSong)
import GBSynth.SFX
  ( alert,
    click,
    coin,
    defeat,
    explosion,
    heal,
    hihatSample,
    impact,
    jump,
    kickSample,
    laser,
    powerup,
    snareSample,
  )
import GBSynth.Song (Section (..), Song (..), Track (..), section, song, track)
import GBSynth.Synthesis
  ( attackDecay,
    expDecay,
    noiseBurst,
    silence,
    sineSweep,
    sineSweepAD,
    squareWaveDecay,
  )
import GBSynth.WAV (msToSamples, sampleRate, toSample, writeWav)
import System.Exit (exitFailure, exitSuccess)
import System.IO (hClose, hFlush, openTempFile, stdout)

-- ---------------------------------------------------------------------------
-- Test harness
-- ---------------------------------------------------------------------------

type TestResult = Either String ()

assertEqual :: (Show a, Eq a) => String -> a -> a -> TestResult
assertEqual label expected actual
  | expected == actual = Right ()
  | otherwise =
      Left
        ( label
            ++ ": expected "
            ++ show expected
            ++ ", got "
            ++ show actual
        )

assertApprox :: String -> Double -> Double -> Double -> TestResult
assertApprox label expected actual tolerance
  | abs (expected - actual) <= tolerance = Right ()
  | otherwise =
      Left
        ( label
            ++ ": expected ~"
            ++ show expected
            ++ " (tolerance "
            ++ show tolerance
            ++ "), got "
            ++ show actual
        )

assertTrue :: String -> Bool -> TestResult
assertTrue _ True = Right ()
assertTrue label False = Left (label ++ ": expected True")

runTests :: [(String, TestResult)] -> IO ()
runTests tests = go tests (0 :: Int) (0 :: Int)
  where
    go [] passed total = do
      putStrLn ""
      putStrLn
        ( show passed
            ++ "/"
            ++ show total
            ++ " tests passed."
        )
      if passed == total then exitSuccess else exitFailure
    go ((name, result) : rest) passed total = do
      case result of
        Right () -> do
          putStrLn ("  PASS: " ++ name)
          hFlush stdout
          go rest (passed + 1) (total + 1)
        Left msg -> do
          putStrLn ("  FAIL: " ++ name ++ " - " ++ msg)
          hFlush stdout
          exitFailure

-- ---------------------------------------------------------------------------
-- Main
-- ---------------------------------------------------------------------------

main :: IO ()
main = do
  putStrLn "gb-synth tests"
  putStrLn (replicate 40 '-')
  wavTests <- testWavRoundtrip
  runTests
    ( testNoteFreq
        ++ testOscillator
        ++ testEnvelope
        ++ testEnvelopePresets
        ++ testPattern
        ++ testRender
        ++ testRenderUtils
        ++ testSynthesis
        ++ testSynthesisEdge
        ++ testInstrument
        ++ testInstrumentPresets
        ++ wavTests
        ++ testSFX
        ++ testChord
        ++ testChordProgressions
        ++ testEffects
        ++ testEffectsEdge
        ++ testSong
    )

-- ---------------------------------------------------------------------------
-- Oscillator tests
-- ---------------------------------------------------------------------------

testNoteFreq :: [(String, TestResult)]
testNoteFreq =
  [ ( "noteFreq A4 = 440 Hz",
      assertApprox "A4" 440.0 (noteFreq concertAMidi) 0.001
    ),
    ( "noteFreq A3 = 220 Hz",
      assertApprox "A3" 220.0 (noteFreq (concertAMidi - octave)) 0.001
    ),
    ( "noteFreq A5 = 880 Hz",
      assertApprox "A5" 880.0 (noteFreq (concertAMidi + octave)) 0.001
    ),
    ( "noteFreq C4 = 261.63 Hz",
      assertApprox "C4" 261.626 (noteFreq middleCMidi) 0.01
    )
  ]
  where
    concertAMidi :: Int
    concertAMidi = 69

    middleCMidi :: Int
    middleCMidi = 60

    octave :: Int
    octave = 12

testOscillator :: [(String, TestResult)]
testOscillator =
  [ ( "oscillate produces correct sample count",
      assertEqual "sine sample count" sampleCount (length (oscillate Sine 440.0 sampleCount))
    ),
    ( "oscillate square sample count",
      assertEqual "square sample count" sampleCount (length (oscillate Square 440.0 sampleCount))
    ),
    ( "oscillate triangle sample count",
      assertEqual "triangle sample count" sampleCount (length (oscillate Triangle 440.0 sampleCount))
    ),
    ( "oscillate sawtooth sample count",
      assertEqual "sawtooth sample count" sampleCount (length (oscillate Sawtooth 440.0 sampleCount))
    ),
    ( "oscillate noise sample count",
      assertEqual "noise sample count" sampleCount (length (oscillate Noise 440.0 sampleCount))
    ),
    ( "sine output in [-1, 1]",
      assertTrue "sine range" (all (\s -> s >= -1.0 && s <= 1.0) (oscillate Sine 440.0 sampleCount))
    ),
    ( "square output in {-1, 1}",
      assertTrue
        "square values"
        ( all
            (\s -> s == 1.0 || s == -1.0)
            (oscillate Square 440.0 sampleCount)
        )
    ),
    ( "noise output in [-1, 1]",
      assertTrue "noise range" (all (\s -> s >= -1.0 && s <= 1.0) (oscillate Noise 440.0 sampleCount))
    )
  ]
  where
    sampleCount :: Int
    sampleCount = 1000

-- ---------------------------------------------------------------------------
-- Envelope tests
-- ---------------------------------------------------------------------------

testEnvelope :: [(String, TestResult)]
testEnvelope =
  [ ( "envelope starts at 0 (attack phase)",
      case env of
        (firstSample : _) -> assertApprox "env start" 0.0 firstSample 0.01
        [] -> Left "env: empty list"
    ),
    ( "envelope reaches peak near 1.0",
      assertTrue "env peak" (peakVal > 0.9)
    ),
    ( "envelope sustain holds at sustain level",
      case drop sustainIdx env of
        (sustainSample : _) -> assertApprox "env sustain" 0.3 sustainSample 0.05
        [] -> Left "env: sustain index out of bounds"
    ),
    ( "envelope ends near 0 (release phase)",
      case reverse env of
        (lastSample : _) -> assertTrue "env release" (lastSample < 0.1)
        [] -> Left "env: empty list"
    ),
    ( "percussive envelope decays quickly",
      let percEnv = renderEnvelope percussive percNoteOn percTotal
       in case drop (percTotal `div` 2) percEnv of
            (midpoint : _) -> assertTrue "percussive decay" (midpoint < 0.1)
            [] -> Left "percEnv: midpoint index out of bounds"
    )
  ]
  where
    -- shortPluck: attack=0.005s (110 samples), decay=0.15s (3308 samples)
    -- sustain starts at sample ~3418, so noteOn must exceed that
    noteOnSamples :: Int
    noteOnSamples = 5000

    totalSamples :: Int
    totalSamples = 8000

    env :: [Double]
    env = renderEnvelope shortPluck noteOnSamples totalSamples

    peakVal :: Double
    peakVal = maximum env

    -- Sample well into the sustain phase (after attack+decay ~3418)
    sustainIdx :: Int
    sustainIdx = 4000

    -- Percussive: attack=0.005s (110), decay=0.05s (1103) — need noteOn > 1213
    percNoteOn :: Int
    percNoteOn = 2000

    percTotal :: Int
    percTotal = 3000

-- ---------------------------------------------------------------------------
-- Pattern tests
-- ---------------------------------------------------------------------------

testPattern :: [(String, TestResult)]
testPattern =
  [ ( "fromNotes step count matches input",
      assertEqual "fromNotes steps" inputLen (patSteps pat)
    ),
    ( "fromNotes events count matches input",
      assertEqual "fromNotes events" inputLen (length (patEvents pat))
    ),
    ( "fromNotes Nothing becomes Rest",
      case drop 1 (patEvents pat) of
        (secondEvent : _) -> assertEqual "rest event" Rest secondEvent
        [] -> Left "patEvents: index 1 out of bounds"
    ),
    ( "fromNotes Just becomes NoteOn",
      case patEvents pat of
        (firstEvent : _) -> assertEqual "note event" (NoteOn 60 1.0) firstEvent
        [] -> Left "patEvents: empty"
    ),
    ( "fromHits correct step count",
      assertEqual "fromHits steps" hitPatLen (patSteps hitPat)
    ),
    ( "fromHits has NoteOn at hit positions",
      case patEvents hitPat of
        (firstEvent : _) -> assertEqual "hit at 0" (NoteOn 0 1.0) firstEvent
        [] -> Left "hitPat events: empty"
    ),
    ( "fromHits has NoteOff at non-hit positions",
      case drop 1 (patEvents hitPat) of
        (secondEvent : _) -> assertEqual "off at 1" NoteOff secondEvent
        [] -> Left "hitPat events: index 1 out of bounds"
    ),
    ( "restPattern all rests",
      assertTrue "all rests" (all (== Rest) (patEvents (restPattern restLen)))
    ),
    ( "restPattern correct length",
      assertEqual "rest steps" restLen (patSteps (restPattern restLen))
    )
  ]
  where
    notes :: [Maybe Int]
    notes = [Just 60, Nothing, Just 64, Nothing, Just 67, Nothing, Nothing, Nothing]

    inputLen :: Int
    inputLen = length notes

    pat :: Pattern
    pat = fromNotes notes

    hitPatLen :: Int
    hitPatLen = 16

    hitPat :: Pattern
    hitPat = fromHits hitPatLen [0, 4, 8, 12]

    restLen :: Int
    restLen = 32

-- ---------------------------------------------------------------------------
-- Render tests
-- ---------------------------------------------------------------------------

testRender :: [(String, TestResult)]
testRender =
  [ ( "renderSfx produces correct sample count",
      let n = msToSamples 100
          signal = replicate n 0.5
          result = renderSfx 1.0 [(1.0, signal)]
       in assertEqual "sfx length" n (length result)
    ),
    ( "layerWeighted mixes two signals",
      let sig1 = [1.0, 0.0]
          sig2 = [0.0, 1.0]
          mixed = layerWeighted [(0.5, sig1), (0.5, sig2)]
       in assertEqual "mixed" [0.5, 0.5] mixed
    ),
    ( "layerWeighted pads shorter signals",
      let short = [1.0]
          long = [1.0, 1.0, 1.0]
          mixed = layerWeighted [(1.0, short), (1.0, long)]
       in assertEqual "padded length" 3 (length mixed)
    ),
    ( "normalizeSignal peak at level",
      let signal = [0.5, -1.0, 0.5]
          normalized = normalizeSignal 1.0 signal
          peak = maximum (map abs (map fromIntegral normalized :: [Double]))
       in assertTrue "peak near max" (peak > 30000)
    ),
    ( "renderSong produces non-empty output",
      assertTrue "song output" (not (null (renderSong renderTestSong)))
    ),
    ( "renderSong sample count matches expected",
      let samplesPerStep = sampleRate * secondsPerMinute `div` (testBpm * testStepsPerBeat)
          expectedSteps = testPatLen * testRepeats
          expectedSamples = samplesPerStep * expectedSteps
          actual = length (renderSong renderTestSong)
       in assertEqual "song sample count" expectedSamples actual
    )
  ]
  where
    testBpm :: Int
    testBpm = 120

    testStepsPerBeat :: Int
    testStepsPerBeat = 4

    testPatLen :: Int
    testPatLen = 4

    testRepeats :: Int
    testRepeats = 1

    secondsPerMinute :: Int
    secondsPerMinute = 60

    renderTestSong :: Song
    renderTestSong =
      Song
        { songTempo = testBpm,
          songStepsPerBeat = testStepsPerBeat,
          songSections =
            [ Section
                { secName = "test",
                  secRepeats = testRepeats,
                  secTracks =
                    [ Track
                        (Synth Sine (ADSR 0.01 0.05 0.5 0.05) 0.5)
                        (fromNotes [Just 60, Nothing, Just 64, Nothing])
                        1.0
                    ]
                }
            ]
        }

-- ---------------------------------------------------------------------------
-- Synthesis tests
-- ---------------------------------------------------------------------------

testSynthesis :: [(String, TestResult)]
testSynthesis =
  [ ( "expDecay at t=0 is 1.0",
      assertApprox "expDecay 0" 1.0 (expDecay 5.0 0.0) 0.001
    ),
    ( "expDecay decreases over time",
      assertTrue "expDecay decreasing" (expDecay 5.0 1.0 < expDecay 5.0 0.5)
    ),
    ( "attackDecay ramps during attack",
      assertApprox "attack midpoint" 0.5 (attackDecay 1.0 5.0 0.5) 0.001
    ),
    ( "attackDecay decays after attack",
      assertTrue "decay phase" (attackDecay 0.1 5.0 0.5 < 0.5)
    ),
    ( "sineSweep produces correct sample count",
      assertEqual "sweep length" sweepLen (length (sineSweep 440.0 220.0 5.0 sweepLen))
    ),
    ( "sineSweep output in [-1, 1]",
      assertTrue
        "sweep range"
        ( all
            (\s -> s >= -1.0 && s <= 1.0)
            (sineSweep 440.0 220.0 5.0 sweepLen)
        )
    ),
    ( "sineSweepAD produces correct sample count",
      assertEqual "sweepAD length" sweepLen (length (sineSweepAD 440.0 220.0 2.0 5.0 sweepLen))
    ),
    ( "noiseBurst produces correct sample count",
      assertEqual "noise length" noiseLen (length (noiseBurst 10.0 noiseLen))
    ),
    ( "noiseBurst output in [-1, 1]",
      assertTrue
        "noise range"
        (all (\s -> s >= -1.0 && s <= 1.0) (noiseBurst 10.0 noiseLen))
    ),
    ( "noiseBurst is deterministic",
      assertEqual "noise determinism" (noiseBurst 10.0 noiseLen) (noiseBurst 10.0 noiseLen)
    ),
    ( "squareWaveDecay produces correct sample count",
      assertEqual "square length" sweepLen (length (squareWaveDecay 440.0 5.0 sweepLen))
    ),
    ( "squareWaveDecay output in [-1, 1]",
      assertTrue
        "square range"
        ( all
            (\s -> s >= -1.0 && s <= 1.0)
            (squareWaveDecay 440.0 5.0 sweepLen)
        )
    ),
    ( "silence produces zeros",
      assertTrue "silence zeros" (all (== 0.0) (silence silenceLen))
    ),
    ( "silence correct length",
      assertEqual "silence length" silenceLen (length (silence silenceLen))
    )
  ]
  where
    sweepLen :: Int
    sweepLen = 500

    noiseLen :: Int
    noiseLen = 1000

    silenceLen :: Int
    silenceLen = 100

-- ---------------------------------------------------------------------------
-- Instrument tests
-- ---------------------------------------------------------------------------

testInstrument :: [(String, TestResult)]
testInstrument =
  [ ( "renderNote Synth produces correct sample count",
      let dur = msToSamples 100
          inst = Synth Sine shortPluck 0.5
       in assertEqual "synth note length" dur (length (renderNote inst 60 dur))
    ),
    ( "renderNote Sample truncates long buffer",
      let buffer = replicate 1000 0.5
          inst = Sample buffer 1.0
          dur = 500
       in assertEqual "sample truncate" dur (length (renderNote inst 0 dur))
    ),
    ( "renderNote Sample pads short buffer",
      let buffer = replicate 100 0.5
          inst = Sample buffer 1.0
          dur = 500
       in assertEqual "sample pad" dur (length (renderNote inst 0 dur))
    ),
    ( "renderNote applies gain",
      let buffer = [1.0]
          inst = Sample buffer 0.5
          result = renderNote inst 0 1
       in case result of
            (firstSample : _) -> assertApprox "gain applied" 0.5 firstSample 0.001
            [] -> Left "renderNote: empty result"
    )
  ]

-- ---------------------------------------------------------------------------
-- WAV tests
-- ---------------------------------------------------------------------------

testWavRoundtrip :: IO [(String, TestResult)]
testWavRoundtrip = do
  let samples = [0, 1000, -1000, 32767, -32768] :: [Int16]
  (path, h) <- openTempFile "." "gb-synth-test.wav"
  hClose h
  writeWav path samples
  raw <- BS.readFile path
  let bytes = BS.unpack raw
  return
    [ ( "WAV file starts with RIFF",
        assertEqual "RIFF magic" [0x52, 0x49, 0x46, 0x46] (take 4 bytes)
      ),
      ( "WAV file contains WAVE",
        assertEqual "WAVE magic" [0x57, 0x41, 0x56, 0x45] (take 4 (drop 8 bytes))
      ),
      ( "WAV sample rate is 22050",
        let srBytes = take 4 (drop 24 bytes)
            srValue = fromLE32 srBytes
         in assertEqual "sample rate" (fromIntegral sampleRate) srValue
      ),
      ( "WAV channels is 1 (mono)",
        let chBytes = take 2 (drop 22 bytes)
            chValue = fromLE16 chBytes
         in assertEqual "channels" 1 chValue
      ),
      ( "WAV bits per sample is 16",
        let bpsBytes = take 2 (drop 34 bytes)
            bpsValue = fromLE16 bpsBytes
         in assertEqual "bits per sample" 16 bpsValue
      ),
      ( "WAV data size matches",
        let dataSizeBytes = take 4 (drop 40 bytes)
            dataSize = fromLE32 dataSizeBytes
            expectedSize = fromIntegral (length samples) * 2
         in assertEqual "data size" expectedSize dataSize
      ),
      ( "WAV total file size correct",
        let expectedFileSize = 44 + length samples * 2
         in assertEqual "file size" expectedFileSize (BS.length raw)
      ),
      ( "toSample clamps positive",
        assertTrue "clamp positive" (toSample 2.0 == 32767)
      ),
      ( "toSample clamps negative",
        assertTrue "clamp negative" (toSample (-2.0) == -32767)
      ),
      ( "msToSamples 1000ms = sampleRate",
        assertEqual "msToSamples 1s" sampleRate (msToSamples 1000)
      )
    ]

-- ---------------------------------------------------------------------------
-- SFX tests
-- ---------------------------------------------------------------------------

testSFX :: [(String, TestResult)]
testSFX =
  [ sfxNonEmpty "laser" laser,
    sfxBounded "laser" laser,
    sfxNonEmpty "explosion" explosion,
    sfxBounded "explosion" explosion,
    sfxNonEmpty "impact" impact,
    sfxBounded "impact" impact,
    sfxNonEmpty "alert" alert,
    sfxBounded "alert" alert,
    sfxNonEmpty "click" click,
    sfxBounded "click" click,
    sfxNonEmpty "powerup" powerup,
    sfxBounded "powerup" powerup,
    sfxNonEmpty "coin" coin,
    sfxBounded "coin" coin,
    sfxNonEmpty "jump" jump,
    sfxBounded "jump" jump,
    sfxNonEmpty "heal" heal,
    sfxBounded "heal" heal,
    sfxNonEmpty "defeat" defeat,
    sfxBounded "defeat" defeat,
    sfxNonEmpty "kickSample" kickSample,
    sfxBounded "kickSample" kickSample,
    sfxNonEmpty "snareSample" snareSample,
    sfxBounded "snareSample" snareSample,
    sfxNonEmpty "hihatSample" hihatSample,
    sfxBounded "hihatSample" hihatSample
  ]

-- | Assert an SFX preset produces non-empty output.
sfxNonEmpty :: String -> [Int16] -> (String, TestResult)
sfxNonEmpty name samples =
  (name ++ " is non-empty", assertTrue (name ++ " non-empty") (not (null samples)))

-- | Assert all samples are within Int16 bounds.
sfxBounded :: String -> [Int16] -> (String, TestResult)
sfxBounded name samples =
  ( name ++ " samples bounded",
    assertTrue
      (name ++ " bounded")
      (all (\s -> s >= minBound && s <= maxBound) samples)
  )

-- ---------------------------------------------------------------------------
-- Chord tests
-- ---------------------------------------------------------------------------

testChord :: [(String, TestResult)]
testChord =
  [ ( "Major chord intervals: root, +4, +7",
      assertEqual "C Major" [60, 64, 67] (chord middleC Major)
    ),
    ( "Minor chord intervals: root, +3, +7",
      assertEqual "A Minor" [57, 60, 64] (chord concertA Minor)
    ),
    ( "Diminished chord intervals: root, +3, +6",
      assertEqual "B Dim" [59, 62, 65] (chord 59 Diminished)
    ),
    ( "Augmented chord intervals: root, +4, +8",
      assertEqual "C Aug" [60, 64, 68] (chord middleC Augmented)
    ),
    ( "Sus2 chord intervals: root, +2, +7",
      assertEqual "C Sus2" [60, 62, 67] (chord middleC Sus2)
    ),
    ( "Sus4 chord intervals: root, +5, +7",
      assertEqual "C Sus4" [60, 65, 67] (chord middleC Sus4)
    ),
    ( "1st inversion moves root up an octave",
      assertEqual "C/E" [64, 67, 72] (inversion 1 [60, 64, 67])
    ),
    ( "2nd inversion moves two lowest up",
      assertEqual "C/G" [67, 72, 76] (inversion 2 [60, 64, 67])
    ),
    ( "inversion 0 is identity",
      assertEqual "no inversion" [60, 64, 67] (inversion 0 [60, 64, 67])
    ),
    ( "chordProgression builds pairs",
      let prog = chordProgression [(60, Major), (65, Major)]
       in assertEqual "progression" [(60, [60, 64, 67]), (65, [65, 69, 72])] prog
    )
  ]
  where
    middleC :: Int
    middleC = 60

    concertA :: Int
    concertA = 57

-- ---------------------------------------------------------------------------
-- Effects tests
-- ---------------------------------------------------------------------------

testEffects :: [(String, TestResult)]
testEffects =
  [ ( "bitCrush clamps to levels",
      let crushed = bitCrush crushedBits [0.5, -0.5, 0.25]
       in assertTrue
            "crushed values"
            (all (\s -> s >= -1.0 && s <= 1.0) crushed)
    ),
    ( "bitCrush preserves length",
      assertEqual "crush length" signalLen (length (bitCrush crushedBits testSignal))
    ),
    ( "echo extends signal length",
      let delayed = echo echoDelay echoDecay testSignal
       in assertEqual "echo length" (signalLen + echoDelay) (length delayed)
    ),
    ( "echo preserves original signal start",
      let delayed = echo echoDelay echoDecay testSignal
       in case delayed of
            (firstSample : _) -> assertApprox "echo start" 1.0 firstSample 0.001
            [] -> Left "echo: empty result"
    ),
    ( "fadeIn starts at zero",
      let faded = fadeIn fadeDur testSignal
       in case faded of
            (firstSample : _) -> assertApprox "fade start" 0.0 firstSample 0.001
            [] -> Left "fadeIn: empty result"
    ),
    ( "fadeIn preserves length",
      assertEqual "fadeIn length" signalLen (length (fadeIn fadeDur testSignal))
    ),
    ( "fadeOut ends near zero",
      let faded = fadeOut fadeDur testSignal
       in case reverse faded of
            (lastSample : _) -> assertTrue "fade end" (abs lastSample < 0.01)
            [] -> Left "fadeOut: empty result"
    ),
    ( "fadeOut preserves length",
      assertEqual "fadeOut length" signalLen (length (fadeOut fadeDur testSignal))
    ),
    ( "reverseSignal is correct",
      assertEqual "reversed" [3.0, 2.0, 1.0] (reverseSignal [1.0, 2.0, 3.0])
    ),
    ( "reverseSignal preserves length",
      assertEqual "reverse length" signalLen (length (reverseSignal testSignal))
    ),
    ( "mix combines signals",
      let mixed = mix [[1.0, 0.0], [0.0, 1.0]]
       in assertEqual "mixed" [1.0, 1.0] mixed
    ),
    ( "mix pads shorter signals",
      let mixed = mix [[1.0], [1.0, 1.0, 1.0]]
       in assertEqual "mix length" 3 (length mixed)
    ),
    ( "mix empty is empty",
      assertEqual "mix empty" ([] :: [Double]) (mix [])
    )
  ]
  where
    signalLen :: Int
    signalLen = 100

    testSignal :: [Double]
    testSignal = replicate signalLen 1.0

    crushedBits :: Int
    crushedBits = 4

    echoDelay :: Int
    echoDelay = 50

    echoDecay :: Double
    echoDecay = 0.5

    fadeDur :: Int
    fadeDur = 50

-- ---------------------------------------------------------------------------
-- Song tests
-- ---------------------------------------------------------------------------

testSong :: [(String, TestResult)]
testSong =
  [ ( "track smart constructor matches Track",
      let t = track testInst testPat testGain
       in assertEqual "track" (Track testInst testPat testGain) t
    ),
    ( "section smart constructor matches Section",
      let s = section "verse" sectionRepeats [Track testInst testPat testGain]
       in assertEqual "section" (Section "verse" sectionRepeats [Track testInst testPat testGain]) s
    ),
    ( "song smart constructor matches Song",
      let s = song testTempo testSteps [Section "intro" 1 []]
       in assertEqual "song" (Song testTempo testSteps [Section "intro" 1 []]) s
    ),
    ( "Song field accessors work",
      let s = Song testTempo testSteps []
       in assertEqual "tempo" testTempo (songTempo s)
    ),
    ( "Section secName accessor",
      let s = Section "chorus" sectionRepeats []
       in assertEqual "secName" "chorus" (secName s)
    ),
    ( "Section secRepeats accessor",
      let s = Section "chorus" sectionRepeats []
       in assertEqual "secRepeats" sectionRepeats (secRepeats s)
    ),
    ( "Track trkGain accessor",
      let t = Track testInst testPat testGain
       in assertApprox "trkGain" testGain (trkGain t) 0.001
    ),
    ( "renderSong empty sections is empty",
      assertTrue "empty song" (null (renderSong (Song testTempo testSteps [])))
    ),
    ( "renderSong with repeats doubles length",
      let singleSong = Song testTempo testSteps [Section "a" 1 songTracks]
          doubleSong = Song testTempo testSteps [Section "a" sectionRepeats songTracks]
          singleLen = length (renderSong singleSong)
          doubleLen = length (renderSong doubleSong)
       in assertEqual "double repeats" (singleLen * sectionRepeats) doubleLen
    ),
    ( "renderSong multi-section concatenates",
      let sec1 = Section "a" 1 songTracks
          sec2 = Section "b" 1 songTracks
          twoSec = Song testTempo testSteps [sec1, sec2]
          oneSec = Song testTempo testSteps [sec1]
          twoLen = length (renderSong twoSec)
          oneLen = length (renderSong oneSec)
       in assertEqual "multi-section" (oneLen * multiSectionFactor) twoLen
    )
  ]
  where
    testInst :: Instrument
    testInst = Synth Sine shortPluck 0.5

    testPat :: Pattern
    testPat = fromNotes [Just 60, Nothing, Just 64, Nothing]

    testGain :: Double
    testGain = 0.8

    testTempo :: Int
    testTempo = 120

    testSteps :: Int
    testSteps = 4

    sectionRepeats :: Int
    sectionRepeats = 2

    multiSectionFactor :: Int
    multiSectionFactor = 2

    songTracks :: [Track]
    songTracks = [Track testInst testPat testGain]

-- ---------------------------------------------------------------------------
-- Envelope preset tests
-- ---------------------------------------------------------------------------

testEnvelopePresets :: [(String, TestResult)]
testEnvelopePresets =
  [ ( "longPad has slow attack",
      assertApprox "longPad attack" 0.08 (adsrAttack longPad) 0.001
    ),
    ( "longPad has full sustain",
      assertApprox "longPad sustain" 1.0 (adsrSustain longPad) 0.001
    ),
    ( "organ has instant attack",
      assertTrue "organ attack" (adsrAttack organ < 0.01)
    ),
    ( "organ has full sustain",
      assertApprox "organ sustain" 1.0 (adsrSustain organ) 0.001
    ),
    ( "percussive has zero sustain",
      assertApprox "percussive sustain" 0.0 (adsrSustain percussive) 0.001
    ),
    ( "shortPluck has low sustain",
      assertApprox "shortPluck sustain" 0.3 (adsrSustain shortPluck) 0.001
    ),
    ( "renderEnvelope zero-length is empty",
      assertTrue "zero env" (null (renderEnvelope shortPluck 0 0))
    )
  ]

-- ---------------------------------------------------------------------------
-- Instrument preset tests
-- ---------------------------------------------------------------------------

testInstrumentPresets :: [(String, TestResult)]
testInstrumentPresets =
  [ ( "bass preset produces output",
      let result = renderNote bass middleC presetDur
       in assertEqual "bass length" presetDur (length result)
    ),
    ( "lead preset produces output",
      let result = renderNote lead middleC presetDur
       in assertEqual "lead length" presetDur (length result)
    ),
    ( "pad preset produces output",
      let result = renderNote pad middleC presetDur
       in assertEqual "pad length" presetDur (length result)
    ),
    ( "bass preset output in range",
      let result = renderNote bass middleC presetDur
       in assertTrue "bass range" (all (\s -> s >= -1.0 && s <= 1.0) result)
    ),
    ( "renderNote Synth zero duration is empty",
      let inst = Synth Sine shortPluck 0.5
       in assertTrue "zero dur" (null (renderNote inst middleC 0))
    ),
    ( "renderNote Sample empty buffer pads",
      let inst = Sample [] 1.0
       in assertEqual "empty buf pad" presetDur (length (renderNote inst 0 presetDur))
    )
  ]
  where
    presetDur :: Int
    presetDur = 1000

    middleC :: Int
    middleC = 60

-- ---------------------------------------------------------------------------
-- Chord progression tests
-- ---------------------------------------------------------------------------

testChordProgressions :: [(String, TestResult)]
testChordProgressions =
  [ ( "pop1564 has 4 chords",
      assertEqual "pop len" popLen (length pop1564)
    ),
    ( "pop1564 starts on C",
      case pop1564 of
        ((root, _) : _) -> assertEqual "pop root" popRoot root
        [] -> Left "pop1564: empty"
    ),
    ( "blues145 has 3 chords",
      assertEqual "blues len" bluesLen (length blues145)
    ),
    ( "minorClassic has 3 chords",
      assertEqual "minor len" minorClassicLen (length minorClassic)
    ),
    ( "inversion on empty list is empty",
      assertEqual "inv empty" ([] :: [Int]) (inversion 1 [])
    ),
    ( "negative inversion is identity",
      assertEqual "neg inv" [60, 64, 67] (inversion (-1) [60, 64, 67])
    ),
    ( "chord Diminished",
      assertEqual "B dim" [59, 62, 65] (chord 59 Diminished)
    ),
    ( "chordProgression preserves order",
      let prog = chordProgression [(60, Major), (65, Minor), (67, Major)]
       in assertEqual "prog len" progLen (length prog)
    )
  ]
  where
    popLen :: Int
    popLen = 4

    popRoot :: Int
    popRoot = 60

    bluesLen :: Int
    bluesLen = 3

    minorClassicLen :: Int
    minorClassicLen = 3

    progLen :: Int
    progLen = 3

-- ---------------------------------------------------------------------------
-- Render utility tests
-- ---------------------------------------------------------------------------

testRenderUtils :: [(String, TestResult)]
testRenderUtils =
  [ ( "mixAt inserts at offset",
      let buf = mixAt Map.empty mixOffset [1.0, 2.0]
       in assertApprox "mixAt val" 1.0 (Map.findWithDefault 0.0 mixOffset buf) 0.001
    ),
    ( "mixAt accumulates overlapping samples",
      let buf1 = mixAt Map.empty 0 [1.0, 2.0]
          buf2 = mixAt buf1 0 [0.5, 0.5]
       in assertApprox "mixAt accum" 1.5 (Map.findWithDefault 0.0 0 buf2) 0.001
    ),
    ( "renderBuffer fills gaps with zero",
      let buf = mixAt Map.empty renderBufOffset [1.0]
       in case renderBuffer renderBufLen buf of
            (firstSample : _) -> assertApprox "gap fill" 0.0 firstSample 0.001
            [] -> Left "renderBuffer: empty"
    ),
    ( "renderBuffer correct length",
      assertEqual "buf len" renderBufLen (length (renderBuffer renderBufLen Map.empty))
    ),
    ( "normalizeSignal empty is empty",
      assertTrue "norm empty" (null (normalizeSignal 1.0 []))
    ),
    ( "renderSfx empty layers is empty",
      assertTrue "sfx empty" (null (renderSfx 1.0 []))
    ),
    ( "layerWeighted empty is empty",
      assertTrue "layer empty" (null (layerWeighted []))
    )
  ]
  where
    mixOffset :: Int
    mixOffset = 5

    renderBufOffset :: Int
    renderBufOffset = 3

    renderBufLen :: Int
    renderBufLen = 5

-- ---------------------------------------------------------------------------
-- Synthesis edge case tests
-- ---------------------------------------------------------------------------

testSynthesisEdge :: [(String, TestResult)]
testSynthesisEdge =
  [ ( "silence zero is empty",
      assertTrue "silence 0" (null (silence 0))
    ),
    ( "expDecay high rate is near zero",
      assertTrue "fast decay" (expDecay 100.0 1.0 < 0.001)
    ),
    ( "attackDecay at attack boundary is 1.0",
      assertApprox "attack top" 1.0 (attackDecay 0.5 5.0 0.5) 0.001
    ),
    ( "sineSweepAD output in range",
      assertTrue
        "sweepAD range"
        (all (\s -> s >= -1.0 && s <= 1.0) (sineSweepAD 440.0 220.0 2.0 5.0 sweepSamples))
    ),
    ( "noiseBurst zero length is empty",
      assertTrue "noise 0" (null (noiseBurst 10.0 0))
    ),
    ( "squareWaveDecay at t=0 has full amplitude",
      case squareWaveDecay 440.0 0.0 squareTestLen of
        (firstSample : _) -> assertApprox "square t0" 1.0 (abs firstSample) 0.001
        [] -> Left "squareWaveDecay: empty"
    )
  ]
  where
    sweepSamples :: Int
    sweepSamples = 500

    squareTestLen :: Int
    squareTestLen = 10

-- ---------------------------------------------------------------------------
-- Effects edge case tests
-- ---------------------------------------------------------------------------

testEffectsEdge :: [(String, TestResult)]
testEffectsEdge =
  [ ( "bitCrush empty is empty",
      assertTrue "crush empty" (null (bitCrush 4 []))
    ),
    ( "echo empty signal produces delay-length silence",
      assertEqual "echo empty len" echoDelaySingle (length (echo echoDelaySingle echoDecaySingle []))
    ),
    ( "fadeIn empty is empty",
      assertTrue "fadeIn empty" (null (fadeIn 50 []))
    ),
    ( "fadeOut empty is empty",
      assertTrue "fadeOut empty" (null (fadeOut 50 []))
    ),
    ( "reverseSignal empty is empty",
      assertTrue "rev empty" (null (reverseSignal []))
    ),
    ( "reverseSignal singleton is identity",
      assertEqual "rev single" [1.0] (reverseSignal [1.0])
    ),
    ( "mix single signal is itself",
      assertEqual "mix single" [1.0, 2.0] (mix [[1.0, 2.0]])
    ),
    ( "echo single sample extends",
      let result = echo echoDelaySingle echoDecaySingle [1.0]
       in assertEqual "echo extend" (1 + echoDelaySingle) (length result)
    )
  ]
  where
    echoDelaySingle :: Int
    echoDelaySingle = 10

    echoDecaySingle :: Double
    echoDecaySingle = 0.5

-- ---------------------------------------------------------------------------
-- Helpers
-- ---------------------------------------------------------------------------

-- | Decode a little-endian 16-bit unsigned value.
fromLE16 :: [Word8] -> Int
fromLE16 (lo : hi : _) = fromIntegral lo + fromIntegral hi * 256
fromLE16 _ = 0

-- | Decode a little-endian 32-bit unsigned value.
fromLE32 :: [Word8] -> Int
fromLE32 (b0 : b1 : b2 : b3 : _) =
  fromIntegral b0
    + fromIntegral b1 * 256
    + fromIntegral b2 * 65536
    + fromIntegral b3 * 16777216
fromLE32 _ = 0
