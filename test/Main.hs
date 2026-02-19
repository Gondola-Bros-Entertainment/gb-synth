-- | gb-synth test suite.
--
-- Hand-rolled assertions — first failure stops all. Same pattern as gbnet-hs.
module Main (main) where

import qualified Data.ByteString as BS
import Data.Int (Int16)
import Data.Word (Word8)
import GBSynth.Chord (Quality (..), chord, chordProgression, inversion)
import GBSynth.Effects (bitCrush, echo, fadeIn, fadeOut, mix, reverseSignal)
import GBSynth.Envelope (ADSR (..), percussive, renderEnvelope, shortPluck)
import GBSynth.Instrument (Instrument (..), renderNote)
import GBSynth.Oscillator (Waveform (..), noteFreq, oscillate)
import GBSynth.Pattern (NoteEvent (..), Pattern (..), fromHits, fromNotes, restPattern)
import GBSynth.Render (layerWeighted, normalizeSignal, renderSfx, renderSong)
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
import GBSynth.Song (Section (..), Song (..), Track (..))
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
import System.IO (hFlush, stdout)

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
        ++ testPattern
        ++ testRender
        ++ testSynthesis
        ++ testInstrument
        ++ wavTests
        ++ testSFX
        ++ testChord
        ++ testEffects
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
      assertApprox "env start" 0.0 (head env) 0.01
    ),
    ( "envelope reaches peak near 1.0",
      assertTrue "env peak" (peakVal > 0.9)
    ),
    ( "envelope sustain holds at sustain level",
      let sustainSample = env !! sustainIdx
       in assertApprox "env sustain" 0.3 sustainSample 0.05
    ),
    ( "envelope ends near 0 (release phase)",
      let lastSample = last env
       in assertTrue "env release" (lastSample < 0.1)
    ),
    ( "percussive envelope decays quickly",
      let percEnv = renderEnvelope percussive percNoteOn percTotal
          midpoint = percEnv !! (percTotal `div` 2)
       in assertTrue "percussive decay" (midpoint < 0.1)
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
      assertEqual "rest event" Rest (patEvents pat !! 1)
    ),
    ( "fromNotes Just becomes NoteOn",
      assertEqual "note event" (NoteOn 60 1.0) (head (patEvents pat))
    ),
    ( "fromHits correct step count",
      assertEqual "fromHits steps" hitPatLen (patSteps hitPat)
    ),
    ( "fromHits has NoteOn at hit positions",
      assertEqual "hit at 0" (NoteOn 0 1.0) (head (patEvents hitPat))
    ),
    ( "fromHits has NoteOff at non-hit positions",
      assertEqual "off at 1" NoteOff (patEvents hitPat !! 1)
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
      assertTrue "song output" (not (null (renderSong testSong)))
    ),
    ( "renderSong sample count matches expected",
      let samplesPerStep = sampleRate * secondsPerMinute `div` (testBpm * testStepsPerBeat)
          expectedSteps = testPatLen * testRepeats
          expectedSamples = samplesPerStep * expectedSteps
          actual = length (renderSong testSong)
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

    testSong :: Song
    testSong =
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
       in assertApprox "gain applied" 0.5 (head result) 0.001
    )
  ]

-- ---------------------------------------------------------------------------
-- WAV tests
-- ---------------------------------------------------------------------------

testWavRoundtrip :: IO [(String, TestResult)]
testWavRoundtrip = do
  let samples = [0, 1000, -1000, 32767, -32768] :: [Int16]
      path = "/tmp/gb-synth-test.wav"
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
       in assertApprox "echo start" 1.0 (head delayed) 0.001
    ),
    ( "fadeIn starts at zero",
      let faded = fadeIn fadeDur testSignal
       in assertApprox "fade start" 0.0 (head faded) 0.001
    ),
    ( "fadeIn preserves length",
      assertEqual "fadeIn length" signalLen (length (fadeIn fadeDur testSignal))
    ),
    ( "fadeOut ends near zero",
      let faded = fadeOut fadeDur testSignal
       in assertTrue "fade end" (abs (last faded) < 0.01)
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
