<div align="center">
<h1>gb-synth</h1>
<p><strong>Procedural Music & Sound Effect Synthesis</strong></p>
<p>Pure Haskell — no audio files, no samples, no DAW. Just math.</p>
<p><a href="#overview">Overview</a> · <a href="#architecture">Architecture</a> · <a href="#usage">Usage</a> · <a href="#api">API</a> · <a href="#example">Example</a></p>
<p>

[![CI](https://github.com/Gondola-Bros-Entertainment/gb-synth/actions/workflows/ci.yml/badge.svg)](https://github.com/Gondola-Bros-Entertainment/gb-synth/actions/workflows/ci.yml)
[![Hackage](https://img.shields.io/hackage/v/gb-synth.svg)](https://hackage.haskell.org/package/gb-synth)
![Haskell](https://img.shields.io/badge/haskell-GHC%209.6-purple)

</p>
</div>

---

## Overview

gb-synth is a synthesis engine with a tracker-style song DSL for generating retro game music and sound effects. Define songs declaratively — chords, patterns, sections, instruments — and render to 16-bit mono PCM WAV at 22050 Hz.

Companion to [gb-sprite](https://github.com/Gondola-Bros-Entertainment/gb-sprite) (procedural 2D graphics).

**Features:**
- 5 waveforms — sine, square, triangle, sawtooth, noise
- ADSR envelopes with 4 presets (percussive, shortPluck, longPad, organ)
- Tracker-style step patterns with note sustain across rests
- Structured songs with sections (intro/verse/chorus/outro) and repeats
- Pre-rendered sample instruments for drums and percussion
- 16-bit mono PCM WAV output

---

## Architecture

```
src/GBSynth/
├── Oscillator.hs   Sine, square, triangle, sawtooth, noise
├── Envelope.hs     ADSR (attack/decay/sustain/release)
├── Instrument.hs   Synth (oscillator+ADSR) or Sample (pre-rendered buffer)
├── Pattern.hs      Tracker-style step grid (MOD/XM/IT inspired)
├── Song.hs         Sections + arrangement (intro/verse/chorus/outro)
├── Synthesis.hs    Reusable SFX building blocks (sweeps, bursts, decays)
├── Render.hs       Song → [Int16] pipeline
└── WAV.hs          16-bit mono PCM writer (22050 Hz)
```

### Pipeline

```
Song → Sections → Tracks → Patterns → Notes → Oscillator + Envelope → SampleMap → Mix → Normalize → [Int16] → WAV
```

1. Each `Section` contains parallel `Track`s
2. Each `Track` pairs an `Instrument` with a `Pattern`
3. `Pattern` steps are rendered: `NoteOn` triggers `renderNote`, `Rest` sustains the previous note, `NoteOff` silences
4. Track audio is mixed into a sparse `SampleMap` for efficient random-access
5. Tracks are layered with per-track gain
6. Section is repeated `secRepeats` times
7. All sections concatenated, normalized, converted to `[Int16]`

---

## Usage

### As a dependency

Add to your `.cabal` file:

```cabal
build-depends: gb-synth >= 0.1
```

### Generating WAVs

```haskell
import GBSynth.Render (renderSong)
import GBSynth.WAV (writeWav)

main :: IO ()
main = writeWav "music.wav" (renderSong mySong)
```

---

## API

### Oscillator

```haskell
data Waveform = Sine | Square | Triangle | Sawtooth | Noise

oscillate :: Waveform -> Double -> Int -> [Double]  -- waveform, freq Hz, duration samples
noteFreq  :: Int -> Double                           -- MIDI note → Hz (A4 = 440)
```

### Envelope

```haskell
data ADSR = ADSR
  { adsrAttack  :: !Double   -- seconds, linear 0→1
  , adsrDecay   :: !Double   -- seconds, 1→sustain
  , adsrSustain :: !Double   -- hold level (0.0–1.0)
  , adsrRelease :: !Double   -- seconds, sustain→0
  }

renderEnvelope :: ADSR -> Int -> Int -> [Double]  -- noteOn samples, total samples → curve

-- Presets
percussive :: ADSR   -- drums, clicks
shortPluck :: ADSR   -- bass, arpeggios
longPad    :: ADSR   -- pads, ambient
organ      :: ADSR   -- sustained tones
```

### Instrument

```haskell
data Instrument
  = Synth !Waveform !ADSR !Double        -- oscillator + envelope + gain
  | Sample ![Double] !Double              -- pre-rendered buffer + gain

renderNote :: Instrument -> Int -> Int -> [Double]  -- instrument, MIDI note, duration

-- Presets
bass :: Instrument   -- square + shortPluck
lead :: Instrument   -- sine + shortPluck
pad  :: Instrument   -- sine + longPad
```

### Pattern

```haskell
data NoteEvent = NoteOn !Int !Double | NoteOff | Rest

data Pattern = Pattern { patSteps :: !Int, patEvents :: ![NoteEvent] }

fromNotes :: [Maybe Int] -> Pattern   -- Nothing = rest, Just n = note on
fromHits  :: Int -> [Int] -> Pattern  -- percussion: total steps + hit positions
```

### Song

```haskell
data Track   = Track   { trkInstrument :: !Instrument, trkPattern :: !Pattern, trkGain :: !Double }
data Section = Section { secName :: !String, secRepeats :: !Int, secTracks :: ![Track] }
data Song    = Song    { songTempo :: !Int, songStepsPerBeat :: !Int, songSections :: ![Section] }
```

### Render

```haskell
renderSong    :: Song -> [Int16]                          -- full song pipeline
renderSfx     :: Double -> [(Double, [Double])] -> [Int16] -- layer + normalize SFX
layerWeighted :: [(Double, [Double])] -> [Double]          -- mix with per-signal gain
```

### Synthesis

Reusable SFX building blocks — combine these to create any sound effect:

```haskell
sineSweep      :: Double -> Double -> Double -> Int -> [Double]         -- freq sweep
sineSweepAD    :: Double -> Double -> Double -> Double -> Int -> [Double] -- sweep + decay
noiseBurst     :: Double -> Int -> [Double]                             -- noise with decay
squareWaveDecay :: Double -> Double -> Int -> [Double]                  -- square + decay
expDecay       :: Double -> Double -> Double                            -- exponential decay
attackDecay    :: Double -> Double -> Double -> Double                  -- attack-decay curve
silence        :: Int -> [Double]                                       -- zero-filled gap
```

### WAV

```haskell
sampleRate  :: Int                    -- 22050
msToSamples :: Int -> Int             -- milliseconds → samples
toSample    :: Double -> Int16        -- [-1,1] → Int16
writeWav    :: FilePath -> [Int16] -> IO ()
```

---

## Example

A complete song with intro, verse, and chorus:

```haskell
import GBSynth.Envelope (ADSR (..), shortPluck, longPad)
import GBSynth.Instrument (Instrument (..), bass, lead, pad)
import GBSynth.Oscillator (Waveform (..))
import GBSynth.Pattern (fromNotes, fromHits)
import GBSynth.Render (renderSong)
import GBSynth.Song (Section (..), Song (..), Track (..))
import GBSynth.WAV (writeWav)

main :: IO ()
main = writeWav "song.wav" (renderSong mySong)

mySong :: Song
mySong = Song
  { songTempo = 120
  , songStepsPerBeat = 4        -- 16th note grid
  , songSections = [intro, verse, chorus]
  }

-- Am - F - C - G chord progression
chords :: [(Int, [Int])]
chords =
  [ (57, [57, 60, 64])   -- Am
  , (53, [53, 57, 60])   -- F
  , (48, [48, 52, 55])   -- C
  , (55, [55, 59, 62])   -- G
  ]

-- Bass: root note sustained per chord (8 steps each)
bassPat :: Pattern
bassPat = fromNotes $ concatMap
  (\(root, _) -> Just root : replicate 7 Nothing)
  chords

-- Arpeggio: root, 3rd, 5th, 3rd
arpPat :: Pattern
arpPat = fromNotes $ concatMap
  (\(_, [n0, n1, n2]) ->
    [Just n0, Nothing, Just n1, Nothing,
     Just n2, Nothing, Just n1, Nothing])
  chords

-- Kick on beats 1 and 3
kickPat :: Pattern
kickPat = fromHits 32 [0, 8, 16, 24]

intro :: Section
intro = Section "intro" 2
  [ Track bass bassPat 0.35 ]

verse :: Section
verse = Section "verse" 4
  [ Track bass bassPat 0.35
  , Track lead arpPat 0.30
  ]

chorus :: Section
chorus = Section "chorus" 4
  [ Track bass bassPat 0.35
  , Track lead arpPat 0.35
  , Track pad  bassPat 0.20
  , Track (Sample kickDrum 1.0) kickPat 0.30
  ]

-- Pre-rendered kick drum sample
kickDrum :: [Double]
kickDrum = -- sine sweep 150→40 Hz with noise transient
  ...
```

---

## License

MIT

---

<p align="center">
  <sub>MIT License · <a href="https://github.com/Gondola-Bros-Entertainment">Gondola Bros Entertainment</a></sub>
</p>
