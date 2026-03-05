# Changelog

## 0.2.1.0

### New Features

- **Pulse waveform with variable duty cycle.** `Pulse !Double` constructor on `Waveform`. Supports the four Game Boy duty cycles (0.125, 0.25, 0.5, 0.75). `Square` now delegates to `Pulse 0.5` — fully backwards compatible.
- **Seventh chords.** Four new `Quality` constructors: `Dominant7`, `Major7`, `Minor7`, `Diminished7`. All work with `chord`, `inversion`, and `chordProgression`.

### Bug Fixes

- Fix envelope discontinuity on fast notes. When note-on is shorter than attack+decay, the release now starts from the actual envelope level instead of jumping to the sustain level.
- Fix duplicated envelope logic in Instrument: `renderNote` now delegates to `Envelope.renderEnvelope` instead of reimplementing the ADSR curve.
- Fix hardcoded sample rate (22050) in Envelope — now uses `sampleRate` from WAV module.
- Fix remaining Envelope field docs: `adsrDecay` and `adsrRelease` described as "exponential" but implementation is linear.

### Documentation

- Update README: test count, GHC badge 9.6 → 9.8, Pulse and seventh chord API docs.

### Internal

- CI: add Haddock step before build (fixes stale `.hi` file coverage), upgrade GHC 9.6 → 9.8.
- Tests: clean up temporary WAV files after roundtrip test.
- 182 pure tests (up from 161).

## 0.2.0.2

- Change license from MIT to BSD-3-Clause.

### Documentation

- Fix Envelope module documentation (decay/release described as "exponential" but implementation is linear).
- Add gb-vector as ecosystem companion in README.

### Internal

- CI: cross-platform build matrix (Linux, macOS, Windows).

## 0.2.0.1

- Fix Haddock cross-references in Effects and Pattern modules.
- Fix README chord progression descriptions and missing Pattern import.
- Remove `-O2` from library and test ghc-options (consumers set their own optimization).

## 0.2.0.0

### New Modules

- **GBSynth.SFX**: 13 ready-to-use sound effect presets (laser, explosion, impact, alert, click, powerup, coin, jump, heal, defeat) plus pre-rendered drum samples (kickSample, snareSample, hihatSample). All built from synthesis primitives, no sample files.
- **GBSynth.Chord**: Programmatic chord construction. `Quality` type (Major, Minor, Diminished, Augmented, Sus2, Sus4), `chord` / `inversion` / `chordProgression` builders, and built-in progressions (pop1564, blues145, minorClassic).
- **GBSynth.Effects**: Post-processing effects — `bitCrush`, `echo`, `fadeIn`, `fadeOut`, `reverseSignal`, `mix`.

### Bug Fixes

- Eliminate partial functions: `tail` → `drop 1` in Oscillator and Synthesis, `maximum` / `foldl1` → safe `foldl'` with empty list guards in Render.

### Internal

- 109 pure tests (up from 60)
- Metadata: cabal-version 3.0, stability experimental, CHANGELOG.md

## 0.1.0.0

Initial release.

### Synthesis Engine
- Oscillators: sine, square, saw, triangle, noise, pulse with configurable duty cycle
- ADSR envelope generator with attack, decay, sustain, release stages
- Instrument definition with oscillator type, envelope, and harmonics
- Multi-channel pattern sequencer with note events and rests
- Song structure: sections, tempo, time signature, instrument assignment

### Rendering
- Pure PCM rendering pipeline: Song → sections → patterns → samples
- Multi-layer signal mixing with per-channel gain
- Signal normalization with configurable output level
- 16-bit mono PCM at 22050 Hz

### WAV Export
- Native WAV file writer (RIFF/WAVE format)
- 16-bit signed PCM encoding
- No external audio dependencies
