# Changelog

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
