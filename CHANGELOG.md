# Changelog

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

### SFX Presets
- 13 ready-to-use sound effects: laser, explosion, impact, alert, click, powerup, coin, jump, heal, defeat
- Pre-rendered drum samples: kickSample, snareSample, hihatSample
- All built from synthesis primitives, no sample files

### Chord Construction
- `Quality` type: Major, Minor, Diminished, Augmented, Sus2, Sus4
- `chord`: root MIDI note + quality → chord voicing
- `inversion`: 1st/2nd/nth chord inversions
- `chordProgression`: build progressions from root-quality pairs
- Built-in progressions: pop1564, blues145, minorClassic

### Effects
- `bitCrush`: reduce bit depth for lo-fi retro sound
- `echo`: simple delay line with decay
- `fadeIn` / `fadeOut`: linear amplitude fades
- `reverseSignal`: reverse audio data
- `mix`: sum multiple signals with equal gain

### Internal
- 109 pure tests: oscillator waveforms, envelope stages, pattern sequencing, rendering pipeline, SFX presets, chord intervals, effects processing
- All partial functions eliminated (safe folds, empty list guards)
