# CLAUDE.md

## Rules

- **Only commit and push when explicitly instructed.** Never amend commits. Never add `Co-Authored-By` headers.
- Run `cabal build --ghc-options="-Werror"` before considering any change complete.
- **`-Wall -Wcompat` clean.** All code compiles without warnings.

## Haskell Style

- **Pure by default.** IO only in WAV.hs for file writing.
- **Strict by default.** Bang patterns on all data fields and accumulators.
- **Total functions only.** No `head`, `tail`, `!!`, `fromJust`, `read`.
- **Named constants.** No magic numbers.
- **No prime-mark variables.** Use descriptive names.
- Ormolu for formatting, HLint for linting.

## Context

Procedural music sequencer for GB games. Library only — each game defines its own songs and SFX in its own repo, depending on gb-synth via Hackage.
