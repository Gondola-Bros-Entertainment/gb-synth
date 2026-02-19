-- | Reusable sound effect presets.
--
-- Pre-rendered @[Int16]@ samples built from the 'GBSynth.Synthesis'
-- primitives. Each preset is a pure value — no IO, no randomness,
-- deterministic output every time.
--
-- Drop these straight into your game's audio mixer.
module GBSynth.SFX
  ( -- * Combat
    laser,
    explosion,
    impact,

    -- * UI
    alert,
    click,
    powerup,
    coin,

    -- * Character
    jump,
    heal,
    defeat,

    -- * Drum samples
    kickSample,
    snareSample,
    hihatSample,
  )
where

import Data.Int (Int16)
import GBSynth.Render (renderSfx)
import GBSynth.Synthesis
  ( noiseBurst,
    silence,
    sineSweep,
    sineSweepAD,
    squareWaveDecay,
  )
import GBSynth.WAV (msToSamples)

-- ---------------------------------------------------------------------------
-- Combat sounds
-- ---------------------------------------------------------------------------

-- | Laser / fire / shoot — sharp high-frequency sweep down.
laser :: [Int16]
laser =
  renderSfx
    laserLevel
    [ (laserToneGain, sineSweep laserStartHz laserEndHz laserDecay laserDur),
      (laserClickGain, squareWaveDecay laserClickHz laserClickDecay laserClickDur)
    ]
  where
    laserLevel :: Double
    laserLevel = 0.9
    laserStartHz :: Double
    laserStartHz = 1800.0
    laserEndHz :: Double
    laserEndHz = 400.0
    laserDecay :: Double
    laserDecay = 8.0
    laserDur :: Int
    laserDur = msToSamples 120
    laserToneGain :: Double
    laserToneGain = 0.7
    laserClickGain :: Double
    laserClickGain = 0.3
    laserClickHz :: Double
    laserClickHz = 2200.0
    laserClickDecay :: Double
    laserClickDecay = 30.0
    laserClickDur :: Int
    laserClickDur = msToSamples 30

-- | Explosion / kill / death — noise burst with low rumble.
explosion :: [Int16]
explosion =
  renderSfx
    explosionLevel
    [ (explosionNoiseGain, noiseBurst explosionNoiseDecay explosionDur),
      (explosionRumbleGain, sineSweep explosionRumbleStart explosionRumbleEnd explosionRumbleDecay explosionDur)
    ]
  where
    explosionLevel :: Double
    explosionLevel = 0.9
    explosionDur :: Int
    explosionDur = msToSamples 250
    explosionNoiseGain :: Double
    explosionNoiseGain = 0.6
    explosionNoiseDecay :: Double
    explosionNoiseDecay = 4.0
    explosionRumbleGain :: Double
    explosionRumbleGain = 0.4
    explosionRumbleStart :: Double
    explosionRumbleStart = 150.0
    explosionRumbleEnd :: Double
    explosionRumbleEnd = 40.0
    explosionRumbleDecay :: Double
    explosionRumbleDecay = 3.0

-- | Impact / structural hit — low thud with sharp attack.
impact :: [Int16]
impact =
  renderSfx
    impactLevel
    [ (impactThudGain, sineSweepAD impactStartHz impactEndHz impactAttackMs impactDecay impactDur),
      (impactNoiseGain, noiseBurst impactNoiseDecay impactNoiseDur)
    ]
  where
    impactLevel :: Double
    impactLevel = 0.9
    impactDur :: Int
    impactDur = msToSamples 150
    impactThudGain :: Double
    impactThudGain = 0.7
    impactStartHz :: Double
    impactStartHz = 200.0
    impactEndHz :: Double
    impactEndHz = 60.0
    impactAttackMs :: Double
    impactAttackMs = 1.0
    impactDecay :: Double
    impactDecay = 6.0
    impactNoiseGain :: Double
    impactNoiseGain = 0.3
    impactNoiseDecay :: Double
    impactNoiseDecay = 15.0
    impactNoiseDur :: Int
    impactNoiseDur = msToSamples 60

-- ---------------------------------------------------------------------------
-- UI sounds
-- ---------------------------------------------------------------------------

-- | Alert / wave start / alarm — two alternating tones.
alert :: [Int16]
alert =
  renderSfx
    alertLevel
    [ (alertGain, sineSweep alertHiHz alertHiHz alertDecay alertToneDur ++ sineSweep alertLoHz alertLoHz alertDecay alertToneDur)
    ]
  where
    alertLevel :: Double
    alertLevel = 0.8
    alertGain :: Double
    alertGain = 1.0
    alertHiHz :: Double
    alertHiHz = 880.0
    alertLoHz :: Double
    alertLoHz = 660.0
    alertDecay :: Double
    alertDecay = 3.0
    alertToneDur :: Int
    alertToneDur = msToSamples 100

-- | Click / button press — very short square pulse.
click :: [Int16]
click =
  renderSfx
    clickLevel
    [ (clickGain, squareWaveDecay clickHz clickDecay clickDur)
    ]
  where
    clickLevel :: Double
    clickLevel = 0.7
    clickGain :: Double
    clickGain = 1.0
    clickHz :: Double
    clickHz = 1000.0
    clickDecay :: Double
    clickDecay = 40.0
    clickDur :: Int
    clickDur = msToSamples 30

-- | Powerup / upgrade / collect — rising sweep with shimmer.
powerup :: [Int16]
powerup =
  renderSfx
    powerupLevel
    [ (powerupSweepGain, sineSweep powerupStartHz powerupEndHz powerupDecay powerupDur),
      (powerupShimmerGain, sineSweep powerupShimmerStart powerupShimmerEnd powerupShimmerDecay powerupDur)
    ]
  where
    powerupLevel :: Double
    powerupLevel = 0.85
    powerupDur :: Int
    powerupDur = msToSamples 200
    powerupSweepGain :: Double
    powerupSweepGain = 0.6
    powerupStartHz :: Double
    powerupStartHz = 400.0
    powerupEndHz :: Double
    powerupEndHz = 1200.0
    powerupDecay :: Double
    powerupDecay = 2.0
    powerupShimmerGain :: Double
    powerupShimmerGain = 0.4
    powerupShimmerStart :: Double
    powerupShimmerStart = 800.0
    powerupShimmerEnd :: Double
    powerupShimmerEnd = 2400.0
    powerupShimmerDecay :: Double
    powerupShimmerDecay = 4.0

-- | Coin / pickup / reward — two quick ascending pips.
coin :: [Int16]
coin =
  renderSfx
    coinLevel
    [ (coinGain, sineSweep coinLoHz coinLoHz coinDecay coinPipDur ++ silence coinGap ++ sineSweep coinHiHz coinHiHz coinDecay coinPipDur)
    ]
  where
    coinLevel :: Double
    coinLevel = 0.8
    coinGain :: Double
    coinGain = 1.0
    coinLoHz :: Double
    coinLoHz = 987.0
    coinHiHz :: Double
    coinHiHz = 1319.0
    coinDecay :: Double
    coinDecay = 8.0
    coinPipDur :: Int
    coinPipDur = msToSamples 60
    coinGap :: Int
    coinGap = msToSamples 20

-- ---------------------------------------------------------------------------
-- Character sounds
-- ---------------------------------------------------------------------------

-- | Jump — quick upward frequency sweep.
jump :: [Int16]
jump =
  renderSfx
    jumpLevel
    [ (jumpGain, sineSweep jumpStartHz jumpEndHz jumpDecay jumpDur)
    ]
  where
    jumpLevel :: Double
    jumpLevel = 0.8
    jumpGain :: Double
    jumpGain = 1.0
    jumpStartHz :: Double
    jumpStartHz = 300.0
    jumpEndHz :: Double
    jumpEndHz = 900.0
    jumpDecay :: Double
    jumpDecay = 6.0
    jumpDur :: Int
    jumpDur = msToSamples 120

-- | Heal / restore — gentle rising shimmer.
heal :: [Int16]
heal =
  renderSfx
    healLevel
    [ (healToneGain, sineSweep healStartHz healEndHz healDecay healDur),
      (healShimmerGain, sineSweep healShimmerStart healShimmerEnd healShimmerDecay healDur)
    ]
  where
    healLevel :: Double
    healLevel = 0.75
    healDur :: Int
    healDur = msToSamples 300
    healToneGain :: Double
    healToneGain = 0.5
    healStartHz :: Double
    healStartHz = 500.0
    healEndHz :: Double
    healEndHz = 800.0
    healDecay :: Double
    healDecay = 1.5
    healShimmerGain :: Double
    healShimmerGain = 0.5
    healShimmerStart :: Double
    healShimmerStart = 1000.0
    healShimmerEnd :: Double
    healShimmerEnd = 1600.0
    healShimmerDecay :: Double
    healShimmerDecay = 2.0

-- | Defeat / game over — descending tone with fading noise.
defeat :: [Int16]
defeat =
  renderSfx
    defeatLevel
    [ (defeatToneGain, sineSweep defeatStartHz defeatEndHz defeatDecay defeatDur),
      (defeatNoiseGain, noiseBurst defeatNoiseDecay defeatDur)
    ]
  where
    defeatLevel :: Double
    defeatLevel = 0.85
    defeatDur :: Int
    defeatDur = msToSamples 500
    defeatToneGain :: Double
    defeatToneGain = 0.7
    defeatStartHz :: Double
    defeatStartHz = 440.0
    defeatEndHz :: Double
    defeatEndHz = 110.0
    defeatDecay :: Double
    defeatDecay = 2.0
    defeatNoiseGain :: Double
    defeatNoiseGain = 0.3
    defeatNoiseDecay :: Double
    defeatNoiseDecay = 3.0

-- ---------------------------------------------------------------------------
-- Drum samples
-- ---------------------------------------------------------------------------

-- | Kick drum — low sine sweep with noise transient.
kickSample :: [Int16]
kickSample =
  renderSfx
    kickLevel
    [ (kickBodyGain, sineSweep kickStartHz kickEndHz kickDecay kickDur),
      (kickClickGain, noiseBurst kickClickDecay kickClickDur)
    ]
  where
    kickLevel :: Double
    kickLevel = 0.9
    kickDur :: Int
    kickDur = msToSamples 120
    kickBodyGain :: Double
    kickBodyGain = 0.8
    kickStartHz :: Double
    kickStartHz = 150.0
    kickEndHz :: Double
    kickEndHz = 40.0
    kickDecay :: Double
    kickDecay = 5.0
    kickClickGain :: Double
    kickClickGain = 0.2
    kickClickDecay :: Double
    kickClickDecay = 25.0
    kickClickDur :: Int
    kickClickDur = msToSamples 15

-- | Snare drum — noise burst with mid-frequency body.
snareSample :: [Int16]
snareSample =
  renderSfx
    snareLevel
    [ (snareNoiseGain, noiseBurst snareNoiseDecay snareDur),
      (snareBodyGain, sineSweep snareBodyHz snareBodyHz snareBodyDecay snareDur)
    ]
  where
    snareLevel :: Double
    snareLevel = 0.85
    snareDur :: Int
    snareDur = msToSamples 100
    snareNoiseGain :: Double
    snareNoiseGain = 0.6
    snareNoiseDecay :: Double
    snareNoiseDecay = 8.0
    snareBodyGain :: Double
    snareBodyGain = 0.4
    snareBodyHz :: Double
    snareBodyHz = 200.0
    snareBodyDecay :: Double
    snareBodyDecay = 10.0

-- | Hi-hat — short high-frequency noise burst.
hihatSample :: [Int16]
hihatSample =
  renderSfx
    hihatLevel
    [ (hihatGain, noiseBurst hihatDecay hihatDur)
    ]
  where
    hihatLevel :: Double
    hihatLevel = 0.7
    hihatGain :: Double
    hihatGain = 1.0
    hihatDecay :: Double
    hihatDecay = 20.0
    hihatDur :: Int
    hihatDur = msToSamples 50
