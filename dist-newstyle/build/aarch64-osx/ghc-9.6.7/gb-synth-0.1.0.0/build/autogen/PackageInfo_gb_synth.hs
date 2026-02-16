{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -w #-}
module PackageInfo_gb_synth (
    name,
    version,
    synopsis,
    copyright,
    homepage,
  ) where

import Data.Version (Version(..))
import Prelude

name :: String
name = "gb_synth"
version :: Version
version = Version [0,1,0,0] []

synopsis :: String
synopsis = "Procedural music sequencer for GB games"
copyright :: String
copyright = ""
homepage :: String
homepage = ""
