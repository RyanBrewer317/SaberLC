{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -w #-}
module PackageInfo_saberlc (
    name,
    version,
    synopsis,
    copyright,
    homepage,
  ) where

import Data.Version (Version(..))
import Prelude

name :: String
name = "saberlc"
version :: Version
version = Version [0,1,0,0] []

synopsis :: String
synopsis = "A simple lambda calculus compiler that targets SaberVM"
copyright :: String
copyright = ""
homepage :: String
homepage = "ryanbrewer.dev"
