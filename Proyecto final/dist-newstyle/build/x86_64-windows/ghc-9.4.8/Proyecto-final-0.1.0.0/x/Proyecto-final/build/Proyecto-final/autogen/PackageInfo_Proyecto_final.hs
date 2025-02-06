{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -w #-}
module PackageInfo_Proyecto_final (
    name,
    version,
    synopsis,
    copyright,
    homepage,
  ) where

import Data.Version (Version(..))
import Prelude

name :: String
name = "Proyecto_final"
version :: Version
version = Version [0,1,0,0] []

synopsis :: String
synopsis = "Evaluador de expresiones matem\225ticas con interfaz gr\225fica"
copyright :: String
copyright = "2025, Tu Nombre"
homepage :: String
homepage = ""
