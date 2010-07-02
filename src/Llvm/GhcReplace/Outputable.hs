-- This module is a minimal replacement for a module in the code base of the
-- GHC compiler. This is needed to allow the use of David Terei's LLVM output
-- code that is part of GHC.

module Llvm.GhcReplace.Outputable where

import Llvm.GhcReplace.Pretty

type SDoc = PprStyle -> Doc

data PprStyle = PprCode CodeStyle

data CodeStyle = CStyle


withPprStyleDoc :: PprStyle -> SDoc -> Doc
withPprStyleDoc sty d = d sty

mkCodeStyle :: CodeStyle -> PprStyle
mkCodeStyle = PprCode
