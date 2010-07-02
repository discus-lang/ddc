-- This module is a minimal replacement for a module in the code base of the
-- GHC compiler. This is needed to allow the use of David Terei's LLVM output
-- code that is part of GHC.

module Llvm.GhcReplace.FastString where

type FastString = String

unpackFS :: FastString -> String
unpackFS = id

lengthFS :: FastString -> Int
lengthFS = length

type LitString = String

mkFastString :: String -> FastString
mkFastString s = s
