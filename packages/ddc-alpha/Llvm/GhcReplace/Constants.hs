-- This module is a minimal replacement for a module in the code base of the
-- GHC compiler. This is needed to allow the use of David Terei's LLVM output
-- code that is part of GHC.

{-# OPTIONS -cpp #-}

module Llvm.GhcReplace.Constants where

#include "ghcconfig.h"

wORD_SIZE :: Int
wORD_SIZE = SIZEOF_VOID_P


