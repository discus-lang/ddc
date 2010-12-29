-- This module is a minimal replacement for a module in the code base of the
-- GHC compiler. This is needed to allow the use of David Terei's LLVM output
-- code that is part of GHC.

module Llvm.GhcReplace.Unique
	( Unique
	, newUnique
	, fakeUnique
	, pprUnique )
	where

import Control.Concurrent.MVar
import System.IO.Unsafe (unsafePerformIO)
import Llvm.GhcReplace.Pretty

-- | An abstract unique object.  Objects of type 'Unique' may be
-- compared for equality and ordering and hashed into 'Int'.
data Unique
	= UniqueNameInt String Integer
	| UniqueStr String
	deriving (Eq,Ord)

uniqSource :: MVar Integer
uniqSource = unsafePerformIO (newMVar 0)

-- | Creates a new object of type 'Unique'.  The value returned will
-- not compare equal to any other value of type 'Unique' returned by
-- previous calls to 'newUnique'.  There is no limit on the number of
-- times 'newUnique' may be called.
newUnique :: String -> IO Unique
newUnique name = do
   val <- takeMVar uniqSource
   let next = val+1
   putMVar uniqSource next
   return (UniqueNameInt name next)

-- | Create a fake 'Unique' object. The DDC Sea backend already generated
-- unique labels. This allows the LLVM backend to use the Sea labels.
fakeUnique :: String -> Unique
fakeUnique s = UniqueStr s

instance Show Unique where
    show (UniqueNameInt s i) = s ++ "." ++ show i
    show (UniqueStr s) = s

pprUnique :: Unique -> Doc
pprUnique u = show u

