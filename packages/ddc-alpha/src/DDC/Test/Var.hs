{-# OPTIONS -O0 #-}
-- This -O0 is needed to prevent bindings being lifted out of newVarIO. 
-- Don't remove it or you'll break the unit tests.

-- | This module is NOT to be used DDC proper -- for unit testing only.
--   It uses `unsafePerformIO` under the covers to generate fresh variables.
--
--   In the real compiler the renamer generates the uniqueIds, but for unit testing we
--   want the uniqueIds to appear magically without running the renamer.
--   The renaming is faked by using a top level fresh name generator, but this working
--   depends on the name creation functions not being inlined, and bindings not being
--   lifted out of them. It's a brutal hack, but that's ok in the test framework.
--
--   Don't import this code into DDC proper as the uniqueIds you get depend on the order
--   in which the variables are accessed -- which makes them different depending
--   on how the code is compiled and what debugging dumps you ask for. GHC has exactly
--   this issue, because it uses filthy uniqueId generation throughout -- a mistake
--   we don't want to repeat.
-- 
module DDC.Test.Var
	( module DDC.Var
	, newVarIO, newVar
	, varV, varT, varR, varE, varC
	, vA1, vA2, vA3, vA4, a1, a2, a3, a4
	, vR1, vR2, vR3, vR4, r1, r2, r3, r4
	, vE1, vE2, vE3, vE4, e1, e2, e3, e4
	, vC1, vC2, vC3, vC4, c1, c2, c3, c4 )
where
import System.IO.Unsafe
import Data.IORef
import DDC.Type
import DDC.Var


-- | Filthy uniquiId generator for fresh variables.
filthyGlobalInt :: IORef Int
{-# NOINLINE filthyGlobalInt #-}
filthyGlobalInt = unsafePerformIO $ newIORef 0


-- | Filthily generate a fresh variable.
newVarIO :: NameSpace -> IO Var
{-# NOINLINE newVarIO #-}
newVarIO space 
 = do	ix	<- readIORef filthyGlobalInt
	writeIORef filthyGlobalInt (ix + 1)
	return	$ (varWithName $ [charPrefixOfSpace space] ++ show ix)
		{ varNameSpace	= space
		, varId		= VarId "GEN" ix }


-- | Filtily generate fresh variable.
newVar :: NameSpace -> Var
{-# NOINLINE newVar #-}
newVar space
 	= unsafePerformIO $ newVarIO space


-- Creation of variables in specific namespaces.
varV :: String -> Var
{-# NOINLINE varV #-}
varV str	= (newVar NameValue)  { varName = str }

varT :: String -> Var
{-# NOINLINE varT #-}
varT str	= (newVar NameType)   { varName = str }

varR :: String -> Var
{-# NOINLINE varR #-}
varR str	= (newVar NameRegion) { varName = str }

varE :: String -> Var
{-# NOINLINE varE #-}
varE str	= (newVar NameEffect) { varName = str }

varC :: String -> Var
{-# NOINLINE varC #-}
varC str	= (newVar NameClosure) { varName = str }


-- Value type variables.
vA1	= varT "a1"
vA2	= varT "a2"
vA3	= varT "a3"
vA4	= varT "a4"

a1	= TVar kValue (UVar vA1)
a2	= TVar kValue (UVar vA2)
a3	= TVar kValue (UVar vA3)
a4	= TVar kValue (UVar vA4)


-- Region variables
vR1	= varR "r1"
vR2	= varR "r2"
vR3	= varR "r3"
vR4	= varR "r4"

r1	= TVar kRegion (UVar vR1)
r2	= TVar kRegion (UVar vR2)
r3	= TVar kRegion (UVar vR3)
r4	= TVar kRegion (UVar vR4)


-- Effect variables
vE1	= varE "e1"
vE2	= varE "e2"
vE3	= varE "e3"
vE4	= varE "e4"

e1	= TVar kEffect (UVar vE1)
e2	= TVar kEffect (UVar vE2)
e3	= TVar kEffect (UVar vE3)
e4	= TVar kEffect (UVar vE4)


-- Closure variables.
vC1	= varC "c1"
vC2	= varC "c2"
vC3	= varC "c3"
vC4	= varC "c4"

c1	= TVar kClosure (UVar vC1)
c2	= TVar kClosure (UVar vC2)
c3	= TVar kClosure (UVar vC3)
c4	= TVar kClosure (UVar vC4)
