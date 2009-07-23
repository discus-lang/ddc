
module Base 
	( Env
	, GenM
	, GenS(..), genStateZero
	, evalGen
	, burn
	, checkFuel
	, withFuel
	, eval
	, runN
	, chooseM
	, genInt
	, genBool)
where

import Bits

import Type.Exp

import Shared.Pretty
import Shared.Var		(NameSpace(..))
import qualified Shared.Var	as Var

import Util

import System.Random


----------------------------------------------------------------------------------------------------
type Env	= [(Type, Var)]

-- Generator Monad ---------------------------------------------------------------------------------
-- TODO: add deterministic support
type GenM a 	= StateT GenS IO a

data GenS
	= GenS 
	{ stateVarGen	:: Int 
	, stateFuel	:: Int }

genStateZero
	= GenS
	{ stateVarGen	= 0 
	, stateFuel	= 0 }

evalGen f = evalStateT f genStateZero

-- | burn some fuel
burn n		= modify $ \s -> s { stateFuel = stateFuel s - n }

-- | check how much fuel is left
checkFuel :: GenM Int
checkFuel 	= gets stateFuel


withFuel :: Int -> GenM a -> GenM a
withFuel fuel f
 = do	oldFuel	<- gets stateFuel
 	modify $ \s -> s { stateFuel = fuel }
	x	<- f
	modify $ \s -> s { stateFuel = oldFuel }
	return x

-- | Eval a generator and print the result
eval :: Pretty a PMode => GenM a -> IO ()
eval f 
 = do	x	<- evalGen f
 	putStr $ pprStrPlain x
	putStr $ "\n\n"


-- | Eval and print this generator multiple times
runN 	:: Pretty a PMode
	=> Int -> GenM a -> IO ()
runN n f 
 = do	xs	<- replicateM n (evalGen f)
 	putStr $ catInt "\n" $ map pprStrPlain xs
	putStr $ "\n\n"

-- | Randomly choose one of these things
chooseM	:: [GenM a] -> GenM a
chooseM xx
 = do	n	<- genInt 0 (length xx - 1)
 	xx !! n
 
-- Primitive Generators ----------------------------------------------------------------------------
genInt :: Int -> Int -> GenM Int
genInt	min max	
	= liftIO $ getStdRandom (randomR (min, max))

genBool :: GenM Bool
genBool
 = do	r	<- genInt 0 1
 	case r of
		0 	-> return False
		1	-> return True
		
