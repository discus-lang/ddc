-- | Utils for generating things.
module Churn.Gen.Base
where

import Churn.Classify

import Source.Exp
import Shared.Pretty

import Control.Monad.State
import System.Random


-- Generation scheme
--	The search space for expressions is huge, and we sometimes want to only
--	consider certain features of the language.
--
data Scheme
	-- Include only these fragments of the langauge.
	= SchemeOnly [Fragment]


-- | When generating a particular expression, we try and burn all
--	available fuel, but no more.
type Fuel	= Int

-- | Random seed to determine which nodes are generated.
type Seed	= Int


-- | Monad to help with generating things.
data GenS
	= GenS
	{ stateFuel	:: Fuel
	, stateStdGen	:: StdGen
	, stateScheme	:: Scheme }

initGenS fuel
 	= GenS
	{ stateFuel	= fuel
	, stateStdGen	= mkStdGen 0
	, stateScheme	= SchemeOnly allNonErrorFragments }

type GenM = State GenS


-- | Run a generator deterministically.
runGenDet :: Seed -> Fuel -> GenM a -> a
runGenDet seed fuel fun
 = let	stdgen	= mkStdGen seed
   in	evalState 
		fun 
		(initGenS fuel) { stateStdGen = stdgen }


-- | Run a generator, seeding it from IO land.
runGen :: Fuel -> GenM a -> IO a
runGen fuel fun
 = do	stdgen	<- newStdGen
	return	$ evalState 
			fun
			(initGenS fuel) { stateStdGen = stdgen }

-- | Run a generator, and pretty print the result
runGen' fuel fun
 = do	x	<- runGen fuel fun
	putStr	$ pprStrPlain x ++ "\n"

-- | Get the available fuel.
getFuel :: GenM Int
getFuel	= gets stateFuel


-- | Burn some fuel.
burn :: Int -> GenM ()
burn i	= modify $ \s -> s { stateFuel = stateFuel s - i }


-- | Generate something within a certain range.
--	Only works for basic things that are instances of Random.
genRandomR :: Random a => (a, a) -> GenM a
genRandomR (lo, hi)
 = do	stdgen			<- gets stateStdGen
	let (x, stdgen')	= randomR (lo, hi) stdgen
	modify $ \s -> s { stateStdGen = stdgen' }
	return x


-- | Choose one of these things at random.
genChoose :: [a] -> GenM a
genChoose	xx
 = do	let len	= length xx
	n	<- genRandomR (0, len-1)
	return	$ xx !! n





