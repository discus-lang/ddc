-- | Utils for generating things.
module Churn.Gen.Base
where

import Churn.Classify
import Churn.Bits

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
	{ stateStdGen	:: StdGen
	, stateScheme	:: Scheme }

initGenS
 	= GenS
	{ stateStdGen	= mkStdGen 0
	, stateScheme	= SchemeOnly allNonErrorFragments }

type GenM = State GenS


-- | Run a generator deterministically.
runGenDet :: Seed -> GenM a -> a
runGenDet seed fun
 = let	stdgen	= mkStdGen seed
   in	evalState 
		fun 
		initGenS { stateStdGen = stdgen }


-- | Run a generator, seeding it from IO land.
runGen :: GenM a -> IO a
runGen fun
 = do	stdgen	<- newStdGen
	return	$ evalState 
			fun
			initGenS { stateStdGen = stdgen }

-- | Run a generator, and pretty print the result
runGen' fun
 = do	x	<- runGen fun
	putStr	$ pprStrPlain x ++ "\n"

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

-- | Split this amount into several random parts.
genSplitFuel :: Int -> Int -> GenM [Int]
genSplitFuel fuel parts
 = do	fracs	<- replicateM parts (genRandomR (1, fuel))
	return	$ drain fuel fracs
	




