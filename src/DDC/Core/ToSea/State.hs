{-# OPTIONS -fwarn-incomplete-patterns -fwarn-unused-matches -fwarn-name-shadowing #-}
module DDC.Core.ToSea.State
	( SeaS(..)
	, SeaM
	, newVarN
	, slurpWitnessKind
	)
where
import DDC.Main.Pretty
import DDC.Var
import Data.Function
import Util				hiding (mapM)
import Prelude				hiding (mapM)
import qualified DDC.Core.Glob		as C
import qualified DDC.Type		as T
import qualified Data.Set		as Set

-- | State of Core to Sea transform.
data SeaS
	= SeaS
	{ -- | variable name generator
	  stateVarGen		:: VarId

	  -- | top level vars known to be CAFs
	, stateCafVars		:: Set Var

	  -- | regions known to be direct
	, stateDirectRegions	:: Set Var

	 -- | the original header glob
	, stateHeaderGlob	:: C.Glob

	  -- | the original module glob.
	, stateModuleGlob	:: C.Glob
	}

type SeaM	= State SeaS

-- | Create a fresh variable in the given namespace.
newVarN :: NameSpace -> SeaM Var
newVarN	space
 = do 	varBind		<- gets stateVarGen
	let varBind'	= incVarId varBind
	modify (\s -> s { stateVarGen = varBind' })

	let var		= (varWithName $ pprStrPlain varBind)
			{ varId		= varBind
			, varNameSpace	= space }
	return var


-- | If this is a witness to constness of a region or type, or purity of an effect
--	then slurp it into the table.
slurpWitnessKind :: T.Kind -> SeaM ()
slurpWitnessKind kk
 = case kk of
	-- const regions
 	T.KApp k (T.TVar kR (T.UVar r))
 	 | k	== T.kDirect
         , kR 	== T.kRegion
	 -> modify $ \s -> s { stateDirectRegions
		 		= Set.insert r (stateDirectRegions s) }

	_ -> return ()

