
module DDC.Desugar.ToCore.Clean
	(cleanGlob)
where
import DDC.Type.TransEnv
import DDC.Core.TransEnv
import DDC.Core.Glob
import DDC.Core.Exp
import DDC.Type
import DDC.Var
import DDC.Main.Error
import Control.Monad.Identity
import Data.Maybe
import qualified Data.Set	as Set
import Data.Set			(Set)
-- import Debug.Trace
-- import DDC.Main.Pretty

stage = "DDC.Desguar.ToCore.Clean"


-- | With higher order programs with complex more-than constraints we sometimes get
--   constraints on variables that aren't bound by a type-lambda.
--   For example:
--
--  @
--   main = \_ : Unit -> ... show (f (Int %r1 -(!e1 :> Read %r1)> Int %r2)) ...
--  @
--
--  That !e1 isn't bound anywhere, so we might as well instantiate it to its constraint.
--  
cleanGlob :: Glob -> Glob
cleanGlob = mapBindsOfGlob cleanTop

cleanTop  :: Top -> Top
cleanTop (PBind v xx) 	= PBind v (cleanX xx)
cleanTop _		= panic stage $ "cleanTop: no match"

cleanX :: Exp -> Exp
cleanX xx 	
 = let	tableT	= transEnvTypeId
		{ transEnvTypeT_up	= cleanT_up 
		, transEnvTypeT_down	= cleanT_down }
		
	tableX	= transEnvCoreId
		{ transEnvCoreX_down	= cleanX_down
		, transEnvCoreT		= Just tableT }

   in	runIdentity $ transEnv tableX Set.empty xx


-- | Transform keep track of bound type variables on the way down.
cleanX_down :: Set Var -> Exp -> Identity (Exp, Set Var)
cleanX_down env xx
 = case xx of
	XLAM b k x	
	 -> let	env'	= Set.union env (Set.fromList $ maybeToList $ takeVarOfBind b)
	    in	return (xx, env')

	_ -> return (xx, env)

cleanT_down :: Set Var -> Type -> Identity (Type, Set Var)
cleanT_down env tt
 = case tt of
	TForall b k t
	 -> let env'	= Set.union env (Set.fromList $ maybeToList $ takeVarOfBind b)
	    in	return (tt, env')
	
	_ -> return (tt, env)


-- | Substitute constraints for unbound effect and closure variables.
cleanT_up :: Set Var -> Type -> Identity Type
cleanT_up env tt
	| TVar k (UMore v tMore) 	<- tt
	, not $ Set.member v env
	= {- trace (pprStrPlain $ vcat 	[ "cleaning " % v 
					, "env = " % env ]) $ -}
	  return tMore
	
	| otherwise
	= return tt



