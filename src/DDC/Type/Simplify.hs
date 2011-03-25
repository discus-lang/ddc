{-# OPTIONS -fwarn-incomplete-patterns -fwarn-unused-matches -fwarn-name-shadowing #-}
{-# OPTIONS -Wnot #-}
module DDC.Type.Simplify
	(simplifyT)
where
import DDC.Main.Error
import DDC.Main.Pretty
import DDC.Type.Builtin
import DDC.Type.Compounds
import DDC.Type.Kind
import DDC.Type.Predicates
import DDC.Type.Exp
import DDC.Type.Pretty
import DDC.Type.Simplify.Usage
import System.IO.Unsafe
import qualified Data.HashTable	as Hash
import qualified Data.Map	as Map
import qualified Data.Set	as Set
import Control.Monad
import Data.Maybe

stage	= "DDC.Type.Simplify"

-- | Simplify a type with some outer-most constraints to normal form.
--   This inlines the appropriate constraints into the body, and drops any left-over
--   unreachable constraints.
--   TODO: make this to closure trimming as well.
--   TODO: make it do all the simplifications we have.
--   TODO: make it work on arbitrary types, not just ones with outermost constraints.
simplifyT :: Type -> Type
simplifyT tt
 = unsafePerformIO $ simplifyIO tt

-- | Simplify a type to normal form.
--   This is in IO because it uses a HashTable internally.
simplifyIO :: Type -> IO Type
simplifyIO tt@(TConstrain tBody crs@(Constraints crsEq crsMore crsOther))
 = do	putStrLn "--------- slurpIO"

	-- Slurp a table of how each of the constrained variable is used.
	-- Any variables free in the body are "Wanted", meaning we don't erase 
	-- constraints on them (unless their trivial bottom constraints).
	uses	<- emptyUsage 
	addUsageFree uses UsedWanted tBody
	slurpUsageCrs uses crs
	
	-- For constrained value variables that only appear on the right of closure
	-- constraints, these are just schemes for stuff in the closure of a function, 
	-- so it's best to trim them right up front.
	let trimSchemeConstraint (t1@(TVar k1 u1), t2)
	     | isValueKind k1
	     = do usage	<- lookupUsage uses u1
		  if Map.toList usage == [(UsedEq OnLeft, 1), (UsedMore OnRight KindClo, 1)]
		     then return (t1, trimToMaterialT t2)
		     else return (t1, t2)
	     | otherwise
	     = return (t1, t2)
		
	crsEq'	<- liftM Map.fromList
		$  mapM trimSchemeConstraint 
		$  Map.toList crsEq
		
	-- dump
	us	<- Hash.toList $ useTable uses
	putStrLn $ pprStrPlain $ vcat us
	
	let crs' = Constraints crsEq' crsMore crsOther
	let tt'	 = TConstrain  tBody crs'
	
	return tt'
	
simplifyIO tt 
 = return tt
	 


-- | Trim a normal value type down to its material components.
--	This does light trimming, keeping value types that contain any material components.
--	We want to leave enough information that we can still recover the set of 
--	dangerous variables later.
--	There can be embedded TConstrain nodes by they may only contain effect or closure constraints.
--	If there are several components this produces a TSum.
trimToMaterialT :: Type -> Type
trimToMaterialT tt
	= makeTSum kValue 
	$ trimToMaterialT' Map.empty Set.empty tt


trimToMaterialT' crsClo vsQuant tt
 = case tt of
	TForall b k tBody
	 -> trimToMaterialT'
		crsClo
		(vsQuant `Set.union` (Set.fromList $ maybeToList $ takeVarOfBind b))
		tBody

	-- TODO: add more closure constraints
	-- Remember any closure constraints, as there may be material 
	-- references to these in the body of the type.
	TConstrain tBody (Constraints crsEq crsMore crsFetter)
	 -> trimToMaterialT'
		crsClo vsQuant tBody

	-- TODO: follow closure in functions.
	TApp{}
	 | Just (t1, t2, eff, clo)	<- takeTFun tt
	 -> [] 

	 -- TODO: if all the variables in a type are either
	 --       quantified or immaterial then we can erase the type completely.
	 | Just (tc, ts)		<- takeTDataTC tt
	 -> panic stage $ "got data" % tt
	 
	_ -> panic stage $ "no match for " % tt

