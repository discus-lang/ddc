{-# OPTIONS -fwarn-incomplete-patterns -fwarn-unused-matches -fwarn-name-shadowing #-}

-- | Slurp constraints for kind inference from a desugared source tree.
module DDC.Desugar.Elaborate.Slurp
	(slurpConstraints)
where
import Shared.VarPrim
import DDC.Desugar.Elaborate.Constraint
import DDC.Desugar.Exp
import DDC.Type
import DDC.Type.Data
import DDC.Var
import DDC.Base.SourcePos
import Data.Sequence	as Seq


-- | Slurp kind constraints from the desugared module
slurpConstraints :: Tree SourcePos -> Seq Constraint
slurpConstraints ps
	= Seq.fromList $ concatMap slurpConstraint ps
	
slurpConstraint pp
 = case pp of
 	PKindSig sp v k	
 	 | resultKind k == kEffect
	 -> [Constraint (KSEffect sp) v k]
	
	 | otherwise
	 -> [Constraint (KSSig sp) v k]

	PClassDecl sp v ts vts
	 -> map (\(TVar k (UVar v)) -> Constraint (KSClass sp) v (defaultKind v k)) ts

 	PData sp def@(DataDef{})
	 -> let	k	= dataDefKind def
	        k'	= forcePrimaryRegion (dataDefName def) k
	    in	[Constraint (KSData sp) (dataDefName def) k']

	_	-> []


defaultKind v k
 	| k == KNil	
	= let Just k' = kindOfSpace $ varNameSpace v
	  in  k'

	| otherwise	= k 


-- Ensure the kinds of data type constructors have their primary regions.
forcePrimaryRegion :: Var -> Kind -> Kind
forcePrimaryRegion vData k

	-- unit doesn't need one
 	| vData == primTUnit
	= k

	-- these abstract types don't need one
	| elem vData [primTObj, primTData, primTThunk]	
	= k

	-- unboxed data types don't need one
	| varIsUnboxedTyConData vData			
	= k

	-- don't elaborate types with higher kinds
	| KFun kR _	<- k
	, kR	== kRegion
	= k
	
	| otherwise
	= KFun kRegion k
