
module Core.Util.Mask
	( cleanLinkVsT 
	, maskLocalE 
	, flattenSumT )
where

import Util
import Core.Exp
import Core.Bits
import Core.Plate.Trans
import Core.Util

import qualified Data.Set	as Set
import Data.Set			(Set)

import qualified Shared.Var	as Var
import Shared.Var		(Var)

-----
-- cleanLinkVsT
--	Clean out uninteresting effect/closure variables.
--
--	When we get effect/closure back from mapInst it'll contain e/c variables from 
--	the graph which were never bound by a forall - and aren't ports of the function.
--	They won't be in scope if we leave them in, so clean them out.

cleanLinkVsT :: Set Var -> Type -> Type
cleanLinkVsT boundVs t		
	= flattenSumT
	$ transformT (cleanLinkVsT' boundVs) t

cleanLinkVsT' boundVs t@(TVar k v)
	| elem k [KEffect, KClosure]
	= if Set.member v boundVs 
	   then	t
	   else TBot k

cleanLinkVsT' _ t	= t




-----
-- maskLocalEs
--	Mask effects which are acting on regions which aren't in this set.
--
maskLocalE :: Set Var -> Effect -> Effect
maskLocalE boundVs eff	
	= flattenSumT
	$ transformT (maskLocalE1 boundVs) eff


maskLocalE1 boundVs eff
 	| TEffect vE [TVar KRegion vR]	<- eff
	, elem (Var.bind vE) [Var.ERead, Var.EWrite]
	, not $ Set.member vR boundVs
	= pure
	
	| otherwise
	= eff
	

 	



-----
-- flattenSumT
--	Flatten out sums in a type, eg
--		${ ${a}; ${b}; }	=> ${ a; b; }
--
flattenSumT :: Type -> Type
flattenSumT 
 = transformT 
	(\tt -> case tt of
		TSum k ts	-> makeSumT k ts
		_		-> tt)
