{-# OPTIONS -fwarn-incomplete-patterns -fwarn-unused-matches -fwarn-name-shadowing #-}

-- | Taking arities and operational types.
module	DDC.Core.OpType
	( superOpTypeP
	, superOpTypeX
	, superOpTypePartT
	, slurpSuperAritiesP)
where
import Util
import DDC.Main.Error
import DDC.Core.Exp
import DDC.Type
import DDC.Type.Data.Base
import DDC.Var
import qualified Shared.VarPrim	as Var
import qualified Data.Map	as Map
import {-# SOURCE #-} DDC.Core.Check.Exp

stage = "DDC.Core.OpType"


-- | Work out the operational type of a supercombinator.
--	The operational type of an object different from the value type in two main respects:
--
--	1) The Sea translation doesn't care about alot of the type information present
--	   in the core types. eg boxed objects are just Obj*, and regions aren't used at all
--
--	2) Supercombinators can return function objects, with value type (a -> b), but the
--	   sea code treats them as just vanilla boxed objects.
--
superOpTypeP ::	 Top -> Type
superOpTypeP	pp
 = case pp of
 	PBind _ x
	 -> let	parts	= superOpType' x
	    in	makeTFunsPureEmpty parts

	-- external functions and ctors carry their operational
	--	types around with them.
	PExtern _ _ to	-> to

	_ 	-> panic stage
		$ "superOpTypeP: no match for " % show pp % "\n"


-- | Work out the operational type of this expression
superOpTypeX :: Exp -> Type
superOpTypeX xx
	= makeTFunsPureEmpty $ superOpType' xx

superOpType'	xx
 = case xx of
	-- skip over type information
	XLAM    _ _ x	-> superOpType' x

	-- slurp off parameter types
 	XLam _ t x _ _
	 -> superOpTypePartT t :  superOpType' x

	-- take the type of the body of the super from the XTau enclosing it.
	XTau	t _	-> [superOpTypePartT t]

	-- there's no XTau enclosing the body, so we'll have to reconstruct
	--	the type for it manually.
	_		-> [superOpTypePartT
			$  checkedTypeOfOpenExp (stage ++ ".superOpType") xx]

superOpTypePartT :: Type -> Type
superOpTypePartT tt
 = case tt of
	TNil			-> TNil

	-- skip over constraints
	TForall _ _ t		-> superOpTypePartT t
	TConstrain t _		-> superOpTypePartT t

	-- an unboxed var of airity zero, eg Int32#
	TCon (TyConData name _ _)
	 | isUnboxedT tt
	 -> makeTData name kValue []

	-- a tycon of arity zero, eg Unit
	TCon TyConData{}
	 -> makeTData Var.primTData kValue []

	TApp{}
	 -> let	result
		 -- unboxed types are represented directly, and the Sea
		 --	code must know about them.
	 	 | Just (v, k, ts)	<- takeTData tt
		 , v == Var.primTPtrU
		 = makeTData v k (map superOpTypePartT ts)

		 -- an unboxed tycon of some aritity, eg String#
		 | Just (v, k, _)	<- takeTData tt
		 , isUnboxedT tt
		 = makeTData v k []

		 -- boxed types are just 'Data'
		 | Just (_, k, _)	<- takeTData tt
		 = makeTData Var.primTData k []

		 -- all function objects are considered to be 'Thunk'
		 | Just _		<- takeTFun tt
		 = makeTData Var.primTThunk kValue []

		 | otherwise
		 = makeTData Var.primTObj kValue []
	   in result

	-- some unknown, boxed object 'Obj'
	TVar k _
	 | k == kValue
	 -> makeTData Var.primTObj kValue []

	_	-> panic stage
		$  "superOpTypePart: no match for " % show tt % "\n"



-- | Slurp the name and arity from this top level thing.
slurpSuperAritiesP :: Top -> Map Var Int
slurpSuperAritiesP pp
 = case pp of
	PExtern v _ tOperational
	 -> let arity	= (length $ flattenTFuns tOperational) - 1
	    in	Map.singleton v arity

	PBind   v x
	 -> let	tOperational	= superOpTypeX x
		arity		= (length $ flattenTFuns tOperational) - 1
	    in  Map.singleton v arity

	PData def
	 -> Map.unions
	 $  map slurpSuperArityCtorDef
	 $  Map.elems $ dataDefCtors def

	_ -> Map.empty

slurpSuperArityCtorDef :: CtorDef -> Map Var Int
slurpSuperArityCtorDef (CtorDef vCtor _ arity _ _)
	= Map.singleton vCtor arity
