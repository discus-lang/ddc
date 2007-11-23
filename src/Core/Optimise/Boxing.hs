
module Core.Optimise.Boxing
(
	coreBoxingTree,
	unboxedType
)

where

import Util
import qualified Debug.Trace	as Debug
import qualified Shared.Var	as Var
import qualified Shared.VarBind	as Var
import qualified Shared.Unique	as Unique
import Shared.Var	(NameSpace(..))
import Shared.Error
import Shared.Exp
import Shared.Literal
import Shared.VarPrim
import Shared.VarUtil

import qualified Data.Map	as Map
import Data.Map			(Map)

import qualified Data.Set	as Set
import Data.Set			(Set)

import Core.Exp
import Core.Util

import qualified Core.Plate.Walk	as Walk
import Core.Plate.Walk			(WalkTable)

import qualified Core.Plate.Trans	as Trans

import Core.Inline
import Core.Snip
import Core.Reconstruct

type	BoxingM	= VarGenM

-----
stage		= "Core.Boxing"
uniqueSnip	= "x" ++ Unique.coreBoxing

-----

unboxFuns2 = 
	[ ("primInt32_add",	5)
	, ("primInt32_sub",	5)
	, ("primInt32_div",	5)
	, ("primInt32_mul",	5)
	, ("primInt32_eq",	5)
	, ("primInt32_neq",	5)
	, ("primInt32_gt",	5)
	, ("primInt32_ge",	5)
	, ("primInt32_lt",	5)
	, ("primInt32_le",	5)

	, ("&&",		5)
	, ("||",		5)
	, ("primInt32_mod",	5)

	-- float
	, ("primFloat32_add",	5)
	, ("primFloat32_sub",	5)
	, ("primFloat32_div",	5)
	, ("primFloat32_mul",	5)
	, ("primFloat32_eq",	5)
	, ("primFloat32_neq",	5)
	, ("primFloat32_gt",	5)
	, ("primFloat32_ge",	5)
	, ("primFloat32_lt",	5)
	, ("primFloat32_le",	5)
	
	, ("arrayUI_get", 	6) ]

coreBoxingTree
	:: Set Var
	-> Tree		-- source tree
	-> Tree		-- header tree
	-> Tree

coreBoxingTree topVars cSource cHeader
 = let	cSnip		= snipBoxing cSource

	cInlined	= inlinePureSingleTree cSnip

	cZapped		= zapUnboxBox 		cInlined
	cResnip		= snipTree Set.empty uniqueSnip  cZapped
	cEat		= eatAnnotsTree 	cResnip
	cRecon		= reconstructTree cHeader cEat

   in	cRecon


-----
snipBoxing :: Tree -> Tree
snipBoxing cSource
 = evalState 
	(Walk.walkZM	
		Walk.walkTableId 
			{ Walk.transSS	= boxWrapSS 
			, Walk.boundT	= slurpTypeMapPs cSource }
		cSource)
	(Var.XBind "xBS" 0)


boxWrapSS
	:: WalkTable BoxingM
	-> [Stmt]
	-> BoxingM [Stmt]

boxWrapSS table ss
 	= liftM concat
	$ mapM (boxWrapS table) ss
		
boxWrapS 
	:: WalkTable BoxingM 
	-> Stmt
	-> BoxingM [Stmt]


boxWrapS table stmt
	| SBind xV x				<- stmt
	, (XAppFP (XVar funV) _ : args)		<- flattenAppsE $ slurpExpX x
	, Just arity				<- lookup (Var.name funV) unboxFuns2
	, length args == arity
	= do
		-- unbox the args
		(vsUnbox, mrsUnbox, mssUnbox)
			<- liftM (unzip3. catMaybes)
			$  mapM (unboxArg table) 
			$  map (\(XAppFP x eff) -> x)
			$  args

		let rsUnbox	= catMaybes mrsUnbox
		let ssUnbox	= catMaybes mssUnbox

		-- work out the resultant unboxed type
		let Just xT	= maybeSlurpTypeX x

		let Just xTU@(TData _ [xuR@(TVar KRegion _)])
				= unboxedType xT
		
		vPrim	<- newVarN NameValue
		return	$  ssUnbox
		        ++ [ SBind (Just vPrim)	
				$ XPrim (MFun funV xTU)
					[ XVar v | v <- vsUnbox]
					(makeTSum KEffect [eReadR table r | r <- rsUnbox]) 

			   , SBind xV	
			  	$ XPrim (MBox xT xTU)
					[XVar vPrim]
					(makeTSum KEffect [ eReadR table xuR ]) ]
							
	| otherwise
	= return [stmt]


unboxArg table x
	|  XVar v			<- x
	, Just t				<- Walk.lookupT table v
	, tF@(TData vC [r@(TVar KRegion rV)])	<- followArgType t
	= do
		vU	<- newVarN NameValue
		let tU	= fromMaybe
				(panic stage $ "unboxArg: no unboxed version of " % tF)
				(unboxedType tF)

		return	$ Just
			$ (vU, Just r, Just $ SBind (Just vU) $ forceUnbox table x tU tF r)

	| XVar v			<- x
	= do
		return	$ Just 
			$ (v, Nothing, Nothing)
		
	| XAPP x1 t			<- x
	=	unboxArg table x1
		
	| otherwise
	= 	return	$ Nothing

followArgType t
 = case t of
	TContext c t	-> followArgType t
	_		-> t



eReadR	table r@(TVar KRegion rV)
	| Just fs	<- Nothing -- Walk.lookupFs table rV
	, elem (TClass primConst [r]) fs
	= pure
	
	| otherwise
	= TEffect primRead [r]
	


forceUnbox table x tU tB r@(TVar KRegion rV)
	| Just fs	<- Nothing -- Walk.lookupFs table rV
	, elem (TClass primLazy [r]) fs
	= XPrim (MUnbox tU tB)
		[XPrim MForce [x] pure]
		(eReadR table r)

	| otherwise
	= XPrim (MUnbox tU tB)
		[x]
		(eReadR table r)
	

	 
	
-----
zapUnboxBox
	:: Tree
	-> Tree
	
zapUnboxBox cTree
	= Trans.transformX zapUnboxBoxX cTree
	
zapUnboxBoxX xx
 = case xx of
 	XPrim 	(MUnbox _ _) 
		[XPrim    (MBox _ _) [x] bEff] 
		uEff					-> x

	XPrim	(MUnbox _ _) 
		[XAnnot n (XPrim (MBox _ _) [x] bEff)] 
		uEff					-> x

	_						-> xx
	

-----
eatAnnotsTree
	:: Tree 
	-> Tree
	
eatAnnotsTree cTree
	= Trans.transformX eatAnnotsX cTree
	
eatAnnotsX xx
 = case xx of
 	XAnnot [NUseCount _] x	-> x
	_			-> xx
	


-----
unboxedType :: Type -> Maybe Type
unboxedType t
 = case t of
	TContext t1 t2
	 -> unboxedType t2

	TWhere t1 vts
	 -> unboxedType t1

 	TData v [r]
	 | Var.bind v == Var.TBool	-> Just $ TData primTBoolU  	[r]
	 | Var.bind v == Var.TInt	-> Just $ TData primTInt32U 	[r]
	 | Var.bind v == Var.TFloat	-> Just $ TData primTFloat32U	[r]
	 | Var.bind v == Var.TString	-> Just $ TData primTStringU	[r]
--	 | Var.bind v == Var.TChar	-> Just $ TCon primTCharU	[r]

	_ -> Nothing
				


