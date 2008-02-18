
-- Transform a tree while walking down it collecting up types
--
module Core.Plate.Walk
	( WalkTable (..)
	, walkZM
	, walkTreeM
	, walkTableId
	, lookupT)

where

import qualified Data.Map	as Map
import Data.Map			(Map)

import Util
import Shared.Error
import Core.Exp
import Core.Util.Slurp		(maybeSlurpTypeX)

-----
stage	= "Core.Plate.Walk"

-----
data WalkTable m
	= WalkTable 
	{ 
	-- bottom-up transforms
	  transP	:: (WalkTable m) -> Top		-> m Top
	, transX	:: (WalkTable m) -> Exp		-> m Exp
	, transS	:: (WalkTable m) -> Stmt 	-> m Stmt

	, transT	:: (WalkTable m) -> Type	-> m Type

	, transSS	:: (WalkTable m) -> [Stmt]	-> m [Stmt]

	-- top-down transforms
	, transX_enter	:: (WalkTable m) -> Exp 	-> m Exp
	
	-- Functions to bind types and kinds, top down, and unbind them on the way up.
	--	These usually just add the bound types and kinds to the boundT\/boundK maps,
	--	but could be hooked by client code, perhaps for keeping track of what witnesses
	--	are defined on each region.
	--
	, bindT		:: (WalkTable m) -> Var -> Type -> m (WalkTable m)
	, bindK		:: (WalkTable m) -> Var -> Kind -> m (WalkTable m)
	
	, unbindT	:: (WalkTable m) -> Var -> Type -> m (WalkTable m)
	, unbindK	:: (WalkTable m) -> Var -> Type -> m (WalkTable m)
	
	, boundT	:: Map Var Type
	, boundK	:: Map Var Kind
	}
	
	
walkTableId :: WalkTable (State s)
walkTableId
	= WalkTable
	{ transP	= \t x -> return x
	, transX	= \t x -> return x
	, transS	= \t x -> return x

	, transT	= \t x -> return x

	, transSS	= \t x -> return x

	, transX_enter	= \t x -> return x

	, bindT		= \z v t -> return $ z { boundT = Map.insert v t $ boundT z }
	, bindK		= \z v k -> return $ z { boundK = Map.insert v k $ boundK z }

	, unbindT	= \z v t -> return $ z { boundT = Map.delete v $ boundT z }
	, unbindK	= \z v t -> return $ z { boundK = Map.delete v $ boundK z }

	, boundT	= Map.empty
	, boundK	= Map.empty
	}

lookupT 	table v	= Map.lookup v $ boundT  table


-- | When walking over a whole tree we must bind all the regions and dictionaries
--	from PRegions first.
walkTreeM 
	:: Monad m
	=> WalkTable m
	-> Tree -> m Tree
walkTreeM zz pp
 = do	z2	<- foldM bindTKF_Top zz pp
	mapM (walkZM z2) pp



-- | Bind types present in this top level thing
bindTKF_Top
	:: Monad m
	=> WalkTable m -> Top -> m (WalkTable m)

bindTKF_Top z pp
 = case pp of
 	PRegion vR vts
	 -> do	z2		<- bindK z z vR KRegion
		z3		<- foldM (\z (v, t) -> bindT z z v t) z2 vts
		return z3

	_ -> return z


-----
class Monad m => WalkM m a
 where	walkZM :: WalkTable m -> a -> m a
 
instance (Monad m, WalkM m a) => WalkM m [a]
 where	walkZM z xx	= mapM (walkZM z) xx

instance (Monad m, WalkM m a, WalkM m b) => WalkM m (a, b)
 where	walkZM z (a, b)
 	 = do	a'	<- walkZM z a
	 	b'	<- walkZM z b
		return	(a', b')

-- Top ---------------------------------------------------------------------------------------------
instance Monad m => WalkM m Top where
 walkZM z p 
  = case p of
  	PBind	v x		
	 -> do	let Just t	= maybeSlurpTypeX x
	 	z'		<- bindT z z v t
	 	x'		<- walkZM z' x
	 	transP z' z'	$ PBind v x'
		
	PExtern v tv to
	 -> 	transP z z p

	PData	v vs cs		 
	 -> do	cs'		<- mapM (walkZM z) cs
	 	transP z z	$ PData v vs cs'

	PRegion vR vts
	 -> do	z2		<- bindK z z vR KRegion
		z3		<- foldM (\z (v, t) -> bindT z z v t) z2 vts
		transP z3 z3	$ PRegion vR vts

	PEffect v k		-> return p
	PClass{}		-> return p

	PCtor	v tv to		-> return p
	PClassDict{}		-> return p

	PClassInst v ts context defs
	 -> do	let (vs, xs)	= unzip defs
	 	xs'		<- mapM (walkZM z) xs
		let defs'	= zip vs xs'
	 	return		$ PClassInst v ts context defs'


-- CtorDef ----------------------------------------------------------------------------------------
instance Monad m => WalkM m CtorDef where
 walkZM z xx
  = case xx of	
  	CtorDef v df
	 -> do	df'		<- mapM (walkZM z) df
	 	return		$ CtorDef v df'

		
-- DataField --------------------------------------------------------------------------------------
instance Monad m => WalkM m (DataField Var Type) where
 walkZM z xx
  = case xx of
  	DataField { dInit = mX }
	 -> do --	mX'	<- liftMaybe (walkZM z) mX
	 	return	xx { dInit = mX }


-- Exp --------------------------------------------------------------------------------------------
instance Monad m => WalkM m Exp where
 walkZM z xx_
  = do	xx	<- (transX_enter z) z xx_
  	walkZM2 z xx
	
walkZM2 z xx
  = case xx of
	XNothing		-> return xx
	XNil			-> return xx

	XAnnot a x
	 -> do	x'		<- walkZM z x
	 	return		$ XAnnot a x'
		
	-- core constructs
	XVar v t		-> return xx
	
	XLAM b k x
	 -> do	-- k'		<- walkZM z k
	 	z'		<- bindK z z (varOfBind b) k
		x'		<- walkZM z' x
	 	transX z' z'	$ XLAM b k x'

	XLam v t x eff clo
	 -> do	t'		<- walkZM z  t
		z'		<- bindT  z  z v t' 
	 	x'		<- walkZM z' x
		
		eff'		<- walkZM z' eff
		clo'		<- walkZM z' clo
	 	transX z' z'	$ XLam v t' x' eff' clo'
		
	XAPP x t
	 -> do	x'		<- walkZM z x
	 	t'		<- walkZM z t
		transX z z	$ XAPP x' t'
		
	XApp x1 x2 eff
	 -> do	x1'		<- walkZM z x1
	 	x2'		<- walkZM z x2
		eff'		<- walkZM z eff
		(transX z) z	$ XApp x1' x2' eff'

	XTau t x
	 -> do	x'		<- walkZM z x
		t'		<- walkZM z t
		(transX z) z	$ XTau t' x'

	XTet vts x
	 -> do	z'		<- foldM (\z (v, t) -> bindT z z v t) z vts
	 	x'		<- walkZM z' x
		
		let (vs, ts)	= unzip vts
		ts'		<- mapM (walkZM z') ts
		let vts'	= zip vs ts'

	 	transX z' z'	$ XTet vts' x'

	XDo ss
	 -> do	-- bind types from this block
	   	z'		<- foldM bindTKF_Stmt z ss
		ss2		<- walkZM z' ss
		ss3		<- transSS z' z' ss2
			
		transX z' z'	$ XDo ss3
		
	XMatch aa
	 -> do 	aa'		<- walkZM z aa
	 	transX z z	$ XMatch aa'
		
	XLit l 		
	 -> do	transX z z	$ XLit l

	XLocal vR vts x
	 -> do	z2		<- bindK z z vR KRegion
		z3		<- foldM (\z (v, t) -> bindT z z v t) z2 vts
	 			
		x'		<- walkZM z3 x
	 	transX z3 z3 	$ XLocal vR vts x'
		
	XType t
	 -> do 	t'		<- walkZM z t
		transX z z 	$ XType t'

	XPrim m xx
	 -> do	xx'		<- mapM (walkZM z) xx
		(transX z) z	$ XPrim m xx'
	
	XProject x j
	 -> do	x'		<- walkZM z x
	 	return		$ XProject x' j
		
	-- atoms
	XAtom{}			-> (transX z) z xx
	
	
	--
	_ 	-> panic stage
		$  "walkZM[Exp]: no match for " % show xx
	
	
-- Alt --------------------------------------------------------------------------------------------
instance Monad m => WalkM m Alt where
 walkZM z ss
  = case ss of
  	AAlt gs x 
	 -> do	z'		<- foldM bindTK_Guard z gs
		gs'		<- walkZM z' gs
	 	x'		<- walkZM z' x
	 	return		$ AAlt gs' x'
		

-- Guard ------------------------------------------------------------------------------------------
instance Monad m => WalkM m Guard where
 walkZM z ss
  = case ss of
	GExp w x
	 -> do	w'		<- walkZM z w
	 	x'		<- walkZM z x
		return		$ GExp w' x'


-- Pat  --------------------------------------------------------------------------------------------
instance Monad m => WalkM m Pat where
 walkZM z ss
  = case ss of
  	WLit v 		-> return ss
	WCon v lvts	-> return ss


-- Stmt --------------------------------------------------------------------------------------------
instance Monad m => WalkM m Stmt where
 walkZM z ss
  = case ss of
--	SComment{}		-> return ss

	SBind (Just v) x
	 -> do	x'		<- walkZM z x
	 	(transS z) z	$ SBind (Just v) x'

	SBind Nothing x
	 -> do	x'		<- walkZM z x
	 	(transS z) z	$ SBind Nothing x'	





-- Type --------------------------------------------------------------------------------------------
instance Monad m => WalkM m Type where
 walkZM z tt 
  = case tt of
	TContext k1 t2
	 -> do	-- t1'		<- walkZM z t1
	 	t2'		<- walkZM z t2
		transT z z	$ TContext k1 t2'		

	TFetters t1 fs
	 -> do	z'		<- foldM (\z (v, t) -> bindT z z v t) z [(v, t) | FWhere v t <- fs]
	 	t1'		<- walkZM z' t1
		
		fs'		<- mapM (walkZM z') fs
	 	transT z' z'	$ TFetters t1' fs'

	TSum k ts
	 -> do	ts'		<- walkZM z ts
	 	transT z z	$ TSum k ts'

	TMask k t1 t2
	 -> do	t1'		<- walkZM z t1
	 	t2'		<- walkZM z t2
		transT z z	$ TMask k t1' t2'
		
	-- data
	TData 	v ts
	 -> do	ts'		<- mapM (walkZM z) ts
	 	transT z z	$ TData v ts'
		
	TFunEC t1 t2 eff clo
	 -> do	t1'		<- walkZM z t1
		t2'		<- walkZM z t2
		eff'		<- walkZM z eff
		clo'		<- walkZM z clo
	 	transT z z 	$ TFunEC t1' t2' eff' clo'

	-- effect
	TEffect v ts
	 -> do	ts'		<- mapM (walkZM z) ts
	 	transT z z	$ TEffect v ts'
		
	-- closure
	TFree 	v t
	 -> do	t'		<- walkZM z t
	 	transT z z	$ TFree v t'
		
	-- class		
	TClass v ts
	 -> do	ts'		<- mapM (walkZM z) ts
	 	transT z z	$ TClass v ts'

	_ -> 	transT z z tt
	

-- Fetter --------------------------------------------------------------------------------------------
instance Monad m => WalkM m Fetter where
 walkZM z ff
  = case ff of
  	FWhere v t	
	 -> do	t'	<- walkZM z t
	 	return	$ FWhere v t'
		
	FMore v t
	 -> do	t'	<- walkZM z t
	 	return	$ FMore v t'

-- bindTKF_Stmt 
--	Bind the vars and regions present in the type of this statement.
--
bindTKF_Stmt 
	:: Monad m
	=> WalkTable m -> Stmt -> m (WalkTable m)
	
bindTKF_Stmt z ss
 = case ss of
 	SBind (Just v) x		
	 -> case maybeSlurpTypeX x of
	    Nothing 	-> return z
            Just t	-> bindT z z v t

	_	-> return z

bindTK_Guard zz g
 = case g of
	GExp  w x	-> bindTK_Pat zz w
	
bindTK_Pat zz ww
 = case ww of
	WLit{}		-> return zz
 	WCon v lvt
	 -> foldM (\z (l, v, t) -> bindT z z v t) zz lvt
 	
