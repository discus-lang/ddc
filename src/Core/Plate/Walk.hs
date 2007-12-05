
-- Transform a tree while walking down it collecting up types
--
module Core.Plate.Walk
	( WalkTable (..)
	, walkZM
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
	, transX_down	:: Maybe ((WalkTable m) -> Exp	-> m Exp)
	, transX_enter	:: (WalkTable m) -> Exp 	-> m Exp
	
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

	, transX_down	= Nothing
	, transX_enter	= \t x -> return x

	, boundT	= Map.empty
	, boundK	= Map.empty
	}

-----
bindT  v t z		= z { boundT 	= Map.insert v t  $ boundT  z }
bindK  v k z		= z { boundK 	= Map.insert v k  $ boundK  z }

lookupT 	table v	= Map.lookup v $ boundT  table

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

-----
instance Monad m => WalkM m Top where
 walkZM z p 
  = case p of
  	PBind	v x		
	 -> do	let Just t	= maybeSlurpTypeX x
	 	let z'		= bindT v t z
	 	x'		<- walkZM z' x
	 	transP z z	$ PBind v x'
		
	PExtern v tv to
	 -> 	transP z z p

	PData	v vs cs		 
	 -> do	cs'		<- mapM (walkZM z) cs
	 	transP z z	$ PData v vs cs'

	PRegion{}	 	-> return p
	PEffect v k		-> return p
	PClass{}		-> return p

	PCtor	v tv to		-> return p
	PClassDict{}		-> return p

	PClassInst v ts context defs
	 -> do	let (vs, xs)	= unzip defs
	 	xs'		<- mapM (walkZM z) xs
		let defs'	= zip vs xs'
	 	return		$ PClassInst v ts context defs'

-----
instance Monad m => WalkM m CtorDef where
 walkZM z xx
  = case xx of	
  	CtorDef v df
	 -> do	df'		<- mapM (walkZM z) df
	 	return		$ CtorDef v df'
		
-----
instance Monad m => WalkM m (DataField Exp Type) where
 walkZM z xx
  = case xx of
  	DataField { dInit = mX }
	 -> do 	mX'		<- liftMaybe (walkZM z) mX
	 	return	xx { dInit = mX' }


-----
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
	XVar v			-> return xx
	
	XLAM b t x
	 -> do	t'		<- walkZM z t
		x'		<- walkZM (bindT (varOfBind b) t' z) x
	 	return		$ XLAM b t' x'

	XLam v t x eff clo
	 -> do	x'		<- walkZM (bindT v t z) x
		t'		<- walkZM z t
		eff'		<- walkZM z eff
		clo'		<- walkZM z clo
	 	return		$ XLam v t' x' eff' clo'
		
	XAPP x t
	 -> do	x'		<- walkZM z x
	 	t'		<- walkZM z t
		return		$ XAPP x' t'
		
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
	 -> do	let z2		= foldr (\(v, t) -> bindT v t) z vts
	 	x'		<- walkZM z2 x
		
		let (vs, ts)	= unzip vts
		ts'		<- mapM (walkZM z2) ts
		let vts'	= zip vs ts'

	 	(transX z2) z2	$ XTet vts' x'

	XDo ss
	 -> case (transX_down z) of 
	     Nothing
	      -> do	-- bind types from this block
	      		let z2		= foldl bindTKF_Stmt z ss
			ss2		<- walkZM z2 ss
			ss3		<- (transSS z2) z2 ss2
			
			(transX z2) z2	$ XDo ss3
			
			
	     Just transDown 
	       -> 	transDown z xx
	       
		
	XMatch aa eff
	 -> do	--mX'		<- liftMaybe (walkZM z) mX
	 	aa'		<- walkZM z aa
	 	return		$ XMatch aa' eff
		
	XConst c t		
	 -> do	t'		<- walkZM z t
	 	(transX z) z	$ XConst c t'

	XLocal r vts x
	 -> do	let z'		= z 
				{ boundT	= Map.union  (Map.fromList vts) 
						$ Map.insert r (TKind KRegion) (boundT z) }

		x'		<- walkZM z' x
	 	return		$ XLocal r vts x'
		
	XType t
	 -> do 	t'		<- walkZM z t
		(transX z) z 	$ XType t'

	XPrim m xx eff
	 -> do	xx'		<- mapM (walkZM z) xx
		(transX z) z	$ XPrim m xx' eff
		
	-- atoms
	XAtom{}			-> (transX z) z xx
	
	
	--
	_ 	-> panic stage
		$  "walkZM[Exp]: no match for " % show xx
	
	
-----
instance Monad m => WalkM m Alt where
 walkZM z ss
  = case ss of
  	AAlt gs x 
	 -> do	let z'		= foldl bindTK_Guard z gs
		gs'		<- walkZM z' gs
	 	x'		<- walkZM z' x
	 	return		$ AAlt gs' x'
		

-----
instance Monad m => WalkM m Guard where
 walkZM z ss
  = case ss of
{-  	GCase w
	 -> do	w'		<- walkZM z w
	 	return		$ GCase w'
-}		
	GExp w x
	 -> do	w'		<- walkZM z w
	 	x'		<- walkZM z x
		return		$ GExp w' x'


-----
instance Monad m => WalkM m Pat where
 walkZM z ss
  = case ss of
  	WConst c
	 -> 	return		$ WConst c
	 
	WCon v lvts
	 -> 	return		$ WCon v lvts


-----
instance Monad m => WalkM m Stmt where
 walkZM z ss
  = case ss of
	SComment{}		-> return ss

	SBind (Just v) x
	 -> do	x'		<- walkZM z x
	 	(transS z) z	$ SBind (Just v) x'

	SBind Nothing x
	 -> do	x'		<- walkZM z x
	 	(transS z) z	$ SBind Nothing x'	





-----
instance Monad m => WalkM m Type where
 walkZM z tt 
  = case tt of
	TContext t1 t2
	 -> do	t1'		<- walkZM z t1
	 	t2'		<- walkZM z t2
		transT z z	$ TContext t1' t2'		

	TWhere t1 vts
	 -> do	let z2		= foldr (\(v, t) -> bindT v t) z vts
	 	t1'		<- walkZM z2 t1
		
		let (vs, ts)	= unzip vts
		ts'		<- mapM (walkZM z2) ts
		let vts'	= zip vs ts'

	 	transT z2 z2	$ TWhere t1' vts'

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
	



-----------------------
-- Helpers for binding Types/Regions
--
bindTFs_VT v t z
 	= bindT v t $ bindFs_Type t z


bindFs_Type t z
 = case t of
 	TForall v k t2	-> bindFs_Type t2 z

	_ -> z
			
			
-- bindTKF_Stmt 
--	Bind the vars and regions present in the type of this statement.
--
bindTKF_Stmt z ss
 = case ss of
 	SBind (Just v) x		
	 -> case maybeSlurpTypeX x of
	    Nothing 	-> z
            Just t	-> bindT v t z

	_			-> z


bindTK_Alt z a
 = case a of
 	AAlt gs x 	-> foldl bindTK_Guard z gs
	 
bindTK_Guard zz g
 = case g of
--	GCase w		-> bindTK_Pat zz w
	GExp  w x	-> bindTK_Pat zz w
	
bindTK_Pat zz ww
 = case ww of
	WConst c	-> zz

 	WCon v lvt
	  -> foldl (\z (l, v, t) -> bindT v t z) zz lvt
 	
