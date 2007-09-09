{-# OPTIONS -fwarn-incomplete-patterns #-}

module Source.Plate.Trans
	( TransTable(..)
	, transTableId
	, transformV
	, transformVM
	, transformX
	, transformXM
	, transZ
	, transZM )

where

import Util
import Source.Exp

-----
class Monad m => TransM m a
 where	transZM :: TransTable m -> a -> m a
 
-----
transZ 
	:: TransM (State ()) a
	=> TransTable (State ())
	-> a -> a
	
transZ	table x 
	= evalState (transZM table x) ()


-----
data TransTable m
	= TransTable
	{ transV	:: Var   -> m Var

	, transX	:: Exp   -> m Exp
	, decendX	:: Bool

	, transT	:: Type	 -> m Type

	, transS	:: Stmt  -> m Stmt
	, transA	:: Alt	 -> m Alt 
	, transP	:: Top	 -> m Top }


transTableId :: TransTable (State s)
transTableId
	= TransTable
	{ transV	= \x -> return x

	, transX	= \x -> return x
	, decendX	= True

	, transT	= \x -> return x

	, transS	= \x -> return x
	, transA	= \x -> return x
	, transP	= \x -> return x }

-----
transformV f t	= transZ  transTableId { transV = \x -> return $ f x } t
transformX f t	= transZ  transTableId { transX = \x -> return $ f x } t

transformVM f t = transZM transTableId { transV = f } t
transformXM f t	= transZM transTableId { transX = f } t

-----
instance (Monad m, TransM m a) 
		=> TransM m [a]
 where	transZM table xx	
 	 = 	mapM (transZM table) xx
 

instance (Monad m, TransM m a, TransM m b) 
	 	=> TransM m (a, b)
 where	transZM table (a, b)
 	 = do
	 	a'		<- transZM table a
		b'		<- transZM table b
		return		(a', b')


instance (Monad m, TransM m a, TransM m b, TransM m c)
		=> TransM m (a, b, c)
 where	transZM table (a, b, c)
 	 = do	a'		<- transZM table a
	 	b'		<- transZM table b
		c'		<- transZM table c
		return		(a', b', c')


instance (Monad m, TransM m a)
		=> TransM m (Maybe a)

 where 	transZM table Nothing	
   	 = 	return Nothing

 	transZM table (Just x)
	 = do	x'	<- transZM table x
	 	return	$ Just x'
 



-----
instance Monad m => TransM m Var where
 transZM = transV


-----
instance Monad m => TransM m Top where
 transZM table pp
  = case pp of
	PForeign f
	 -> do	f'		<- transZM table f
	 	transP table	$ PForeign f'

	-- Imports
	PImportExtern  v t mT
	 -> do	v'		<- transZM table v
	 	t'		<- transZM table t
		mT'		<- transZM table mT
		return		$ PImportExtern v' t' mT'


	PData v vs fs
	 -> do	v'		<- transZM table v
	 	vs'		<- transZM table vs
		fs'		<- transZM table fs
		transP table	$ PData v' vs' fs'

	-- Classes
	PClass v k
	 -> do	v'		<- transZM table v
	 	return		$ PClass v' k

	PClassInst v ts inh ss
	 -> do 	ss'		<- transZM table ss
		transP table 	$ PClassInst v ts inh ss'

	PProjDict t ss
	 -> do	ss'		<- transZM table ss
	 	transP table	$ PProjDict t ss'

	-- 
 	PStmt s	
	 -> do 	s'		<- transZM table s
		transP table 	$ PStmt s'
		
	PEffect v k
	 -> do	v'		<- transZM table v
	 	return		$ PEffect v' k
		
	PRegion v
	 -> do	v'		<- transZM table v
	 	return		$ PRegion v'
		
		
	_ -> return pp

-----
instance Monad m => TransM m Foreign where
 transZM table ff
  = case ff of
  	OImport f
	 -> do	f'		<- transZM table f
	 	return		$ OImport f'
		
	OExport f
	 -> do	f'		<- transZM table f
	 	return		$ OExport f'
		
	OCCall mS v t
	 -> do	v'		<- transZM table v
	 	t'		<- transZM table t
		return		$ OCCall mS v' t'
		
	OExtern mS v t mT
	 -> do	v'		<- transZM table v
	 	t'		<- transZM table t
		mT'		<- transZM table mT
		return		$ OExtern mS v' t' mT'


-----
instance Monad m => TransM m Stmt where
 transZM table s
  = case s of
    	SBind sp v x
	 | decendX table
	 -> do	v'		<- transZM table v
	 	x'		<- transZM table x
		transS table	$ SBind sp v' x'
		
	 | otherwise
	 -> return s
    
    
	SBindPats sp v ps x
	 | decendX table
	 -> do	v'		<- transZM table v
		ps'		<- transZM table ps
	 	x'		<- transZM table x
		transS table 	$ SBindPats sp v' ps' x'

	 | otherwise
	 -> return s

	SSig {}	-> return s
	
{-
	SSig sp v t
	 | decendX table
	 -> do	v'		<- transZM table v
	 	t'		<- transZM table t
		transS table	$ SSig sp v' t'
		
	 | otherwise
	 -> return s
-}	

-----
instance Monad m => TransM m Exp where
 transZM table x
  = case x of
	XNil 
	 ->	transX table	$ XNil

	XAnnot n x1
	 -> do	x1'		<- transZM table x1
	    	transX table	$ XAnnot n x1'
	
	XUnit 	sp
	 ->	transX table	$ XUnit sp

	XVoid 	sp
	 ->	transX table	$ XVoid sp

	XVar 	sp v
	 -> do	v'		<- transZM table v
	 	transX table 	$ XVar sp v'
	 
	XConst	sp c
	 -> 	transX table	x
	 
	XProj 	sp x1 p
	 -> do 	x1'		<- transZM table x1
	 	transX table	$ XProj sp x1' p

	XLambda sp vs x1
	 -> do	x1'		<- transZM table x1
	    	transX table	$ XLambda sp vs x1'
	    
	XApp 	sp x1 x2
	 -> do	x1'		<- transZM table x1
	 	x2'		<- transZM table x2
	    	transX table	$ XApp sp x1' x2'
	    
	XCase 	sp x1 aa
	 -> do	x1'		<- transZM table x1
	 	aa'		<- transZM table aa
	    	transX table	$ XCase sp x1' aa'

 	XLet 	sp ss x1
	 -> do	ss'		<- transZM table ss
	 	x1'		<- transZM table x1
	    	transX table	$ XLet sp ss' x1'

	XDo 	sp ss
	 -> do	ss'		<- transZM table ss
	    	transX table	$ XDo sp ss'
	    
	XIfThenElse sp x1 x2 x3
	 -> do	x1'		<- transZM table x1
	 	x2'		<- transZM table x2
		x3'		<- transZM table x3
	    	transX table	$ XIfThenElse sp x1' x2' x3'

	-- lambda sugar	    
	XLambdaPats sp ps x1
	 -> do	ps'		<- transZM table ps
	 	x1'		<- transZM table x1
		return		$ XLambdaPats sp ps' x1'

	XLambdaCase sp aa
	 -> do	aa'		<- transZM table aa
	    	transX table	$ XLambdaCase sp aa'

	XLambdaProj sp j xs
	 -> do	xs'		<- transZM table xs
	 	transX table	$ XLambdaProj sp j xs'
	    
	-- match sugar
	XMatch sp aa
	 -> do	aa'		<- transZM table aa
	 	transX table	$ XMatch sp aa'
	    
	-- defix
	XDefix 	sp xx
	 -> do	xx'		<- transZM table xx
	    	transX table	$ XDefix sp xx'
	    
	XDefixApps sp xx
	 -> do	xx'		<- transZM table xx
	 	transX table	$ XDefixApps sp xx'

	XAppSusp sp e1 e2 
	 -> do	e1'		<- transZM table e1
	 	e2'		<- transZM table e2
		transX table	$ XAppSusp sp e1' e2'

	XOp sp v
	 -> do	return x

	-- exceptions
	XThrow sp x
	 -> do	x'		<- transZM  table x
	 	transX table	$ XThrow sp x'
	
	XTry sp x aa w
	 -> do	x'		<- transZM table x
	 	aa'		<- transZM table aa
		w'		<- transZM table w
		transX table	$ XTry sp x' aa' w'
	 	
	-- imperative
	XWhile	sp x1 x2
	 -> do	x1'		<- transZM table x1
	 	x2'		<- transZM table x2
		transX table	$ XWhile sp x1' x2'
	
	XWhen	sp x1 x2
	 -> do	x1'		<- transZM table x1
	 	x2'		<- transZM table x2
		transX table	$ XWhen sp x1' x2'
		
	XUnless	sp x1 x2
	 -> do	x1'		<- transZM table x1
	 	x2'		<- transZM table x2
		transX table	$ XUnless sp x1' x2'

	XBreak sp
	 ->	transX table	$ x

	-- oop
	XObjVar	sp v
	 -> do	v'		<- transZM table v
	 	transX table	$ XObjVar sp v'
		
	XObjField sp v
	 -> do	v'		<- transZM table v
	 	transX table	$ XObjField sp v'
		
	XObjFieldR sp v
	 -> do	v'		<- transZM table v
	 	transX table	$ XObjFieldR sp v'

	XListRange sp b x1 x2
	 -> do	x1'		<- transZM table x1
	 	x2'		<- transZM table x2
		transX table	$ XListRange sp b x1' x2'

	XListComp sp x qs
	 -> do	x'		<- transZM table x
	 	qs'		<- transZM table qs
		transX table	$ XListComp sp x' qs'

	-- 
	XAppE sp x1 x2 eff
	 -> do	x1'		<- transZM table x1
	 	x2'		<- transZM table x2
	    	transX table	$ XAppE sp x1' x2' eff
	    
	XCaseE sp x1 aa eff
	 -> do	x1'		<- transZM table x1
	 	aa'		<- transZM table aa
	    	transX table	$ XCaseE sp x1' aa' eff
	    
	XAt 	sp v x1
	 -> do	x1'		<- transZM table x1
		transX table	$ XAt sp v x1'
 
	-- patterns
	XCon 	sp v xx
	 -> do	v'		<- transZM table v
	 	xx'		<- transZM table xx
		return		$ XCon sp v' xx'
		
	XTuple 	sp xx
	 -> do	xx'		<- transZM table xx
	 	return		$ XTuple sp xx'
		
	XCons 	sp x1 x2
	 -> do	x1'		<- transZM table x1
	 	x2'		<- transZM table x2
		return		$ XCons sp x1' x2'
		
	XList 	sp xx
	 -> do	xx'		<- transZM table xx
	 	return		$ XList sp xx'


-----
instance Monad m => TransM m Alt where
 transZM table a
  = case a of
	APat p x
	 -> do	p'		<- transZM table p
	 	x'		<- transZM table x
		transA table	$ APat p' x'

	AAlt gs x
	 -> do	gs'		<- transZM table gs
	 	x'		<- transZM table x
		transA table	$ AAlt gs' x'

	ADefault x
	 -> do	x'		<- transZM table x
	 	transA table	$ ADefault x'


-----
instance Monad m => TransM m Guard where
 transZM table a
  = case a of
  	GCase pat
	 -> do	pat'		<- transZM table pat
	 	return		$ GCase pat'
		
	GExp x1 x2
	 -> do	x1'		<- transZM table x1
	 	x2'		<- transZM table x2
		return		$ GExp x1' x2'
		
	GBool x
	 -> do	x'		<- transZM table x
	 	return		$ GBool x'
		
	GBoolU x
	 -> do	x'		<- transZM table x
	 	return		$ GBoolU x'
	 
	 
-----
instance Monad m => TransM m Pat where
 transZM table ww
  = case ww of
  	WVar v
	 -> do	v'		<- transZM table v
	 	return		$ WVar v
		
	WConst c
	 ->  	return		$ WConst c
		
	WCon v ps
	 -> do	v'		<- transZM table v
	 	ps'		<- transZM table ps
		return		$ WCon v' ps'

	WConLabel v ps
	 -> do	v'		<- transZM table v
	 	ps'		<- transZM table ps
		return		$ WConLabel v' ps'
		
	WAt v p
	 -> do	v'		<- transZM table v
	 	p'		<- transZM table p
		return		$ WAt v' p'
  
  	WWildcard 
	 -> do	return		$ WWildcard 
  
	WUnit
	 ->	return		$ WUnit
	 
	WTuple ps
	 -> do	ps'		<- transZM table ps
	 	return		$ WTuple ps'
		
	WCons p1 p2
	 -> do	p1'		<- transZM table p1
	 	p2'		<- transZM table p2
		return		$ WCons p1' p2'
		
	WList ps
	 -> do	ps'		<- transZM table ps
	 	return		$ WList ps'
		
	WExp x
	 -> do	x'		<- transZM table x
	 	return		$ WExp x'

	
-----
instance Monad m => TransM m Label where
 transZM table ll
  = case ll of
  	LIndex i		-> return ll
	
	LVar v
	 -> do	v'		<- transZM table v
	 	return		$ LVar v'
	
-----
instance Monad m => TransM m LCQual where
 transZM table q
  = case q of
  	LCGen b x1 x2
	 -> do	x1'		<- transZM table x1
	 	x2'		<- transZM table x2
		return		$  LCGen b x1' x2'
		
	LCLet ss
	 -> do	ss'		<- transZM table ss
	 	return		$  LCLet ss'
		
	LCExp x
	 -> do	x'		<- transZM table x
	 	return		$  LCExp x'
		

-----
instance Monad m => TransM m (DataField Exp Type) where
 transZM table ff
  = case ff of
  	DataField{}
	 -> do	dLabel'		<- transZM table $ dLabel ff
		dInit'		<- transZM table $ dInit ff
		return		$ ff 	{ dLabel	= dLabel'
					, dInit		= dInit' }
					
-----
instance Monad m => TransM m Type where
 transZM table tt	
 	= transT table tt
		

