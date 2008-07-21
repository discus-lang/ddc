{-# OPTIONS -fwarn-incomplete-patterns #-}

-- Sea.Plate.Trans
--	Transform over Sea.Exp
--	
--
--	* Keep the order of constructors in the transform functions the same as in Sea.Exp.
--	  this makes it much easier to see if they're all present and accounted for.


module Sea.Plate.Trans
	( TransTable (..)
	, transTableId
	, transZ
	, transZM
	, transformN
	, transformX
	, transformSM
	, transformSSM
	, transformSS )
where

import Util
import Sea.Exp


-----
class Monad m => TransM m a1 a2 exp where
 transZM 
 	:: TransTable m a1 a2
	-> exp a1 -> m (exp a2)
 
-----
transZ 
	:: TransM (State ()) a1 a2 exp
	=> TransTable (State ()) a1 a2
	-> exp a1 -> exp a2
	
transZ	table x 
	= evalState (transZM table x) ()


-----
data TransTable m a1 a2
	= TransTable
	{ transV	:: Var		-> m Var
	, transN	:: a1		-> m a2

	, transX	:: Exp a2	-> m (Exp a2)
	, decendX	:: Bool

	, transSS	:: [Stmt a2]	-> m [Stmt a2]
	, transS	:: Stmt	a2	-> m (Stmt a2)
	, transA	:: Alt	a2	-> m (Alt a2)
	, transP	:: Top	a2	-> m (Top a2) }

transTableId 
	:: (a1 -> State s a2)
	-> TransTable (State s) a1 a2

transTableId transN'
	= TransTable
	{ transV	= \x -> return x
	, transN	= transN'

	, transX	= \x -> return x
	, decendX	= True

	, transSS	= \x -> return x
	, transS	= \x -> return x
	, transA	= \x -> return x
	, transP	= \x -> return x }

-----
transformN	f z	= transZ  (transTableId (\n -> return $ f n)) z
transformX	f z	= transZ  (transTableId return) { transX  = \x -> return $ f x } z


-----
transformSM 
	:: (TransM (State s) a2 a2 exp)
	=> (Stmt a2 -> State s (Stmt a2))
	-> exp a2
	-> State s (exp a2)
	
transformSM	f z	= transZM (transTableId return) { transS  = f } z

-----
transformSSM	f z	= transZM (transTableId return) { transSS = f } z


transformSS	f z	= transZ  (transTableId return) { transSS = \x -> return $ f x } z



-- Top ---------------------------------------------------------------------------------------------
instance Monad m => TransM m a1 a2 Top where
 transZM table p
  = case p of
	PNil 
	 ->	transP table PNil
	
	-- fix me
	PData v ctors
	 ->	transP table	$ PData v []

	PCtor v ts t
	 ->	transP table	$ PCtor v ts t
	 
	-- supers
	PProto v ts t
	 ->	transP table	$ PProto v ts t
	 
 	PSuper v args t ss
	 -> do	ss2		<- mapM (transZM table) ss
		ss3		<- transSS table ss2
		transP table	$ PSuper v args t ss3
		
	-- cafs
	PCafProto v	
	 ->	transP table	$ PCafProto v
	 
	PCafSlot v
	 ->	transP table	$ PCafSlot v
	 
	PCafInit v ss
	 -> do	ss2		<- mapM (transZM table) ss
	 	ss3		<- transSS table ss2
		transP table	$ PCafInit v ss3
		
	-- atoms
	PAtomProto v t
	 -> 	transP table	$ PAtomProto v t
	 
	PAtom v t
	 ->	transP table	$ PAtom v t
	 
	-- structures
	PStruct v vts
	 ->	transP table	$ PStruct v vts
	 
	-- hackery
	PHashDef s1 s2
	 ->	transP table	$ PHashDef s1 s2
	 
	PInclude s
	 ->	transP table	$ PInclude s
	 
	PIncludeAbs s
	 ->	transP table	$ PIncludeAbs s
	 
	PHackery s
	 ->	transP table	$ PHackery s
	 
	PComment s
	 ->	transP table	$ PComment s
	 
	PBlank
	 ->	transP table	$ PBlank
	 


-- Stmt ---------------------------------------------------------------------------------------------
instance Monad m => TransM m a1 a2 Stmt where
 transZM table s
  = case s of
	-- misc
	SBlank
	 ->	transS table	$ SBlank

 	SComment str
	 ->	transS table	$ SComment str
	 
	SHackery str
	 -> 	transS table	$ SHackery str

	-- stack
	SAuto	v t
	 -> do	v'		<- transV table v
	 	transS table	$ SAuto v' t

	SEnter 	countS
	 -> 	transS table 	$ SEnter countS
	 
	SLeave	countS
	 ->	transS table 	$ SLeave countS


	-- assignment	 
	SAssign x1 t x2
	 -> do 	x1'		<- transZM table x1
	 	x2'		<- transZM table x2
		transS table	$ SAssign x1' t x2'

	SStmt x
	 -> do	x'		<- transZM table x
	 	transS table	$ SStmt x'

	-- control flow
	SReturn x
	 -> do	x'		<- transZM table x
	 	transS table	$ SReturn x'
		
	SLabel v
	 -> do	v'		<- transV table v
	 	transS table	$ SLabel v'
		
	SGoto v
	 -> do	v'		<- transV table v
	 	transS table	$ SGoto v'
		
	SSwitch x aa
	 -> do	x'		<- transZM table x
	 	aa'		<- mapM (transZM table) aa
		transS table	$ SSwitch x' aa'

	SMatch aa
	 -> do	aa'		<- mapM (transZM table) aa
	 	transS table	$ SMatch aa'
				

		
-- Exp ---------------------------------------------------------------------------------------------
instance Monad m => TransM m a1 a2 Exp where
 transZM table x
  = case x of
 	XNil		
	 -> 	transX table 	$ XNil

	XVar v 
	 -> do 	v'		<- transV table v
		transX table	$ XVar v'

	XSlot  v i
	 -> do 	v'		<- transV table v
		transX table	$ XSlot v' i

	XSlotCAF v
	 -> do	v'		<- transV table v
	 	transX table	$ XSlotCAF v'
		
	XSlotA v i
	 -> do	v'		<- transV table v
	 	transX table	$ XSlotA v' i

	XGlobal v
	 -> do	v'		<- transV table v
	 	transX table	$ XGlobal v
		
	-- application
	XTailCall v xs
	 -> do	v'		<- transV table v
	 	xs'		<- mapM (transZM table) xs
		transX table	$ XTailCall v' xs'

	XCall v xs
	 -> do 	v'		<- transV table v
	 	xs'		<- mapM (transZM table) xs
		transX table	$ XCall v' xs'
		
	XCallApp v i xs
	 -> do	v'		<- transV table v
	 	xs'		<- mapM (transZM table) xs
		transX table	$ XCallApp v' i xs'
		
	XApply x xs
	 -> do	x'		<- transZM table x
	 	xs'		<- mapM (transZM table) xs
		transX table	$ XApply x' xs'
		
	XCurry v i xs
	 -> do	v'		<- transV table v
	 	xs'		<- mapM (transZM table) xs
		transX table	$ XCurry v' i xs'
		
	XSuspend v xs
	 -> do	v'		<- transV table v
	 	xs'		<- mapM (transZM table) xs
		transX table	$ XSuspend v' xs'

	XPrim v xs
	 -> do	xs'		<- mapM (transZM table) xs
	 	transX table	$ XPrim v xs'
		
	-- projection
	XArg x t i
	 -> do	x'		<- transZM table x
		transX table	$ XArg x' t i
		
	XTag x
	 -> do	x'		<- transZM table x
	 	transX table	$ XTag x'
		
	XField x v f
	 -> do 	x'		<- transZM table x
		transX table	$ XField x' v f
		
	XFieldR x v f
	 -> do	x'		<- transZM table x
	 	transX table	$ XFieldR x' v f
		
	-- constants
	XCon v
	 -> do	v'		<- transV table v
	 	transX table	$ XCon v'
		
	XInt i
	 -> 	transX table 	$ XInt i
	
	XUnit
	 ->	transX table 	$ XUnit
	 
	XLit l
	 -> 	transX table 	$ XLit l
	 
	XSuper v
	 -> do	v'		<- transV table v
	 	transX table	$ XSuper v'

	XTagThunk
	 ->	transX table 	$ XTagThunk

	XAtom v
	 -> do	v'		<- transV table v
	 	transX table 	$ XAtom v'
	
	-- control
	XLabel v
	 ->	transX table 	$ XLabel v

	 
	XNull
	 -> 	transX table 	$ XNull

	-- boxing
	XBox 	t x
	 -> do	x'		<- transZM table x
		transX table	$ XBox t x'
		
	XUnbox 	t x
	 -> do	x'		<- transZM table x
		transX table	$ XUnbox t x'

	XForce 	x
	 -> do	x'		<- transZM table x
	 	transX table	$ XForce x'
		
		
	-- allocation
	XAlloc i
	 ->	transX table 	$ XAlloc i
	 
	XAllocThunk v airity args
	 -> do	v'		<- transV table v
	 	transX table	$ XAllocThunk v' airity args
		
	XAllocData v airity
	 -> do	v'		<- transV table v
	 	transX table	$ XAllocData v' airity
		
	XAllocSusp v airity
	 -> do	v'		<- transV table v
	 	transX table	$ XAllocSusp v' airity

	XAllocDataAnchored v i
	 -> do	v'		<- transV table v
	 	transX table	$ XAllocDataAnchored v i 
	 	

-- Alt ---------------------------------------------------------------------------------------------
instance Monad m => TransM m a1 a2 Alt where
 transZM table a
  = case a of
	AAlt gs ss
	 -> do	gs'		<- mapM (transZM table) gs

	 	ss2		<- mapM (transZM table) ss
		ss3		<- transSS table ss2

	 	return		$ AAlt gs' ss3

	ASwitch x ss
	 -> do	x'		<- transZM table x

	 	ss2		<- mapM (transZM table) ss
		ss3		<- transSS table ss2

		transA table	$ ASwitch x' ss3
		
	ACaseSusp x l
	 -> do	x'		<- transZM table x
		transA table	$ ACaseSusp x' l

	ACaseDeath
	 ->	transA table 	$ ACaseDeath	 
	 
	ADefault ss
	 -> do	ss2		<- mapM (transZM table) ss
	 	ss3		<- transSS table ss2
	 	transA table	$ ADefault ss3
	
-- Guard ---------------------------------------------------------------------------------------------
instance Monad m => TransM m a1 a2 Guard where
 transZM table a
  = case a of
  	GCase b ss x1 x2
	 -> do	ss2		<- mapM (transZM table) ss
		ss3		<- transSS table ss2

	 	x1'		<- transZM table x1
		x2'		<- transZM table x2
		return		$ GCase b ss3 x1' x2'
		
	 
		
