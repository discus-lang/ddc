{-# OPTIONS -fwarn-incomplete-patterns #-}

-- Sea.Plate.Trans
--	Transform over Sea.Exp
--
--	* Keep the order of constructors in the transform functions the same as in Sea.Exp.
--	  this makes it much easier to see if they're all present and accounted for.
--
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
import DDC.Sea.Exp
import Control.Monad.State.Strict


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

	PData v ctors
	 -> 	transP table	$ PData v ctors

	-- supers
	PProto v ts t
	 ->	transP table	$ PProto v ts t

 	PSuper v args t ss
	 -> do	ss2		<- mapM (transZM table) ss
		ss3		<- transSS table ss2
		transP table	$ PSuper v args t ss3

	-- cafs
	PCafProto v	t
	 ->	transP table	$ PCafProto v t

	PCafSlot v t
	 ->	transP table	$ PCafSlot v t

	PCafInit v t ss
	 -> do	ss2		<- mapM (transZM table) ss
	 	ss3		<- transSS table ss2
		transP table	$ PCafInit v t ss3

	-- hackery
	PHashDef s1 s2
	 ->	transP table	$ PHashDef s1 s2

	PInclude s
	 ->	transP table	$ PInclude s

	PIncludeAbs s
	 ->	transP table	$ PIncludeAbs s

	PHackery s
	 ->	transP table	$ PHackery s

	PMain mn ml
	 ->	transP table	$ PMain mn ml

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

	SIf x ssThen
	 -> do	x'		<- transZM table x

		ssThen2		<- mapM (transZM table) ssThen
		ssThen3		<- transSS table ssThen2

		transS table	$ SIf x' ssThen3

	SCaseFail
	 ->	transS table	$ SCaseFail


-- Name --------------------------------------------------------------------------------------------
transName table name
 = case name of
	NRts	v	-> liftM  NRts    (transV table v)
	NSuper	v	-> liftM  NSuper  (transV table v)
	NAuto	v	-> liftM  NAuto   (transV table v)
	NSlot	v i	-> liftM2 NSlot   (transV table v) (return i)
	NCafPtr v	-> liftM  NCafPtr (transV table v)
	NCaf	v	-> liftM  NCaf    (transV table v)


-- Exp ---------------------------------------------------------------------------------------------
instance Monad m => TransM m a1 a2 Exp where
 transZM table x
  = case x of
 	XNil
	 -> 	transX table 	$ XNil

	XVar n t
	 -> do 	n'		<- transName table n
		transX table	$ XVar n' t

	XPrim v xs
	 -> do	xs'		<- mapM (transZM table) xs
	 	transX table	$ XPrim v xs'

	-- projection
	XArg x i
	 -> do	x'		<- transZM table x
		transX table	$ XArg x' i

	XTag x
	 -> do	x'		<- transZM table x
	 	transX table	$ XTag x'

	-- constants
	XLit (LDataTag v)
	 -> do	v'		<- transV table v
	 	transX table	$ XLit (LDataTag v')

	XLit lit
	 -> 	transX table	$ XLit lit



-- Alt ---------------------------------------------------------------------------------------------
instance Monad m => TransM m a1 a2 Alt where
 transZM table a
  = case a of
	AAlt gs ss
	 -> do	gs'		<- mapM (transZM table) gs

	 	ss2		<- mapM (transZM table) ss
		ss3		<- transSS table ss2

	 	transA table	$ AAlt gs' ss3

	ASwitch x ss
	 -> do	x'		<- transZM table x

	 	ss2		<- mapM (transZM table) ss
		ss3		<- transSS table ss2

		transA table	$ ASwitch x' ss3

	ACaseSusp x l
	 -> do	x'		<- transZM table x
		transA table	$ ACaseSusp x' l

	ACaseIndir x l
	 -> do	x'		<- transZM table x
		transA table	$ ACaseIndir x' l

	ACaseDeath sp
	 ->	transA table 	$ ACaseDeath sp

	ADefault ss
	 -> do	ss2		<- mapM (transZM table) ss
	 	ss3		<- transSS table ss2
	 	transA table	$ ADefault ss3


-- Guard ---------------------------------------------------------------------------------------------
instance Monad m => TransM m a1 a2 Guard where
 transZM table a
  = case a of
  	GCase sp b ss x1 x2
	 -> do	ss2		<- mapM (transZM table) ss
		ss3		<- transSS table ss2

	 	x1'		<- transZM table x1
		x2'		<- transZM table x2
		return		$ GCase sp b ss3 x1' x2'



