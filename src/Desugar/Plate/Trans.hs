{-# OPTIONS -fwarn-incomplete-patterns #-}

module Desugar.Plate.Trans
	( TransM(..)
	, TransTable(..)
	, transTableId
	, transformN
	, transformW
	, transformXM
	, transZ)
where
import Desugar.Exp
import Shared.Exp
import Type.Exp
import Util
import DDC.Var


-----
class Monad m => TransM m a1 a2 exp where
 transZM
 	:: TransTable m a1 a2
 	-> exp a1 -> m (exp a2)
	
	
-----
transZ 	:: TransM (State ()) a1 a2 exp
	=> TransTable (State ()) a1 a2
	-> exp a1 -> exp a2
	
transZ	table x 
	= evalState (transZM table x) ()


-----
data TransTable m a1 a2
	= TransTable
	{ transV	:: Var 		-> m Var
	, transN	:: a1		-> m a2

	, transP	:: Top  a2	-> m (Top  a2)
	, transJ	:: Proj	a2	-> m (Proj a2)
	, transW	:: Pat  a2	-> m (Pat  a2) 
	, transT	:: Type		-> m Type

	-- stmt
	, transS	:: TransTable m a1 a2 -> Stmt a1 -> m (Stmt a2)
	, transS_follow	:: TransTable m a1 a2 -> Stmt a1 -> m (Stmt a2)
	, transS_leave	:: Stmt a2	-> m (Stmt a2)
	, transSS	:: [Stmt a2]	-> m [Stmt a2] 

	-- exp
	, transX	:: TransTable m a1 a2 -> Exp a1	 -> m (Exp a2)
	, transX_follow	:: TransTable m a1 a2 -> Exp a1  -> m (Exp a2)
	, transX_enter	:: Exp a1	-> m (Exp a1)
	, transX_leave	:: Exp a2	-> m (Exp a2)
	
	-- alt
	, transA	:: TransTable m a1 a2 -> Alt a1	-> m (Alt  a2) 
	, transA_follow	:: TransTable m a1 a2 -> Alt a1	-> m (Alt  a2) 
	}

	
-- | Identity transform, does nothing.
transTableId 
	:: (a1 -> (State s) a2) 
	-> TransTable (State s) a1 a2

transTableId transN'
	= TransTable
	{ transV	= \x -> return x
	, transN	= transN'

	, transP	= \x -> return x 
	, transJ	= \x -> return x
	, transW	= \x -> return x 
	, transT	= \x -> return x

	-- stmt
	, transS	= transS_default
	, transS_follow	= \table x	-> followS table x
	, transS_leave	= \x -> return x

	, transSS	= \x -> return x 
	
	-- exp
	, transX	= transX_default
	, transX_enter	= \x 		-> return x
	, transX_follow	= \table x	-> followX table x
	, transX_leave	= \x 		-> return x	
	
	-- alt
	, transA	= transA_default
	, transA_follow	= \table x	-> followA table x
	
	}
	

-----------------------
transformN f t		= transZ (transTableId (\x -> return $ f x)) t
transformW f xx  	= transZ ((transTableId return) { transW = \w -> return $ f w }) xx

transformXM f xx	= transZM ((transTableId return) { transX_leave = \x -> f x }) xx


-----------------------
instance Monad m => TransM m a1 a2 Top where
 transZM table pp
  = case pp of
  	PNil
	 ->	transP table	$ PNil

	PImport nn ms
	 -> do 	nn'		<- transN	table nn
	 	transP table	$ PImport nn' ms

	PExtern nn v tv mto
	 -> do	nn'		<- transN  table nn
	 	v'		<- transV  table v
		tv'		<- transT  table tv

		mto'		<- case mto of
					Just to	-> do	to'	<- transT  table to
							return	$ Just to'
					_	-> return $ Nothing
							
		transP table	$ PExtern nn' v' tv' mto'

	PExternData nn s v k
	 -> do	nn'		<- transN table nn
		v'		<- transV table v
		transP table	$ PExternData nn' s v' k

	PEffect nn v k
	 -> do	nn'		<- transN	table nn
	 	transP table	$ PEffect nn' v k
		
	PRegion nn v
	 -> do	nn'		<- transN	table nn
	 	transP table	$ PRegion nn' v

	PKindSig nn v k
	 -> do	nn'		<- transN 	table nn
	 	v'		<- transV table v
		transP table	$ PKindSig nn' v' k

	PTypeSynonym nn v t
	 -> do	nn'		<- transN 	table nn
	 	v'		<- transV table v
		transP table	$ PTypeSynonym nn' v' t

	PData nn v vs ctors
	 -> do	nn'		<- transN	table nn
	 	(v':vs')	<- mapM (transV table) (v:vs)
		ctors'		<- mapM (transZM table) ctors
		transP table	$ PData nn' v' vs' ctors'

	PClass nn v k
	 -> do	nn'		<- transN 	table nn
	 	transP table	$ PClass nn' v k

	PClassDict nn v ts cs sigs
	 -> do	nn'		<- transN	table nn

		let (vsSig, tsSig)	= unzip sigs
		tsSig'			<- mapM (transT table) tsSig
		let sigs'		= zip vsSig tsSig'
		
		ts'		<- mapM (transT table) ts
		transP table	$ PClassDict nn' v ts' cs sigs'
	 	
	PClassInst nn v ts cs ss
	 -> do	nn'		<- transN	table nn
		ss'		<- mapM (transZM table) ss
		ts'		<- mapM (transT table) ts
		transP table	$ PClassInst nn' v ts' cs ss'

	PProjDict nn t ss
	 -> do	nn'		<- transN 	table nn
	 	ss'		<- mapM (transZM table) ss
		t'		<- transT table t
		transP table	$ PProjDict nn' t' ss'	

	PSig nn vs t
	 -> do	nn'		<- transN	table nn
	 	vs'		<- mapM (transV	table) vs
		t'		<- transT	table t
		transP table	$ PSig nn' vs' t'
		 
	PBind nn mV x
	 -> do	nn'		<- transN  	table nn
	 	mV'		<- liftMaybe 	(transV table) mV
		x'		<- transZM 	table x
		transP table	$ PBind nn' mV' x'


-- CtorDef ----------------------------------------------------------------------------------------
instance Monad m => TransM m a1 a2 CtorDef where
 transZM table xx
  = case xx of
  	CtorDef nn v fields
	 -> do	nn'		<- transN 	table nn
	 	v'		<- transV	table v
		fields'		<- mapM (transDataField table) fields
		return		$ CtorDef nn' v' fields'
		

-- DataField --------------------------------------------------------------------------------------
transDataField 
	:: (Monad m, TransM m a1 a2 Exp)
	=> TransTable m a1 a2
	-> DataField    (Exp a1) Type
	-> m (DataField (Exp a2) Type)

transDataField table xx
 = do	dInit'	<- case dInit xx of
 			Nothing	-> return Nothing
			Just x	-> do
				x'	<- transZM table x
				return	$ Just x'
 
	t'	<- transT table (dType xx)
 
 	return	$ DataField
		{ dPrimary	= dPrimary xx
		, dLabel	= dLabel   xx
		, dType		= t'
		, dInit		= dInit' }


-- Exp --------------------------------------------------------------------------------------------
instance Monad m => TransM m a1 a2 Exp where
 transZM table xx
  = transX table table xx

transX_default table x
 = do	xE	<- transX_enter  table x
	xF	<- transX_follow table table xE
	xL	<- transX_leave  table xF
	return	xL

followX table xx
  = case xx of
  	XNil
	 ->	return	$ XNil
	 
	XVoid nn
	 -> do	nn'	<- transN  table nn
	 	return	$ XVoid nn' 
		
	XLit nn l
	 -> do	nn'	<- transN  table nn
	 	return	$ XLit nn' l
		
	XVar nn v
	 -> do	nn'	<- transN  table nn
	 	v'	<- transV  table v
		return	$ XVar nn' v'

	XProj nn x j
	 -> do	nn'	<- transN  table nn
	 	x'	<- transZM table x
		j'	<- transZM table j
		return	$ XProj nn' x' j'

	XProjT nn t j
	 -> do	nn'	<- transN  table nn
		j'	<- transZM table j
		t'	<- transT  table t
		return	$ XProjT nn' t' j'
		
	XLambda nn v x
	 -> do	nn'	<- transN  table nn
	 	v'	<- transV  table v
		x'	<- transZM table x
		return	$ XLambda nn' v' x'
		
	XApp nn x1 x2
	 -> do	nn'	<- transN  table nn
	 	x1'	<- transZM table x1
		x2'	<- transZM table x2
		return	$ XApp nn' x1' x2'
	
	XMatch nn x1 aa
	 -> do	nn'	<- transN  table nn
	 	x1'	<- liftMaybe (transZM table) x1
		aa'	<- mapM  (transZM table) aa
		return	$ XMatch nn' x1' aa'
		
	XDo nn ss
	 -> do	nn'	<- transN  table nn
	 	ss'	<- mapM (transZM table) ss
		ss2	<- transSS table ss'
		return	$ XDo nn' ss2
		
	XIfThenElse nn x1 x2 x3
	 -> do	nn'	<- transN  table nn
	 	x1'	<- transZM table x1
		x2'	<- transZM table x2
		x3'	<- transZM table x3
		return	$ XIfThenElse nn' x1' x2' x3'


	XLambdaTEC nn v x1 t eff clo
	 -> do	nn'	<- transN table nn
		v'	<- transV table v
	 	x1'	<- transZM table x1
		return	$ XLambdaTEC nn' v' x1' t eff clo

	XProjTagged nn vI tC x j
	 -> do	nn'	<- transN table nn
	 	vI'	<- transV table vI
		tC'	<- transT table tC
		x'	<- transZM table x
		j'	<- transZM table j
		return	$ XProjTagged nn' vI' tC' x' j'

	XProjTaggedT nn vI tC j
	 -> do	nn'	<- transN table nn
	 	vI'	<- transV table vI
		tC'	<- transT table tC
		j'	<- transZM table j
		return	$ XProjTaggedT nn' vI' tC' j'

	XVarInst nn v
	 -> do	nn'	<- transN table nn
	 	v'	<- transV table v
		return	$ XVarInst nn' v'
		

-- Proj ---------------------------------------------------------------------------------------------
instance Monad m => TransM m a1 a2 Proj where
 transZM table xx
  = case xx of
  	JField nn v
	 -> do	nn'		<- transN  table nn
	 	v'		<- transV  table v
		transJ table	$ JField nn' v'
		
	JFieldR nn v
	 -> do	nn'		<- transN  table nn
	 	v'		<- transV  table v
		transJ table	$ JFieldR nn' v'
		

-- Stmt -------------------------------------------------------------------------------------------
instance Monad m => TransM m a1 a2 Stmt where
 transZM table s
  = transS table table s
 
transS_default table ss
 = do	ssF	<- transS_follow table table ss
	ssL	<- transS_leave table ssF
 	return	ssL
	
followS table xx
  = case xx of
	SSig nn vs t
	 -> do	nn'	<- transN  table nn
	 	vs'	<- mapM (transV  table) vs
		t'	<- transT table t
		return	$ SSig nn' vs' t'

  	SBind nn mV x
	 -> do	nn'	<- transN  	table nn
	 	mV'	<- liftMaybe 	(transV table) mV
		x'	<- transZM 	table x
		return	$ SBind nn' mV' x'

	SBindPat nn pat x
	 -> do	nn'	<- transN table nn
	 	pat'	<- transZM table pat
		x'	<- transZM table x
		return	$ SBindPat nn' pat' x'
		
	SBindMonadic nn w x
	 -> do	nn'	<- transN table nn
	 	w'	<- transZM table w
		x'	<- transZM table x
		return	$ SBindMonadic nn' w' x'
				

-- Alt --------------------------------------------------------------------------------------------
instance Monad m => TransM m a1 a2 Alt where
 transZM table xx
  = transA table table xx
  
transA_default table aa
 = do	aa'	<- transA_follow table table aa
 	return aa'

followA table xx
  = case xx of
	AAlt nn gs x
	 -> do	nn'		<- transN table nn
	 	gs'		<- mapM (transZM table) gs
		x'		<- transZM table x
		return		$ AAlt nn' gs' x'


-- Guard ------------------------------------------------------------------------------------------
instance Monad m => TransM m a1 a2 Guard where
 transZM table xx
  = case xx of
  	GCase nn w
	 -> do	nn'		<- transN table nn
	 	w'		<- transZM table w
		return		$ GCase nn' w'
		
	GExp nn w x
	 -> do	nn'		<- transN table nn
	 	w'		<- transZM table w
		x'		<- transZM table x
		return		$ GExp nn' w' x'
		
		
-- Pat --------------------------------------------------------------------------------------------
instance Monad m => TransM m a1 a2 Pat where
 transZM table ww
  = case ww of
  	WConLabel nn v lvs
	 -> do	nn'		<- transN table nn
	 	v'		<- transV table v

		let (ls, vs)	= unzip lvs
		ls'		<- mapM (transZM table) ls
		vs'		<- mapM (transV table) vs
		let lvs'	= zip ls' vs'
		
		transW table	$ WConLabel nn' v' lvs'
		
	WLit nn c
	 -> do	nn'		<- transN table nn
	 	transW table	$ WLit nn' c


	WVar nn v
	 -> do 	nn'		<- transN table nn
	 	v'		<- transV table v
	 	transW table	$ WVar nn' v'
		
	WAt nn v p
	 -> do	nn'		<- transN table nn
	 	v'		<- transV table v
		p'		<- transZM table p
		transW table	$ WAt nn' v' p'
		
	WConLabelP nn v lps
	 -> do	nn'		<- transN table nn
	 	v'		<- transV table v
		
		lps'		<- mapZippedM (transZM table) (transZM table) lps
		transW table	$ WConLabelP nn' v' lps'		 
	 

-- Label ------------------------------------------------------------------------------------------
instance Monad m => TransM m a1 a2 Label where
 transZM table ll
  = case ll of
  	LIndex nn i
	 -> do	nn'		<- transN table nn
	 	return		$ LIndex nn' i
		
	LVar nn v
	 -> do	nn'		<- transN table nn
	 	v'		<- transV table v
		return		$ LVar nn' v'		


