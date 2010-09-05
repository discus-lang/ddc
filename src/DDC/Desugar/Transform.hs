{-# OPTIONS -fwarn-incomplete-patterns -fwarn-unused-matches -fwarn-name-shadowing #-}

-- | Generic transforms on the Desugared IR.
module DDC.Desugar.Transform
	( TransM(..)
	, TransTable(..)
	, transTableId
	, transformN
	, transformW
	, transformXM
	, transZ)
where
import DDC.Desugar.Exp
import DDC.Type.Exp
import DDC.Type.Data
import DDC.Var
import Data.Traversable
import Prelude				hiding (mapM)
import Control.Monad.State.Strict	hiding (mapM, forM)
import Util.Control.Monad
import Util.Data.List

-- | Monadic transforms of desugared expressions.
--   They can also change the type of the annotation along the way.
class Monad m => TransM m a1 a2 exp where
 transZM
 	:: TransTable m a1 a2
 	-> exp a1 -> m (exp a2)
	
	
-- | Apply a transform in the identity monad.
transZ 	:: TransM (State ()) a1 a2 exp
	=> TransTable (State ()) a1 a2
	-> exp a1 -> exp a2
	
transZ	table x 
	= evalState (transZM table x) ()


-- | Table containing fns to apply at each node in the AST.
data TransTable m a1 a2
	= TransTable
	{ -- | Transform to apply to variables
	  transV	:: Var 		-> m Var

	  -- | Transform to apply to annotations
	, transN	:: a1		-> m a2

	  -- | Transform to apply to top level things
	, transP	:: Top  a2	-> m (Top  a2)

	  -- | Transform to apply to projections
	, transJ	:: Proj	a2	-> m (Proj a2)

	  -- | Transform to apply to patterns
	, transW	:: Pat  a2	-> m (Pat  a2) 

	  -- | Transform to apply to types
	, transT	:: Type		-> m Type

	  -- | Transform to apply to statements.
	  --   The default behavior is to apply the 'follow' transfrom to each 
	  --   of the children then apply the 'leave' in a bottom-up manner.
	, transS	:: TransTable m a1 a2 -> Stmt a1 -> m (Stmt a2)
	, transS_follow	:: TransTable m a1 a2 -> Stmt a1 -> m (Stmt a2)
	, transS_leave	:: Stmt a2	-> m (Stmt a2)
	, transSS	:: [Stmt a2]	-> m [Stmt a2] 

	  -- | Transform to apply to expressions.
	  --   The default behavior is to apply the 'enter' transform to the whole node, 
	  --   then the 'follow' transform to each of the children, 
	  --   then the 'leave' transform in a bottom-up manner.
	, transX	:: TransTable m a1 a2 -> Exp a1	 -> m (Exp a2)
	, transX_follow	:: TransTable m a1 a2 -> Exp a1  -> m (Exp a2)
	, transX_enter	:: Exp a1	-> m (Exp a1)
	, transX_leave	:: Exp a2	-> m (Exp a2)
	
	  -- | Transform to apply to alternatives.
	, transA	:: TransTable m a1 a2 -> Alt a1	-> m (Alt  a2) 
	, transA_follow	:: TransTable m a1 a2 -> Alt a1	-> m (Alt  a2) 
	}

	
-- | Identity transform that just returns the original expression.
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
	, transS_follow	= \table x 	-> followS table x
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
	

-- Common Transforms ------------------------------------------------------------------------------
-- | Apply a transform to the annotations only.
transformN f t
	= transZ (transTableId (\x -> return $ f x)) t

-- | Apply a transform to all the patterns in an expression.
transformW f xx 
	= transZ ((transTableId return) { transW = \w -> return $ f w }) xx

-- | Apply a monadic transform to all the expressions in a thing, 
--   in a bottom-up manner.
transformXM f xx
	= transZM ((transTableId return) { transX_leave = \x -> f x }) xx


-- Top --------------------------------------------------------------------------------------------
instance Monad m => TransM m a1 a2 Top where
 transZM table pp
  = case pp of
	PImport nn ms
	 -> liftM2 PImport (transN table nn) (return ms)
	 >>= transP table
	
	PExtern nn v tv mto
	 -> liftM4 PExtern 	(transN table nn) (transV table v) 
				(transT table tv) (mLiftM (transT table) mto)
	 >>= transP table
		
	PRegion nn v
	 ->  liftM2 PRegion	(transN table nn) (transV table v)	
	 >>= transP table
	
	PKindSig nn v k
	 ->  liftM3 PKindSig	(transN table nn) (transV table v) (return k)
	 >>= transP table
	
	PTypeSynonym nn v t
	 ->  liftM3 PTypeSynonym (transN table nn) (transV table v) (transT table t)
	 >>= transP table
	
	PData nn def
	 ->  liftM2 PData	(transN table nn) (transDataDef table def)
	 >>= transP table
	
	PSuperSig nn v k
	 ->  liftM3 PSuperSig	(transN table nn) (transV table v) (return k)
	 >>= transP table

	PClassDecl nn v ts sigs
	 ->  liftM4 PClassDecl	(transN table nn) (transV table v) (mapM (transT table) ts)
				(mapM (t22LiftM $ transT table) sigs)
	 >>= transP table

	PClassInst nn v ts ss
	 ->  liftM4 PClassInst 	(transN table nn) (transV table v)
				(mapM (transT table) ts) (mapM (transZM table) ss)
	 >>= transP table
	
	PProjDict nn t ss
	 ->  liftM3 PProjDict	(transN table nn) (transT table t) (mapM (transZM table) ss)
	 >>= transP table

	PTypeSig nn vs t
	 ->  liftM3 PTypeSig	(transN table nn) (mapM (transV table) vs) (transT table t)
	 >>= transP table
		 
	PBind nn mV x
	 ->  liftM3 PBind	(transN table nn) (mLiftM (transV table) mV) (transZM table x)
	 >>= transP table
	

-- Data Def ---------------------------------------------------------------------------------------
transDataDef
	:: Monad m
	=> TransTable m a1 a2
	-> DataDef -> m DataDef

transDataDef table
	def@(DataDef	{ dataDefName	= name
			, dataDefParams	= vksParams
			, dataDefCtors	= ctors })

 = do	name'		<- transV table name

	let (vsParams, ksParams)
			= unzip vksParams

	vsParams'	<- mapM (transV table) vsParams
	let vksParams'	= zip vsParams' ksParams
		
	ctors'		<- forM ctors (transCtorDef table)
	return		$ def 	{ dataDefName	= name'
				, dataDefParams	= vksParams'
				, dataDefCtors	= ctors' }


-- CtorDef ----------------------------------------------------------------------------------------
transCtorDef 
	:: Monad m 
	=> TransTable m a1 a2
	-> CtorDef -> m CtorDef 

transCtorDef table 
	def@(CtorDef	{ ctorDefName	= name
		 	, ctorDefType	= t })

 = do	name'	<- transV table name
	t'	<- transT table t
	return	$ def 	{ ctorDefName 	= name'
			, ctorDefType	= t' }
		


-- Exp --------------------------------------------------------------------------------------------
instance Monad m => TransM m a1 a2 Exp where
 transZM table xx
  = transX table table xx

transX_default table x
 = do	xE	<- transX_enter  table x
	xF	<- transX_follow table table xE
	xL	<- transX_leave  table xF
	return	xL

followX	:: Monad m => TransTable m a1 a2 -> Exp a1  -> m (Exp a2)
followX table xx
  = case xx of
  	XNil
	 -> return XNil
	 
	XVoid    nn	
	 -> liftM  XVoid	(transN table nn)

	XLit     nn l	
	 -> liftM2 XLit 	(transN table nn) (return l)	

	XVar     nn v
	 -> liftM2 XVar		(transN table nn) (transV table v)

	XProj    nn x j	
	 -> liftM3 XProj	(transN table nn) (transZM table x) (transZM table j)

	XProjT   nn t j	
	 -> liftM3 XProjT	(transN table nn) (transT table t)  (transZM table j)	

	XLambda  nn v x	
	 -> liftM3 XLambda	(transN table nn) (transV table v) (transZM table x)	

	XApp nn x1 x2	
	 -> liftM3 XApp		(transN table nn) (transZM table x1) (transZM table x2)

	XMatch nn x1 aa
	 -> liftM3 XMatch	(transN table nn) (mLiftM (transZM table) x1) 
			  	(mapM (transZM table) aa)
	XDo nn ss	
	 -> liftM2 XDo		(transN table nn) (mapM (transZM table) ss >>= transSS table)
		
	XIfThenElse nn x1 x2 x3
	 -> liftM4 XIfThenElse	(transN table nn)  (transZM table x1)
				(transZM table x2) (transZM table x3)
	
	XLambdaTEC nn v x1 t eff clo
	 -> liftM6 XLambdaTEC	(transN table nn)  (transV table v)   (transZM table x1)
				(transT table t)   (transT table eff) (transT table clo)
	
	XProjTagged nn vI tC x j
	 -> liftM5 XProjTagged	(transN table nn)  (transV table vI) (transT table tC)
				(transZM table x)  (transZM table j)

	XProjTaggedT nn vI tC j
	 -> liftM4 XProjTaggedT	(transN table nn) (transV table vI) (transT table tC)
				(transZM table j)

	XVarInst nn v
	 -> liftM2 XVarInst	(transN table nn) (transV table v)
		

-- Proj ---------------------------------------------------------------------------------------------
instance Monad m => TransM m a1 a2 Proj where
 transZM table xx
  = case xx of
  	JField  nn v	
	 -> liftM2 JField  	(transN table nn) (transV table v) >>= transJ table		

	JFieldR nn v	
	 -> liftM2 JFieldR 	(transN table nn) (transV table v) >>= transJ table


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
	 -> liftM3 SSig		(transN table nn) (mapM (transV table) vs) (transT table t)

  	SBind nn mV x
	 -> liftM3 SBind	(transN table nn) (mLiftM (transV table) mV) (transZM table x)
	
	SBindPat nn pat x	
	 -> liftM3 SBindPat	(transN table nn) (transZM table pat) (transZM table x)
		
	SBindMonadic nn w x
	 -> liftM3 SBindMonadic	(transN table nn) (transZM table w) (transZM table x)
				

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
	 -> liftM3 AAlt 	(transN table nn) (mapM (transZM table) gs) (transZM table x)
	

-- Guard ------------------------------------------------------------------------------------------
instance Monad m => TransM m a1 a2 Guard where
 transZM table xx
  = case xx of
  	GCase nn w
	 -> liftM2 GCase	(transN table nn) (transZM table w)
		
	GExp nn w x
	 -> liftM3 GExp		(transN table nn) (transZM table w) (transZM table x)
			
	
-- Pat --------------------------------------------------------------------------------------------
instance Monad m => TransM m a1 a2 Pat where
 transZM table ww
  = case ww of
  	WConLabel nn v lvs
 	 ->  liftM3 WConLabel	(transN table nn) (transV table v)
				(mapM (ttLiftM (transZM table) (transV table)) lvs)
	 >>= transW table
		
	WLit nn c
	 ->  liftM2 WLit	(transN table nn) (return c)
	 >>= transW table

	WVar nn v
	 -> liftM2 WVar		(transN table nn) (transV table v)
	 >>= transW table
	
	WAt nn v p
	 -> liftM3 WAt		(transN table nn) (transV table v) (transZM table p)
	 >>= transW table
		
	WConLabelP nn v lps
	 -> liftM3 WConLabelP	(transN table nn) (transV table v) 
				(mapZippedM (transZM table) (transZM table) lps)
	 >>= transW table


-- Label ------------------------------------------------------------------------------------------
instance Monad m => TransM m a1 a2 Label where
 transZM table ll
  = case ll of
  	LIndex nn i	-> liftM2 LIndex (transN table nn) (return i)
	LVar   nn v	-> liftM2 LVar   (transN table nn) (transV table v)

