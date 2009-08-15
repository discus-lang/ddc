{-# OPTIONS -fwarn-incomplete-patterns #-}

module Source.Plate.Trans
	( TransTable(..)
	, transTableId
	, transZM 
	, trans
	
	, Trans
	, Trans1 )

where

import Util
import qualified Shared.Var	as Var
import Source.Exp
import Debug.Trace

-- | The transform class.
--	It transforms some thing 'a' into some other thing 'b'.
--	The n1 and n2 are optional annotations sprinkled over those things.
--
class Monad m => TransM m n1 n2 a b
 where	
 	-- apply all the three transforms
 	transZM		:: TransTable m n1 n2 -> a -> m b

	-- decend into the expression
	followZM 	:: TransTable m n1 n2 -> a -> m b	
	followZM	= error "followZM: undefined"


-- Apply the various transforms
--	first apply the in-place top-down transform
--	then decend with the follow function
--	then apply the in-place bottom-up transform
--	
transMe
	:: Monad m 
	=> (TransTable m n1 n2 -> x n1 -> m (x n2))
	-> (x n1 -> m (x n1))
	-> (x n2 -> m (x n2))
	-> TransTable m n1 n2
	-> x n1
	-> m (x n2)
	
transMe pTrans pEnter pLeave table xx
 = do	xxE	<- pEnter xx
	xxF	<- (pTrans table) xxE
	xxL	<- pLeave xxF
	return	$ xxL
	

type Trans  m n1 n2 x	= TransTable m n1 n2 -> x n1 -> m (x n2)
type Trans1 m n1 x	= x n1 -> m (x n1)

-- Basic Instances ---------------------------------------------------------------------------------

-- maybe
instance(  Monad  m
	,  TransM m n1 n2 a b)
	=> TransM m n1 n2 (Maybe a) (Maybe b)

 where 	transZM table Nothing	
   	 = 	return Nothing

 	transZM table (Just x)
	 = do	x'	<- transZM table x
	 	return	$ Just x'

-- lists
instance(  Monad  m
	,  TransM m n1 n2 a b)
	=> TransM m n1 n2 [a] [b]

 where	transZM table xx	
 	 = mapM (transZM table) xx

-- tuples
instance(  Monad  m
	,  TransM m n1 n2 a1 a2
	,  TransM m n1 n2 b1 b2)
	=> TransM m n1 n2 (a1, b1) (a2, b2)

 where	transZM table (a, b)
 	 = do 	a'		<- transZM table a
		b'		<- transZM table b
		return		(a', b')

-----
instance Monad m 
	=> TransM m n1 n2 (DataField (Exp n1) Type) (DataField (Exp n2) Type) where

 transZM table ff
  = case ff of
  	DataField{}
	 -> do	dLabel'		<- transZM table $ dLabel ff
		dInit'		<- transZM table $ dInit ff
		return		$ ff 	{ dLabel	= dLabel'
					, dInit		= dInit' }

instance Monad m => TransM m n1 n2 Var Var
 where	transZM table xx = (transVar table) xx

instance Monad m => TransM m n1 n2 Type Type
 where	transZM table xx = (transType table) xx

-- Identity Instances ------------------------------------------------------------------------------
-- We don't do anything special with these elements of the tree

instance Monad m => TransM m n1 n2 Int Int
 where	transZM table xx = return xx

instance Monad m => TransM m n1 n2 Bool Bool
 where	transZM table xx = return xx

instance Monad m => TransM m n1 n2 Var.Module Var.Module
 where	transZM table xx = return xx

instance Monad m => TransM m n1 n2 LiteralFmt LiteralFmt
 where	transZM table xx = return xx
 
instance Monad m => TransM m n1 n2 Kind Kind
 where	transZM table xx = return xx

instance Monad m => TransM m n1 n2 Char Char
 where 	transZM table xx = return xx


-- Helper functions --------------------------------------------------------------------------------
-- | Transform some thing with the identity monad
trans	:: TransM (State ()) n1 n2 a b
	=> TransTable (State ()) n1 n2
	-> a -> b
	
trans table x 
	= evalState (transZM table x) ()


-- The transform table -----------------------------------------------------------------------------

-- | Table of transform functions
data TransTable m n1 n2
	= TransTable
	{ transN		:: n1		-> m n2

	, transVar		:: Var  -> m Var
	, transType		:: Type -> m Type

	-- top
	, transTop		:: Trans m n1 n2 Top
	, transTop_enter	:: Trans1 m n1 Top
	, transTop_leave	:: Trans1 m n2 Top 

	-- export
	, transExport		:: Trans m n1 n2 Export
	, transExport_enter	:: Trans1 m n1 Export
	, transExport_leave	:: Trans1 m n2 Export

	-- foreign
	, transForeign		:: Trans m n1 n2 Foreign
	, transForeign_enter	:: Trans1 m n1 Foreign
	, transForeign_leave	:: Trans1 m n2 Foreign

	-- infixmode
	, transInfixMode	:: Trans m n1 n2 InfixMode
	, transInfixMode_enter	:: Trans1 m n1 InfixMode
	, transInfixMode_leave	:: Trans1 m n2 InfixMode


	-- stmt
	, transStmt		:: Trans m n1 n2 Stmt
	, transStmt_enter	:: Trans1 m n1 Stmt
	, transStmt_leave	:: Trans1 m n2 Stmt 
	
	-- exp
	, transExp		:: Trans m n1 n2 Exp
	, transExp_enter	:: Trans1 m n1 Exp
	, transExp_leave	:: Trans1 m n2 Exp 

	-- proj
	, transProj		:: Trans m n1 n2 Proj
	, transProj_enter	:: Trans1 m n1 Proj
	, transProj_leave	:: Trans1 m n2 Proj 

	-- alt
	, transAlt		:: Trans m n1 n2 Alt
	, transAlt_enter	:: Trans1 m n1 Alt
	, transAlt_leave	:: Trans1 m n2 Alt 

	-- guard
	, transGuard		:: Trans m n1 n2 Guard
	, transGuard_enter	:: Trans1 m n1 Guard
	, transGuard_leave	:: Trans1 m n2 Guard 
	
	-- pat
	, transPat		:: Trans m n1 n2 Pat
	, transPat_enter	:: Trans1 m n1 Pat
	, transPat_leave	:: Trans1 m n2 Pat

	-- label
	, transLabel		:: Trans m n1 n2 Label
	, transLabel_enter	:: Trans1 m n1 Label
	, transLabel_leave	:: Trans1 m n2 Label 

	-- LCQual
	, transLCQual		:: Trans m n1 n2 LCQual
	, transLCQual_enter	:: Trans1 m n1 LCQual
	, transLCQual_leave	:: Trans1 m n2 LCQual 

	}


-- | Zero transform table

-- this is the sig, but GHC won't take it
transTableId 
	:: forall a b n1 n2 (m :: * -> *)
	.  Monad m
	=>  (n1 -> m n2) 
	-> TransTable m n1 n2

transTableId transN'
	= TransTable
	{ transN		= transN'

	, transVar		= \x -> return x
	, transType		= \x -> return x
	
	, transTop		= followZM
	, transTop_enter	= \x -> return x
	, transTop_leave	= \x -> return x 

	, transExport		= followZM
	, transExport_enter	= \x -> return x
	, transExport_leave	= \x -> return x 

	, transForeign		= followZM
	, transForeign_enter	= \x -> return x
	, transForeign_leave	= \x -> return x 

	, transInfixMode		= followZM
	, transInfixMode_enter	= \x -> return x
	, transInfixMode_leave	= \x -> return x 
	
	, transExp		= followZM
	, transExp_enter	= \x -> return x
	, transExp_leave	= \x -> return x 
	
	, transProj		= followZM
	, transProj_enter	= \x -> return x
	, transProj_leave	= \x -> return x 

	, transStmt		= followZM
	, transStmt_enter	= \x -> return x
	, transStmt_leave	= \x -> return x 
		
	, transAlt		= followZM
	, transAlt_enter	= \x -> return x
	, transAlt_leave	= \x -> return x 

	, transGuard		= followZM
	, transGuard_enter	= \x -> return x
	, transGuard_leave	= \x -> return x 

	, transPat		= followZM
	, transPat_enter	= \x -> return x
	, transPat_leave	= \x -> return x 

	, transLabel		= followZM
	, transLabel_enter	= \x -> return x
	, transLabel_leave	= \x -> return x 

	, transLCQual		= followZM
	, transLCQual_enter	= \x -> return x
	, transLCQual_leave	= \x -> return x  }




 
instance (Monad m) => TransM m n1 n2 (Top n1) (Top n2) where
        transZM table xx
          = transMe (transTop table) (transTop_enter table)
              (transTop_leave table)
              table
              xx
        followZM table xx
          = case xx of
                PPragma x0 x1
                  -> do x0' <- transN table x0
                        x1' <- transZM table x1
                        return (PPragma x0' x1')
                PModule x0 x1
                  -> do x0' <- transN table x0
                        x1' <- transZM table x1
                        return (PModule x0' x1')
                PInfix x0 x1 x2 x3
                  -> do x0' <- transN table x0
                        x1' <- transZM table x1
                        x2' <- transZM table x2
                        x3' <- transZM table x3
                        return (PInfix x0' x1' x2' x3')
                PImportModule x0 x1
                  -> do x0' <- transN table x0
                        x1' <- transZM table x1
                        return (PImportModule x0' x1')
                PExport x0 x1
                  -> do x0' <- transN table x0
                        x1' <- transZM table x1
                        return (PExport x0' x1')
                PForeign x0 x1
                  -> do x0' <- transN table x0
                        x1' <- transZM table x1
                        return (PForeign x0' x1')
                PTypeSynonym x0 x1 x2
                  -> do x0' <- transN table x0
                        x1' <- transZM table x1
                        x2' <- transZM table x2
                        return (PTypeSynonym x0' x1' x2')
                PTypeKind x0 x1 x2
                  -> do x0' <- transN table x0
                        x1' <- transZM table x1
                        x2' <- transZM table x2
                        return (PTypeKind x0' x1' x2')
                PData x0 x1 x2 x3
                  -> do x0' <- transN table x0
                        x1' <- transZM table x1
                        x2' <- transZM table x2
                        x3' <- transZM table x3
                        return (PData x0' x1' x2' x3')
                PEffect x0 x1 x2
                  -> do x0' <- transN table x0
                        x1' <- transZM table x1
                        x2' <- transZM table x2
                        return (PEffect x0' x1' x2')
                PRegion x0 x1
                  -> do x0' <- transN table x0
                        x1' <- transZM table x1
                        return (PRegion x0' x1')
                PClass x0 x1 x2
                  -> do x0' <- transN table x0
                        x1' <- transZM table x1
                        x2' <- transZM table x2
                        return (PClass x0' x1' x2')
                PClassDict x0 x1 x2 x3 x4
                  -> do x0' <- transN table x0
                        x1' <- transZM table x1
                        x2' <- transZM table x2
                        x3' <- transZM table x3
                        x4' <- transZM table x4
                        return (PClassDict x0' x1' x2' x3' x4')
                PClassInst x0 x1 x2 x3 x4
                  -> do x0' <- transN table x0
                        x1' <- transZM table x1
                        x2' <- transZM table x2
                        x3' <- transZM table x3
                        x4' <- transZM table x4
                        return (PClassInst x0' x1' x2' x3' x4')
                PProjDict x0 x1 x2
                  -> do x0' <- transN table x0
                        x1' <- transZM table x1
                        x2' <- transZM table x2
                        return (PProjDict x0' x1' x2')
                PStmt x0
                  -> do x0' <- transZM table x0
                        return (PStmt x0')
 
instance (Monad m) => TransM m n1 n2 (Export n1) (Export n2) where
        transZM table xx
          = transMe (transExport table) (transExport_enter table)
              (transExport_leave table)
              table
              xx
        followZM table xx
          = case xx of
                EValue x0 x1
                  -> do x0' <- transN table x0
                        x1' <- transZM table x1
                        return (EValue x0' x1')
                EType x0 x1
                  -> do x0' <- transN table x0
                        x1' <- transZM table x1
                        return (EType x0' x1')
                ERegion x0 x1
                  -> do x0' <- transN table x0
                        x1' <- transZM table x1
                        return (ERegion x0' x1')
                EEffect x0 x1
                  -> do x0' <- transN table x0
                        x1' <- transZM table x1
                        return (EEffect x0' x1')
                EClass x0 x1
                  -> do x0' <- transN table x0
                        x1' <- transZM table x1
                        return (EClass x0' x1')
 
instance (Monad m) => TransM m n1 n2 (Foreign n1) (Foreign n2)
         where
        transZM table xx
          = transMe (transForeign table) (transForeign_enter table)
              (transForeign_leave table)
              table
              xx
        followZM table xx
          = case xx of
                OImport x0 x1 x2 x3
                  -> do x0' <- transZM table x0
                        x1' <- transZM table x1
                        x2' <- transZM table x2
                        x3' <- transZM table x3
                        return (OImport x0' x1' x2' x3')
                OImportUnboxedData x0 x1 x2
                  -> do x0' <- transZM table x0
                        x1' <- transZM table x1
                        x2' <- transZM table x2
                        return (OImportUnboxedData x0' x1' x2')
 
instance (Monad m) => TransM m n1 n2 (InfixMode n1) (InfixMode n2)
         where
        transZM table xx
          = transMe (transInfixMode table) (transInfixMode_enter table)
              (transInfixMode_leave table)
              table
              xx
        followZM table xx
          = case xx of
                InfixLeft -> do return (InfixLeft)
                InfixRight -> do return (InfixRight)
                InfixNone -> do return (InfixNone)
                InfixSuspend -> do return (InfixSuspend)
 
instance (Monad m) => TransM m n1 n2 (Exp n1) (Exp n2) where
        transZM table xx
          = transMe (transExp table) (transExp_enter table)
              (transExp_leave table)
              table
              xx
        followZM table xx
          = case xx of
                XNil -> do return (XNil)
                XLit x0 x1
                  -> do x0' <- transN table x0
                        x1' <- transZM table x1
                        return (XLit x0' x1')
                XVar x0 x1
                  -> do x0' <- transN table x0
                        x1' <- transZM table x1
                        return (XVar x0' x1')
                XObjField x0 x1
                  -> do x0' <- transN table x0
                        x1' <- transZM table x1
                        return (XObjField x0' x1')
                XProj x0 x1 x2
                  -> do x0' <- transN table x0
                        x1' <- transZM table x1
                        x2' <- transZM table x2
                        return (XProj x0' x1' x2')
                XProjT x0 x1 x2
                  -> do x0' <- transN table x0
                        x1' <- transZM table x1
                        x2' <- transZM table x2
                        return (XProjT x0' x1' x2')
                XLambdaPats x0 x1 x2
                  -> do x0' <- transN table x0
                        x1' <- transZM table x1
                        x2' <- transZM table x2
                        return (XLambdaPats x0' x1' x2')
                XLambdaProj x0 x1 x2
                  -> do x0' <- transN table x0
                        x1' <- transZM table x1
                        x2' <- transZM table x2
                        return (XLambdaProj x0' x1' x2')
                XLambdaCase x0 x1
                  -> do x0' <- transN table x0
                        x1' <- transZM table x1
                        return (XLambdaCase x0' x1')
                XCase x0 x1 x2
                  -> do x0' <- transN table x0
                        x1' <- transZM table x1
                        x2' <- transZM table x2
                        return (XCase x0' x1' x2')
                XMatch x0 x1
                  -> do x0' <- transN table x0
                        x1' <- transZM table x1
                        return (XMatch x0' x1')
                XDo x0 x1
                  -> do x0' <- transN table x0
                        x1' <- transZM table x1
                        return (XDo x0' x1')
                XLet x0 x1 x2
                  -> do x0' <- transN table x0
                        x1' <- transZM table x1
                        x2' <- transZM table x2
                        return (XLet x0' x1' x2')
                XIfThenElse x0 x1 x2 x3
                  -> do x0' <- transN table x0
                        x1' <- transZM table x1
                        x2' <- transZM table x2
                        x3' <- transZM table x3
                        return (XIfThenElse x0' x1' x2' x3')
                XTry x0 x1 x2 x3
                  -> do x0' <- transN table x0
                        x1' <- transZM table x1
                        x2' <- transZM table x2
                        x3' <- transZM table x3
                        return (XTry x0' x1' x2' x3')
                XThrow x0 x1
                  -> do x0' <- transN table x0
                        x1' <- transZM table x1
                        return (XThrow x0' x1')
                XWhere x0 x1 x2
                  -> do x0' <- transN table x0
                        x1' <- transZM table x1
                        x2' <- transZM table x2
                        return (XWhere x0' x1' x2')
                XTuple x0 x1
                  -> do x0' <- transN table x0
                        x1' <- transZM table x1
                        return (XTuple x0' x1')
                XList x0 x1
                  -> do x0' <- transN table x0
                        x1' <- transZM table x1
                        return (XList x0' x1')
                XListRange x0 x1 x2 x3 x4
                  -> do x0' <- transN table x0
                        x1' <- transZM table x1
                        x2' <- transZM table x2
                        x3' <- transZM table x3
                        x4' <- transZM table x4
                        return (XListRange x0' x1' x2' x3' x4')
                XListComp x0 x1 x2
                  -> do x0' <- transN table x0
                        x1' <- transZM table x1
                        x2' <- transZM table x2
                        return (XListComp x0' x1' x2')
                XWhile x0 x1 x2
                  -> do x0' <- transN table x0
                        x1' <- transZM table x1
                        x2' <- transZM table x2
                        return (XWhile x0' x1' x2')
                XWhen x0 x1 x2
                  -> do x0' <- transN table x0
                        x1' <- transZM table x1
                        x2' <- transZM table x2
                        return (XWhen x0' x1' x2')
                XUnless x0 x1 x2
                  -> do x0' <- transN table x0
                        x1' <- transZM table x1
                        x2' <- transZM table x2
                        return (XUnless x0' x1' x2')
                XBreak x0
                  -> do x0' <- transN table x0
                        return (XBreak x0')
                XDefix x0 x1
                  -> do x0' <- transN table x0
                        x1' <- transZM table x1
                        return (XDefix x0' x1')
                XDefixApps x0 x1
                  -> do x0' <- transN table x0
                        x1' <- transZM table x1
                        return (XDefixApps x0' x1')
                XOp x0 x1
                  -> do x0' <- transN table x0
                        x1' <- transZM table x1
                        return (XOp x0' x1')
                XApp x0 x1 x2
                  -> do x0' <- transN table x0
                        x1' <- transZM table x1
                        x2' <- transZM table x2
                        return (XApp x0' x1' x2')
                XAppSusp x0 x1 x2
                  -> do x0' <- transN table x0
                        x1' <- transZM table x1
                        x2' <- transZM table x2
                        return (XAppSusp x0' x1' x2')
                XParens x0 x1
                  -> do x0' <- transN table x0
                        x1' <- transZM table x1
                        return (XParens x0' x1')
 
instance (Monad m) => TransM m n1 n2 (Proj n1) (Proj n2) where
        transZM table xx
          = transMe (transProj table) (transProj_enter table)
              (transProj_leave table)
              table
              xx
        followZM table xx
          = case xx of
                JField x0 x1
                  -> do x0' <- transN table x0
                        x1' <- transZM table x1
                        return (JField x0' x1')
                JFieldR x0 x1
                  -> do x0' <- transN table x0
                        x1' <- transZM table x1
                        return (JFieldR x0' x1')
                JIndex x0 x1
                  -> do x0' <- transN table x0
                        x1' <- transZM table x1
                        return (JIndex x0' x1')
                JIndexR x0 x1
                  -> do x0' <- transN table x0
                        x1' <- transZM table x1
                        return (JIndexR x0' x1')
 
instance (Monad m) => TransM m n1 n2 (Stmt n1) (Stmt n2) where
        transZM table xx
          = transMe (transStmt table) (transStmt_enter table)
              (transStmt_leave table)
              table
              xx
        followZM table xx
          = case xx of
                SSig x0 x1 x2
                  -> do x0' <- transN table x0
                        x1' <- transZM table x1
                        x2' <- transZM table x2
                        return (SSig x0' x1' x2')
                SStmt x0 x1
                  -> do x0' <- transN table x0
                        x1' <- transZM table x1
                        return (SStmt x0' x1')
                SBindFun x0 x1 x2 x3
                  -> do x0' <- transN table x0
                        x1' <- transZM table x1
                        x2' <- transZM table x2
                        x3' <- transZM table x3
                        return (SBindFun x0' x1' x2' x3')
                SBindPat x0 x1 x2
                  -> do x0' <- transN table x0
                        x1' <- transZM table x1
                        x2' <- transZM table x2
                        return (SBindPat x0' x1' x2')
                SBindMonadic x0 x1 x2
                  -> do x0' <- transN table x0
                        x1' <- transZM table x1
                        x2' <- transZM table x2
                        return (SBindMonadic x0' x1' x2')
 
instance (Monad m) => TransM m n1 n2 (Alt n1) (Alt n2) where
        transZM table xx
          = transMe (transAlt table) (transAlt_enter table)
              (transAlt_leave table)
              table
              xx
        followZM table xx
          = case xx of
                APat x0 x1 x2
                  -> do x0' <- transN table x0
                        x1' <- transZM table x1
                        x2' <- transZM table x2
                        return (APat x0' x1' x2')
                AAlt x0 x1 x2
                  -> do x0' <- transN table x0
                        x1' <- transZM table x1
                        x2' <- transZM table x2
                        return (AAlt x0' x1' x2')
                ADefault x0 x1
                  -> do x0' <- transN table x0
                        x1' <- transZM table x1
                        return (ADefault x0' x1')
 
instance (Monad m) => TransM m n1 n2 (Guard n1) (Guard n2) where
        transZM table xx
          = transMe (transGuard table) (transGuard_enter table)
              (transGuard_leave table)
              table
              xx
        followZM table xx
          = case xx of
                GExp x0 x1 x2
                  -> do x0' <- transN table x0
                        x1' <- transZM table x1
                        x2' <- transZM table x2
                        return (GExp x0' x1' x2')
                GBool x0 x1
                  -> do x0' <- transN table x0
                        x1' <- transZM table x1
                        return (GBool x0' x1')
 
instance (Monad m) => TransM m n1 n2 (Pat n1) (Pat n2) where
        transZM table xx
          = transMe (transPat table) (transPat_enter table)
              (transPat_leave table)
              table
              xx
        followZM table xx
          = case xx of
                WVar x0 x1
                  -> do x0' <- transN table x0
                        x1' <- transZM table x1
                        return (WVar x0' x1')
                WObjVar x0 x1
                  -> do x0' <- transN table x0
                        x1' <- transZM table x1
                        return (WObjVar x0' x1')
                WLit x0 x1
                  -> do x0' <- transN table x0
                        x1' <- transZM table x1
                        return (WLit x0' x1')
                WCon x0 x1 x2
                  -> do x0' <- transN table x0
                        x1' <- transZM table x1
                        x2' <- transZM table x2
                        return (WCon x0' x1' x2')
                WConLabel x0 x1 x2
                  -> do x0' <- transN table x0
                        x1' <- transZM table x1
                        x2' <- transZM table x2
                        return (WConLabel x0' x1' x2')
                WAt x0 x1 x2
                  -> do x0' <- transN table x0
                        x1' <- transZM table x1
                        x2' <- transZM table x2
                        return (WAt x0' x1' x2')
                WWildcard x0
                  -> do x0' <- transN table x0
                        return (WWildcard x0')
                WUnit x0
                  -> do x0' <- transN table x0
                        return (WUnit x0')
                WTuple x0 x1
                  -> do x0' <- transN table x0
                        x1' <- transZM table x1
                        return (WTuple x0' x1')
                WCons x0 x1 x2
                  -> do x0' <- transN table x0
                        x1' <- transZM table x1
                        x2' <- transZM table x2
                        return (WCons x0' x1' x2')
                WList x0 x1
                  -> do x0' <- transN table x0
                        x1' <- transZM table x1
                        return (WList x0' x1')
 
instance (Monad m) => TransM m n1 n2 (Label n1) (Label n2) where
        transZM table xx
          = transMe (transLabel table) (transLabel_enter table)
              (transLabel_leave table)
              table
              xx
        followZM table xx
          = case xx of
                LIndex x0 x1
                  -> do x0' <- transN table x0
                        x1' <- transZM table x1
                        return (LIndex x0' x1')
                LVar x0 x1
                  -> do x0' <- transN table x0
                        x1' <- transZM table x1
                        return (LVar x0' x1')
 
instance (Monad m) => TransM m n1 n2 (LCQual n1) (LCQual n2) where
        transZM table xx
          = transMe (transLCQual table) (transLCQual_enter table)
              (transLCQual_leave table)
              table
              xx
        followZM table xx
          = case xx of
                LCGen x0 x1 x2
                  -> do x0' <- transZM table x0
                        x1' <- transZM table x1
                        x2' <- transZM table x2
                        return (LCGen x0' x1' x2')
                LCLet x0
                  -> do x0' <- transZM table x0
                        return (LCLet x0')
                LCExp x0
                  -> do x0' <- transZM table x0
                        return (LCExp x0')