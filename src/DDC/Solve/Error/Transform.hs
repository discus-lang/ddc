{-# OPTIONS -fwarn-incomplete-patterns -fwarn-unused-matches -fwarn-name-shadowing #-}

module DDC.Solve.Error.Transform
	( TransM (..)
	, TransTable (..)
	, transTableId)
where
import DDC.Solve.Location
import DDC.Solve.Error.Base
import DDC.Type.SigMode
import DDC.Type
import DDC.Var
import DDC.Base.SourcePos
import Control.Monad
import Util.Control.Monad

-- | Monadic transforms.
class Monad m => TransM m a 
 where	transZM :: TransTable m -> a -> m a


-- | Table containing fns to apply to various components of a type error
data TransTable m
	= TransTable
	{ transK	:: Kind   -> m Kind
	, transT	:: Type   -> m Type
	, transF	:: Fetter -> m Fetter }
	
	
-- | Identity transform.
transTableId :: Monad m => TransTable m
transTableId
	= TransTable
	{ transK	= \k -> return k
	, transT	= \t -> return t
	, transF	= \f -> return f }
	

-- Basics
instance Monad m => TransM m Type where
 transZM table tt	= transT table tt

instance Monad m => TransM m Kind where
 transZM table kk	= transK table kk


-- Structure
instance (Monad m, TransM m a, TransM m b) => TransM m (a, b) where
 transZM table (x, y)	= liftM2 (,) (transZM table x) (transZM table y)

instance (Monad m, TransM m a) => TransM m (Maybe a) where
 transZM table xx
  = case xx of
	Nothing		-> return Nothing
	Just x		-> liftM Just (transZM table x)

instance (Monad m, TransM m a) => TransM m [a] where
 transZM table xx	= mapM (transZM table) xx


-- No-ops
instance Monad m => TransM m Var where
 transZM _ xx	= return xx

instance Monad m => TransM m SourcePos where
 transZM _ xx	= return xx

instance Monad m => TransM m TypeSource where
 transZM _ xx	= return xx

instance Monad m => TransM m SigMode where
 transZM _ xx	= return xx


-- The errors
instance Monad m => TransM m Error where
 transZM table rr
  = case rr of
	ErrorUnifyCtorMismatch x1 x2 x3 x4
	 -> liftM4 ErrorUnifyCtorMismatch
			(transZM table x1) (transZM table x2) (transZM table x3) 
			(transZM table x4)
		
	ErrorSigMismatch x1 x2 x3 x4 x5 x6
	 -> liftM6 ErrorSigMismatch 
			(transZM table x1) (transZM table x2) (transZM table x3)
			(transZM table x4) (transZM table x5) (transZM table x6)
	
	_	-> return rr
	
	
	
	