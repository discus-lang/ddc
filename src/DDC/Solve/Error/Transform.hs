
module DDC.Solve.Error.Transform
	( TransM (..)
	, TransTable (..)
	, transTableId)
where
import DDC.Type
import DDC.Solve.Location
import DDC.Solve.Error.Base
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

-- Structure
instance (Monad m, TransM m a, TransM m b) => TransM m (a, b) where
 transZM table (x, y)	= liftM2 (,) (transZM table x) (transZM table y)

-- No-ops
instance Monad m => TransM m TypeSource where
 transZM table xx	= return xx
	

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
	
	
	
	