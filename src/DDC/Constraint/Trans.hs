{-# OPTIONS -fwarn-incomplete-patterns -fwarn-unused-matches -fwarn-name-shadowing #-}

-- | Transformation boiler-plate for constraints.
module DDC.Constraint.Trans
	( transM 
	, Table (..))
where
import DDC.Constraint.Exp
import DDC.Constraint.Pretty	()
import DDC.Type.Exp
import DDC.Main.Error
import qualified Data.Traversable	as Seq
import Data.Sequence			(Seq)
import Util

stage	= "DDC.Constraint.Plate.Trans"


-- | Transform class
class Monad m => TransM m a where
 transM :: Table m -> a -> m a


-- | Transform table
data Table m
	= Table
	{ tableTransT	:: Type -> m Type }

	
-- Simple Instances --------------------------------------------------------------------------------
instance (Monad m, TransM m a) => TransM m [a] where
 transM _     []	= return []
 transM table (x:xs)	
  = do	x'	<- transM table x
  	xs'	<- mapM (transM table) xs
	return	$  x' : xs'

instance (Monad m, TransM m a) => TransM m (Seq a) where
 transM table mm	= Seq.mapM (transM table) mm


-- CTree -------------------------------------------------------------------------------------------
instance Monad m => TransM m CTree where
 transM table xx 
  = let down xx' = transM table xx'
    in case xx of
  	CTreeNil	-> return CTreeNil
	
	CBranch bb bs
	 -> liftM2 CBranch (return bb) (down bs)
		
	CSig ts t1 t2
	 -> liftM3 CSig    (return ts) (down t1) (down t2)
	
	CEq ts t1 t2
	 -> liftM3 CEq	   (return ts) (down t1) (down t2)
		
	CEqs ts tt
	 -> liftM2 CEqs	   (return ts) (down tt)
	
	CMore ts t1 t2
	 -> liftM3 CMore   (return ts) (down t1) (down t2)
	
	CClass ts v tt
	 -> liftM3 CClass  (return ts) (return v) (down tt)
			
	CProject ts j v t1 t2
 	 -> do 	t1'	<- down t1
		t2'	<- down t2
		return	$ CProject ts j v t1' t2'
		
	CInst{}	
	 -> return xx
	
	CGen ts t
	 -> liftM2 CGen (return ts) (down t)
			
	CDictProject ts t mv
	 -> liftM3 CDictProject (return ts) (down t) (return mv)
		
	CClassInst ts v tt
	 -> liftM3 CClassInst   (return ts) (return v) (down tt)
			
	-- don't handle constructors used internally by the solver
	_ -> panic stage
		$ "transM/CTree: no match for " % xx % "\n"


-- Type --------------------------------------------------------------------------------------------
instance Monad m => TransM m Type where
 transM table	= tableTransT table


