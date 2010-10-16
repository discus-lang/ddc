{-# OPTIONS -fwarn-incomplete-patterns -fwarn-unused-matches -fwarn-name-shadowing #-}

module DDC.Solve.Error.Beautify
	(beautifyErrors)
where
import DDC.Type
import DDC.Solve.Error.Base
import DDC.Solve.Error.Transform
import Control.Monad.State.Strict
import Data.Sequence		(Seq)
import qualified Data.Sequence	as Seq
import Debug.Trace

beautifyErrors :: [Error] -> [Error]
beautifyErrors errs
 = let	bs	= collectBoundFromErrs errs
   in	trace 	(show bs)
		$ errs


-- | Collect all the bounds used in a list of errors.
collectBoundFromErrs :: [Error] -> Seq Bound
collectBoundFromErrs errs
 	= execState (mapM collectBoundFromErrM errs) Seq.empty

collectBoundFromErrM :: Error -> State (Seq Bound) Error
collectBoundFromErrM err
 = let	collectK k
 	 = do	modify (Seq.>< texBound k)
		return k

	collectT t
	 = do	modify (Seq.>< texBound t) 
		return t
		
	collectF f
	 = do	modify (Seq.>< texBound f)
		return f
	
   in	transZM ((transTableId :: TransTable (State (Seq Bound)))
			{ transK	= collectK
			, transT	= collectT
			, transF	= collectF })
		err
