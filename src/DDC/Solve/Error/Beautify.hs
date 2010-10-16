{-# OPTIONS -fwarn-incomplete-patterns -fwarn-unused-matches -fwarn-name-shadowing #-}

-- | Beautification of type error messages.
module DDC.Solve.Error.Beautify
	(beautifyErrors)
where
import DDC.Type
import DDC.Solve.Error.Base
import DDC.Solve.Error.Transform
import DDC.Var.NiceNames
import DDC.Var
import Data.List
import Data.Maybe
import Data.Map			(Map)
import Data.Sequence		(Seq)
import qualified Data.Foldable	as Foldable
import qualified Data.Set	as Set
import qualified Data.Sequence	as Seq
import Control.Monad.State.Strict


-- | Rewrite the variables in a list of errors so they have nice consistent names.
--
--   Variables with source locations are not rewritten, because we want to present
--      the same names in error messages as were in the original program.
--
--   Introduced names to not clash with source names.
--
--   TODO: Also rewrite meta variables from the inferencer like %123 to real variables.
--   TODO: Also rewrite tag variables in closure terms.
--
beautifyErrors :: [Error] -> [Error]
beautifyErrors errs
 = let	
	-- Get all the varish things used in the errors
	bs	= collectBoundFromErrs errs
	
	-- This is all the plain variables used in the errors.
	vsAll	= nub $ mapMaybe takeVarOfBound $ Foldable.toList bs

	-- Partition variables into the ones that appear in the source program
	-- and the dummy variables that were introduced by the elaborator
	-- or type inferencer. We only want to rename the dummies.
	(vsSource, vsDummy)
	 	= partition (isJust . takeSourcePosOfVar) vsAll

	-- We can only use new names that were not already present in the 
	-- source program.
	nsSource = Set.fromList
		 $ map varName vsSource

	canUse n = not $ Set.member n nsSource

	-- Make the substitution that maps old names to new ones.
	(_, sub) = makeNiceVarSub canUse allNiceNames vsDummy

	-- Rewrite all the error messages.
	errs'	 = map (subError sub) errs

   in	errs'


-- | Collect all the bounds used in a list of errors.
--   This also returns varialbes in binding positions, like the ones
--   quantified by a forall.
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

-- | Substitute vars for vars in an error.
subError :: Map Var Var -> Error -> Error
subError sub err
	= evalState 
		(transZM (transTableId :: TransTable (State ()))
				{ transK	= \k -> return $ subVV_everywhere sub k
				, transT	= \t -> return $ subVV_everywhere sub t
				, transF	= \f -> return $ subVV_everywhere sub f }
			err)
		()
				