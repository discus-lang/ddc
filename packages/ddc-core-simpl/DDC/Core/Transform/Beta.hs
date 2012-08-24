
module DDC.Core.Transform.Beta
        ( betaReduce
	, betaReduceTrans)
where
import DDC.Base.Pretty
import DDC.Core.Exp
import DDC.Core.Simplifier.Base
import DDC.Core.Transform.TransformX
import DDC.Core.Transform.SubstituteTX
import DDC.Core.Transform.SubstituteWX
import DDC.Core.Transform.SubstituteXX
import Control.Monad.Writer	(Writer, runWriter, tell)
import Data.Monoid		(Monoid, mempty, mappend)
import Data.Typeable		(Typeable)
import DDC.Type.Env             (Env)
import qualified DDC.Type.Env    as Env


-- | Beta-reduce applications of a explicit lambda abstractions 
--   to variables and values.
betaReduce  
        :: forall (c :: * -> * -> *) a n 
        .  (Ord n, TransformUpMX (Writer BetaReduceInfo) c)
        => c a n 
        -> c a n
betaReduce x
	-- Ignore the extra information for now
        = fst
	$ runWriter
	$ transformUpMX betaReduce1 Env.empty Env.empty x

-- | Beta-reduce applications of a explicit lambda abstractions 
--   to variables and values.
--   Return information about which beta reductions are performed
betaReduceTrans
        :: (Ord n)
        => Exp a n 
        -> TransformResult a n
betaReduceTrans x
 = let (x', info) = runWriter
		  $ transformUpMX betaReduce1 Env.empty Env.empty x
   in  TransformResult
	{ resultExp	 = x'
	, resultProgress = progress info
	, resultInfo	 = TransformInfo info }
 where
  -- Check if any actual work was performed
  progress (BetaReduceInfo ty wit val _)
   = (ty + wit + val) > 0

betaReduce1
    :: Ord n
    => Env n
    -> Env n
    -> Exp a n
    -> Writer BetaReduceInfo (Exp a n)
betaReduce1 _ _ xx
 = case xx of
        XApp _ (XLAM _ b11 x12) (XType t2)
         -> ret mempty { infoTypes	   = 1 }
	      $ substituteTX b11 t2 x12

        XApp _ (XLam _ b11 x12) (XWitness w2)
         -> ret mempty { infoWits	   = 1 }
	      $	substituteWX b11 w2 x12

        XApp _ (XLam _ b11 x12) x2
         |  canBetaSubstX x2     
         -> ret mempty { infoValues	   = 1 }
	      $	substituteXX b11 x2 x12

         | otherwise
         -> ret mempty { infoValuesSkipped = 1 }
	      $	xx

        _ -> return xx
 where
  ret info x = tell info >> return x



-- | Check whether we can safely substitute this expression during beta
--   evaluation. 
-- 
--   We allow variables, abstractions, type and witness applications.
--   Duplicating these expressions is guaranteed not to duplicate work
--   at runtime,
canBetaSubstX :: Exp a n -> Bool
canBetaSubstX xx
 = case xx of
        XVar{}  -> True
        XCon{}  -> True
        XLam{}  -> True
        XLAM{}  -> True

        XApp _ x1 (XType _)
         -> canBetaSubstX x1

        XApp _ x1 (XWitness _)
         -> canBetaSubstX x1 

        _       -> False


-- | A summary of what the beta reduction performed
data BetaReduceInfo
    = BetaReduceInfo
    { infoTypes		:: Int
    , infoWits		:: Int
    , infoValues	:: Int
    , infoValuesSkipped :: Int }
    deriving Typeable


instance Pretty BetaReduceInfo where
 ppr (BetaReduceInfo ty wit val skip)
  =  text "Beta reduction:"
  <$> indent 4 (vcat
      [ text "Types:          "	<> int ty
      , text "Witnesses:      "	<> int wit
      , text "Values:         "	<> int val
      , text "Values skipped: " <> int skip ])


instance Monoid BetaReduceInfo where
 mempty = BetaReduceInfo 0 0 0 0
 mappend
    (BetaReduceInfo ty1 wit1 val1 skip1)
    (BetaReduceInfo ty2 wit2 val2 skip2)
  = (BetaReduceInfo (ty1+ty2) (wit1+wit2) (val1+val2) (skip1+skip2))

