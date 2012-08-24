
module DDC.Core.Transform.Beta
        (betaReduce)
where
import DDC.Base.Pretty
import DDC.Core.Collect
import DDC.Core.Exp
import DDC.Core.Simplifier.Base
import DDC.Core.Transform.TransformX
import DDC.Core.Transform.SubstituteTX
import DDC.Core.Transform.SubstituteWX
import DDC.Core.Transform.SubstituteXX
import Control.Monad.Writer	(Writer, runWriter, tell)
import Data.Monoid		(Monoid, mempty, mappend)
import Data.Typeable		(Typeable)
import DDC.Type.Compounds
import DDC.Type.Env             (Env)
import qualified DDC.Type.Env    as Env
import qualified Data.Set       as Set


-- | Beta-reduce applications of a explicit lambda abstractions 
--   to variables and values.
betaReduce  
        :: forall (c :: * -> * -> *) a n 
        .  (Ord n, TransformUpMX (Writer BetaReduceInfo) c)
        => c a n 
        -> TransformResult (c a n)
betaReduce x
 = let (x', info) = runWriter
		  $ transformUpMX betaReduce1 Env.empty Env.empty x
   in  TransformResult
	{ result   	 = x'
	, resultProgress = progress info
	, resultInfo	 = TransformInfo info }
 where
  -- Check if any actual work was performed
  progress (BetaReduceInfo ty wit val _)
   = (ty + wit + val) > 0



-- | Do a single beta reduction for this application.
--
--    To avoid duplicating work, we only reduce value applications when the
--     the argument is not a redex.
--
--    If needed, we also insert 'weakclo' to ensure the result has the same
--    closure as the original expression.
--    
betaReduce1
        :: Ord n
        => Env n
        -> Env n
        -> Exp a n
        -> Writer BetaReduceInfo (Exp a n)
betaReduce1 kenv tenv xx
 = case xx of
        XApp a (XLAM _ b11 x12) (XType t2)
         -> let usesBind        = any (flip boundMatchesBind b11)
                                $ Set.toList $ freeT kenv x12
                fvs2            = freeT Env.empty t2
            in  ret mempty { infoTypes = 1}
                 $ if usesBind || Set.null fvs2
                    then substituteTX b11 t2 x12
                     else XCast a (CastWeakenClosure [XType t2])
                        $ substituteTX b11 t2 x12

        XApp a (XLam _ b11 x12) (XWitness w2)
         -> let usesBind        = any (flip boundMatchesBind b11)
                                $ Set.toList $ freeX tenv x12
                fvs2            = freeX Env.empty w2
            in  ret mempty { infoWits = 1 }
                 $ if usesBind || Set.null fvs2
                    then substituteWX b11 w2 x12
                    else XCast a (CastWeakenClosure [XWitness w2])
                       $ substituteWX b11 w2 x12

        XApp a (XLam _ b11 x12) x2
         |  canBetaSubstX x2
         -> let usesBind        = any (flip boundMatchesBind b11) 
                                $ Set.toList $ freeX tenv x12
                fvs2            = freeX Env.empty x2
            in  ret mempty { infoValues = 1 }
                 $ if usesBind || Set.null fvs2
                    then substituteXX b11 x2 x12
                    else XCast a (CastWeakenClosure [x2])
                       $ substituteXX b11 x2 x12

         | otherwise
         -> ret mempty { infoValuesSkipped = 1 }
              $ xx

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

