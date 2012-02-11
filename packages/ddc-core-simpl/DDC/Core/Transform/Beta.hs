
module DDC.Core.Transform.Beta
        (betaReduce)
where
import DDC.Core.Exp
import DDC.Core.Transform.TransformX
import DDC.Core.Transform.SubstituteTX
import DDC.Core.Transform.SubstituteWX
import DDC.Core.Transform.SubstituteXX
import DDC.Type.Env     (Env)

-- | Beta-reduce applications of a explicit lambda abstractions 
--   to variables and values.
betaReduce  :: Ord n => Env n -> Env n -> Exp a n -> Exp a n
betaReduce 
        = transformUpX betaReduce1


betaReduce1 :: Ord n => Env n -> Env n -> Exp a n -> Exp a n
betaReduce1 _ _ xx
 = case xx of
        XApp _ (XLAM _ b11 x12) (XType t2)
         -> substituteTX b11 t2 x12

        XApp _ (XLam _ b11 x12) (XWitness w2)
         -> substituteWX b11 w2 x12

        XApp _ (XLam _ b11 x12) x2
         |  canBetaSubstX x2     
         -> substituteXX b11 x2 x12

         | otherwise
         -> xx

        _ -> xx


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

