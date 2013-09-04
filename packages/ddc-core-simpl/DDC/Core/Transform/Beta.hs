
-- | Beta-reduce applications of a explicit lambda abstractions 
--   to variables and values.
module DDC.Core.Transform.Beta
        ( Config        (..)
        , configZero
        , Info          (..)
        , betaReduce)
where
import DDC.Base.Pretty
import DDC.Core.Collect
import DDC.Core.Exp
import DDC.Core.Fragment
import DDC.Core.Predicates
import DDC.Core.Transform.TransformUpX
import DDC.Core.Transform.SubstituteTX
import DDC.Core.Transform.SubstituteWX
import DDC.Core.Transform.SubstituteXX
import DDC.Core.Simplifier.Result
import Control.Monad.Writer             (Writer, runWriter, tell)
import Data.Monoid                      (Monoid, mempty, mappend)
import Data.Typeable                    (Typeable)
import DDC.Type.Env                     (KindEnv, TypeEnv)
import DDC.Type.Compounds
import qualified DDC.Type.Env           as Env
import qualified Data.Set               as Set


-------------------------------------------------------------------------------
data Config
        = Config
        { -- | If we find a lambda abstraction applied to a redex then let-bind
          --   the redex and substitute the new variable instead.
          configBindRedexes     :: Bool }
        deriving Show


-- | Empty beta configuration with all flags set to False.
configZero :: Config
configZero
        = Config
        { configBindRedexes     = False }


-------------------------------------------------------------------------------
-- | A summary of what the beta reduction transform did.
data Info
        = Info
        { -- | Number of type applications reduced.
          infoTypes             :: Int

          -- | Number of witness applications reduced.
        , infoWits              :: Int

          -- | Number of value applications reduced.
        , infoValues            :: Int

          -- | Number of redexes let-bound.
        , infoValuesLetted      :: Int

          -- | Number of applications that we couldn't reduce.
        , infoValuesSkipped     :: Int }
        deriving Typeable


instance Pretty Info where
 ppr (Info ty wit val lets skip)
  =  text "Beta reduction:"
  <$> indent 4 (vcat
      [ text "Types:          " <> int ty
      , text "Witnesses:      " <> int wit
      , text "Values:         " <> int val
      , text "Values letted:  " <> int lets
      , text "Values skipped: " <> int skip ])


instance Monoid Info where
 mempty = Info 0 0 0 0 0
 mappend (Info ty1 wit1 val1 lets1 skip1)
         (Info ty2 wit2 val2 lets2 skip2)
  = Info 
                (ty1   + ty2)   (wit1  + wit2) (val1 + val2)
                (lets1 + lets2) (skip1 + skip2)


-------------------------------------------------------------------------------
-- | Beta-reduce applications of a explicit lambda abstractions 
--   to variables and values.
--
--   If the flag is set then if we find a lambda abstraction that is applied
--   to a redex then let-bind the redex and substitute the new variable
--   instead.
betaReduce  
        :: forall (c :: * -> * -> *) a n 
        .  (Ord n, TransformUpMX (Writer Info) c)
        => Profile n    -- ^ Language profile.
        -> Config       -- ^ Beta transform config.
        -> c a n        -- ^ Thing to transform.
        -> TransformResult (c a n)

betaReduce profile config x
 = {-# SCC betaReduce #-}
   let (x', info) = runWriter
                  $ transformUpMX (betaReduce1 profile config) Env.empty Env.empty x

       -- Check if any actual work was performed
       progress 
        = case info of
                Info ty wit val lets' _
                 -> (ty + wit + val + lets') > 0

   in  TransformResult
        { result         = x'
        , resultAgain    = progress
        , resultProgress = progress
        , resultInfo     = TransformInfo info }


-- | Do a single beta reduction for this application.
--
--    To avoid duplicating work, we only reduce value applications when the
--    the argument is not a redex.
--
--    If needed, we also insert 'weakclo' to ensure the result has the same
--    closure as the original expression.
--    
betaReduce1
        :: Ord n
        => Profile n    -- ^ Language profile.
        -> Config       -- ^ Beta tranform config.
        -> KindEnv n    -- ^ Current kind environment.
        -> TypeEnv n    -- ^ Current type environment.
        -> Exp a n      -- ^ Expression to transform.
        -> Writer Info (Exp a n)

betaReduce1 profile config _kenv tenv xx
 = let  ret info x = tell info >> return x

        -- If we're using closure types then when we perform a beta-reduction:
        --  (\v. X1) X2 => X1[X2/v] then we need to weaken the closure if the 
        -- body expression X1 does not reference 'v'.
        weakenClosure a usesBind fvs2 xWeak x
         | featuresTrackedClosures $ profileFeatures profile 
         , not (usesBind || Set.null fvs2)
         = XCast a (CastWeakenClosure [xWeak]) x

         | otherwise
         = x

   in case xx of

        -- Substitute type arguments into type abstractions.
        --  If the type argument of the redex does not appear as an 
        --  argument of the result then we need to add a closure weakening
        --  for the case where t2 was a region variable or handle.
        XApp a (XLAM _ b11 x12) (XType a2 t2)
         | isRegionKind $ typeOfBind b11
         -> let sup             = support Env.empty Env.empty x12

                usUsed          = Set.unions
                                        [ supportTyConXArg sup
                                        , supportSpVarXArg sup ]

                usesBind        = any (flip boundMatchesBind b11)
                                $ Set.toList usUsed

                fvs2            = freeT Env.empty t2

            in  ret mempty { infoTypes = 1}
                 $ weakenClosure a usesBind fvs2 (XType a2 t2)
                 $ substituteTX b11 t2 x12

        -- Substitute type arguments into type abstractions,
        --  Where the argument is not a region type.
        XApp _ (XLAM _ b11 x12) (XType _ t2)
         -> ret mempty { infoTypes = 1 }
                 $ substituteTX b11 t2 x12

        -- Substitute witness arguments into witness abstractions.
        XApp a (XLam _ b11 x12) (XWitness a2 w2)
         -> let usesBind        = any (flip boundMatchesBind b11)
                                $ Set.toList $ freeX tenv x12
                fvs2            = freeX Env.empty w2
            in  ret mempty { infoWits = 1 }
                 $ weakenClosure a usesBind fvs2 (XWitness a2 w2)
                 $ substituteWX b11 w2 x12

        -- Substitute value arguments into value abstractions.
        XApp a (XLam _ b11 x12) x2
         |  canBetaSubstX x2
         -> let usesBind        = any (flip boundMatchesBind b11) 
                                $ Set.toList $ freeX tenv x12
                fvs2            = freeX Env.empty x2
            in  ret mempty { infoValues = 1 }
                 $ weakenClosure a usesBind fvs2 x2
                 $ substituteXX b11 x2 x12

         | configBindRedexes config
         -> ret mempty { infoValuesLetted  = 1 }
              $ XLet a (LLet b11 x2) x12

         | otherwise
         -> ret mempty { infoValuesSkipped = 1 }
              $ xx

        _ -> return xx


-- | Check whether we can safely substitute this expression during beta
--   evaluation. 
-- 
--   We allow variables, abstractions, type and witness applications.
--   Duplicating these expressions is guaranteed not to duplicate work
--   at runtime,
--
canBetaSubstX :: Exp a n -> Bool
canBetaSubstX xx
 = case xx of
        XVar{}  -> True
        XCon{}  -> True
        XLam{}  -> True
        XLAM{}  -> True

        XApp _ x1 XType{}
         -> canBetaSubstX x1

        XApp _ x1 XWitness{}
         -> canBetaSubstX x1 

        _       -> False

