
-- | Beta-reduce applications of a explicit lambda abstractions
--   to variables and values.
module DDC.Core.Transform.Beta
        ( Config        (..)
        , configZero
        , Info          (..)
        , betaReduce)
where
import DDC.Core.Exp.Annot
import DDC.Core.Fragment
import DDC.Core.Transform.TransformUpX
import DDC.Core.Transform.SubstituteTX
import DDC.Core.Transform.SubstituteWX
import DDC.Core.Transform.SubstituteXX
import DDC.Core.Simplifier.Result
import DDC.Core.Env.EnvX                (EnvX)
import Control.Monad.Writer             (Writer, runWriter, tell)
import Data.Typeable                    (Typeable)
import DDC.Data.Pretty                  as P
import qualified DDC.Core.Env.EnvX      as EnvX
import qualified Data.Semigroup         as SG


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
  = vcat
  [ text "Beta reduction:"
  , indent 4 $ vcat
      [ text "Types:          " % int ty
      , text "Witnesses:      " % int wit
      , text "Values:         " % int val
      , text "Values letted:  " % int lets
      , text "Values skipped: " % int skip ] ]


instance SG.Semigroup Info where
 (<>)           = unionInfo


instance Monoid Info where
 mempty         = emptyInfo
 mappend        = unionInfo


-- | Construct an empty info record.
emptyInfo :: Info
emptyInfo = Info 0 0 0 0 0


-- | Union two info records.
unionInfo :: Info -> Info -> Info
unionInfo (Info ty1 wit1 val1 lets1 skip1)
          (Info ty2 wit2 val2 lets2 skip2)
  = Info  (ty1   + ty2)   (wit1  + wit2) (val1 + val2)
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
                  $ transformUpMX (betaReduce1 profile config) EnvX.empty x

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
        -> EnvX n       -- ^ Current environment
        -> Exp a n      -- ^ Expression to transform.
        -> Writer Info (Exp a n)

betaReduce1 _profile config _env xx
 = let  ret info x = tell info >> return x

   in case xx of

        -- Substitute type arguments into type abstractions.
        --  If the type argument of the redex does not appear as an
        --  argument of the result then we need to add a closure weakening
        --  for the case where t2 was a region variable or handle.
        XApp _a (XLAM _ b11 x12) (RType t2)
         | isRegionKind $ typeOfBind b11
         -> ret mempty { infoTypes = 1}
              $ substituteTX b11 t2 x12

        -- Substitute type arguments into type abstractions,
        --  Where the argument is not a region type.
        XApp _a (XLAM _ b11 x12) (RType t2)
         -> ret mempty { infoTypes = 1 }
              $ substituteTX b11 t2 x12

        -- Substitute witness arguments into witness abstractions.
        XApp _a (XLam _ b11 x12) (RWitness w2)
         -> ret mempty { infoWits = 1 }
              $ substituteWX b11 w2 x12

        -- Substitute value arguments into value abstractions.
        XApp a (XLam _ b11 x12) (RTerm x2)
         |  canBetaSubstX x2
         -> ret mempty { infoValues = 1 }
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
        XVar{}                  -> True
        XCon{}                  -> True
        XLam{}                  -> True
        XLAM{}                  -> True
        XApp _ x1 RType{}       -> canBetaSubstX x1
        XApp _ x1 RWitness{}    -> canBetaSubstX x1
        _                       -> False

