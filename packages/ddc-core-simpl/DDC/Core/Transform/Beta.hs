
-- | Beta-reduce applications of a explicit lambda abstractions 
--   to variables and values.
module DDC.Core.Transform.Beta
        ( BetaReduceInfo(..)
        , betaReduce)
where
import DDC.Base.Pretty
import DDC.Core.Collect
import DDC.Core.Exp
import DDC.Core.Predicates
import DDC.Core.Simplifier.Base
import DDC.Core.Transform.TransformX
import DDC.Core.Transform.SubstituteTX
import DDC.Core.Transform.SubstituteWX
import DDC.Core.Transform.SubstituteXX
import Control.Monad.Writer	        (Writer, runWriter, tell)
import Data.Monoid		        (Monoid, mempty, mappend)
import Data.Typeable		        (Typeable)
import DDC.Type.Env                     (Env)
import DDC.Type.Compounds
import qualified DDC.Type.Env           as Env
import qualified Data.Set               as Set


-------------------------------------------------------------------------------
-- | A summary of what the beta reduction transform did.
data BetaReduceInfo
        = BetaReduceInfo
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


instance Pretty BetaReduceInfo where
 ppr (BetaReduceInfo ty wit val lets skip)
  =  text "Beta reduction:"
  <$> indent 4 (vcat
      [ text "Types:          " <> int ty
      , text "Witnesses:      " <> int wit
      , text "Values:         " <> int val
      , text "Values letted:  " <> int lets
      , text "Values skipped: " <> int skip ])


instance Monoid BetaReduceInfo where
 mempty = BetaReduceInfo 0 0 0 0 0
 mappend (BetaReduceInfo ty1 wit1 val1 lets1 skip1)
         (BetaReduceInfo ty2 wit2 val2 lets2 skip2)
  = (BetaReduceInfo 
                (ty1   + ty2)   (wit1  + wit2) (val1 + val2)
                (lets1 + lets2) (skip1 + skip2))


-------------------------------------------------------------------------------
-- | Beta-reduce applications of a explicit lambda abstractions 
--   to variables and values.
--
--   If the flag is set then if we find a lambda abstraction that is applied
--   to a redex then let-bind the redex and substitute the new variable
--   instead.
betaReduce  
        :: forall (c :: * -> * -> *) a n 
        .  (Ord n, TransformUpMX (Writer BetaReduceInfo) c)
        => Bool         -- ^ Let-bind redexes.
        -> c a n 
        -> TransformResult (c a n)
betaReduce lets x
 = {-# SCC betaReduce #-}
   let (x', info) = runWriter
		  $ transformUpMX (betaReduce1 lets) Env.empty Env.empty x

       -- Check if any actual work was performed
       progress 
        = case info of
                BetaReduceInfo ty wit val lets' _
                 -> (ty + wit + val + lets') > 0

   in  TransformResult
	{ result   	 = x'
        , resultAgain    = progress
	, resultProgress = progress
	, resultInfo	 = TransformInfo info }


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
        => Bool	        -- ^ Let-bind redexes.
        -> Env n
        -> Env n
        -> Exp a n
        -> Writer BetaReduceInfo (Exp a n)

betaReduce1 lets _kenv tenv xx
 = let  ret info x = tell info >> return x
   in case xx of

        -- Substitute type arguments into type abstractions.
        --  If the type argument of the redex does not appear as an 
        --  argument of the result then we need to add a closure weakening
        --  for the case where t2 was a region variable or handle.
        XApp a (XLAM _ b11 x12) (XType t2)
         | isRegionKind $ typeOfBind b11
         -> let sup             = support Env.empty Env.empty x12

                usUsed          = Set.unions
                                        [ supportTyConXArg sup
                                        , supportSpVarXArg sup ]

                usesBind        = any (flip boundMatchesBind b11)
                                $ Set.toList usUsed

                fvs2            = freeT Env.empty t2

            in  ret mempty { infoTypes = 1}
                 $ if usesBind || Set.null fvs2
                    then substituteTX b11 t2 x12
                    else XCast a (CastWeakenClosure [XType t2])
                        $ substituteTX b11 t2 x12

        -- Substitute type arguments into type abstractions,
        --  Where the argument is not a region type.
        XApp _ (XLAM _ b11 x12) (XType t2)
         -> ret mempty { infoTypes = 1 }
                 $ substituteTX b11 t2 x12

        -- Substitute witness arguments into witness abstractions.
        XApp a (XLam _ b11 x12) (XWitness w2)
         -> let usesBind        = any (flip boundMatchesBind b11)
                                $ Set.toList $ freeX tenv x12
                fvs2            = freeX Env.empty w2
            in  ret mempty { infoWits = 1 }
                 $ if usesBind || Set.null fvs2
                    then substituteWX b11 w2 x12
                    else XCast a (CastWeakenClosure [XWitness w2])
                       $ substituteWX b11 w2 x12


        -- Substitute value arguments into value abstractions.
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

         | lets
         -> ret mempty { infoValuesLetted  = 1 }
	      $	XLet a (LLet LetStrict b11 x2) x12

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

        XApp _ x1 (XType _)
         -> canBetaSubstX x1

        XApp _ x1 (XWitness _)
         -> canBetaSubstX x1 

        _       -> False

