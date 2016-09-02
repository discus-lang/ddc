
module DDC.Core.Check.Judge.EqX
        (makeEqX)
where
import DDC.Core.Check.Base
import qualified DDC.Core.Env.EnvT      as EnvT
import qualified Data.Map.Strict        as Map


-- | Make two types equivalent to each other,
--   or throw the provided error if this is not possible.
makeEqX :: (Eq n, Ord n, Pretty n, Show n)
        => Config n                     -- ^ Type checker configuration.
        -> a                            -- ^ Current annotation.
        -> Context n                    -- ^ Input context.
        -> Type n                       -- ^ Inferred type of expression.
        -> Type n                       -- ^ Expected type of expression.
        -> Error a n                    -- ^ Error to throw if we can't force equality.
        -> CheckM a n (Context n)

makeEqX config a ctx0 tL tR err

 -- EqX_SynL
 --   Expand type synonym on the left.
 | TCon (TyConBound (UName n) _) <- tL
 , Just tL' <- Map.lookup n $ EnvT.envtEquations $ contextEnvT ctx0
 = do
        ctrace  $ vcat
                [ text "**  EqX_SynL"
                , text "    tL : " <> ppr tL
                , text "    tL': " <> ppr tL'
                , text "    tR : " <> ppr tR
                , empty ]

        makeEqX config a ctx0 tL' tR err


 -- EqX_SynR
 --   Expand type synonym on the right.
 | TCon (TyConBound (UName n) _) <- tR
 , Just tR' <- Map.lookup n $ EnvT.envtEquations $ contextEnvT ctx0
 = do
        ctrace  $ vcat
                [ text "**  EqX_SynR"
                , text "    tL : " <> ppr tL
                , text "    tR : " <> ppr tR
                , text "    tR': " <> ppr tR'
                , empty ]

        makeEqX config a ctx0 tL tR' err


 -- EqX_SolveL
 | Just iL <- takeExists tL
 , not $ isTExists tR
 = do   
        ctrace  $ vcat
                [ text "**  EqX_SolveL"
                , text "    tL:  " <> ppr tL
                , text "    tR:  " <> ppr tR
                , empty ]

        let Just ctx1   = updateExists [] iL tR ctx0

        return ctx1


 -- EqX_SolveR
 | Just iR <- takeExists tR
 , not $ isTExists tL
 = do   
        ctrace  $ vcat
                [ text "**  EqX_SolveR"
                , text "    tL:  " <> ppr tL
                , text "    tR:  " <> ppr tR
                , empty ]

        let Just ctx1   = updateExists [] iR tL ctx0

        return ctx1


 -- EqX_EachL
 --   Both types are existentials, and the left is bound earlier in the stack.
 --   CAREFUL: The returned location is relative to the top of the stack,
 --            hence we need lL > lR here.
 | Just iL <- takeExists tL,    Just lL <- locationOfExists iL ctx0
 , Just iR <- takeExists tR
 , True    <- case locationOfExists iR ctx0 of
                Just lR -> lL > lR      -- Left is earlier in the stack.
                Nothing -> True         -- Right has already been popped off.
 = do   
        let Just ctx1   = updateExists [] iR tL ctx0

        ctrace  $ vcat
                [ text "**  EqX_EachL"
                , text "    tL: " <> ppr tL
                , text "    tR: " <> ppr tR
                , indent 4 $ ppr ctx0
                , indent 4 $ ppr ctx1
                , empty ]

        return ctx1


 -- EqX_EachR
 --   Both types are existentials, and the right is bound earlier in the stack.
 --   CAREFUL: The returned location is relative to the top of the stack,
 --            hence we need lR > lL here.
 | Just iL <- takeExists tL
 , Just iR <- takeExists tR,    Just lR <- locationOfExists iR ctx0
 , True    <- case locationOfExists iL ctx0 of
                Just lL -> lR > lL      -- Right is earlier in the stack.
                Nothing -> True         -- Left has already been popped off.
 = do
        let Just ctx1   = updateExists [] iL tR ctx0

        ctrace  $ vcat
                [ text "**  EqX_EachR"
                , text "    tL: " <> ppr tL
                , text "    tR: " <> ppr tR
                , indent 4 $ ppr ctx0
                , indent 4 $ ppr ctx1
                , empty ]

        return ctx1


 -- EqX_Var
 --   Both sides are the same (rigid) type variable,
 --   so we don't need to do anything further.
 | TVar u1      <- tL
 , TVar u2      <- tR
 , u1 == u2
 = do
        -- Suppress tracing of boring rule.
        -- ctrace  $ vcat
        --         [ text "**  EqX_Var"
        --         , text "    tL: " <> ppr tL
        --         , text "    tR: " <> ppr tR
        --         , indent 4 $ ppr ctx0
        --         , empty ]

        return ctx0


 -- EqX_Con
 --   Both sides are equivalent.
 --   The `equivT` function will also crush any effect types, 
 --   and handle comparing type sums for equivalence.
 --
 | TCon tc1     <- tL
 , TCon tc2     <- tR
 , equivTyCon tc1 tc2
 = do
        -- Only trace rule if it's done something interesting.
        when (not $ tc1 == tc2)
         $ ctrace  $ vcat
                [ text "**  EqX_Con"
                , text "    tL: " <> ppr tL
                , text "    tR: " <> ppr tR
                , empty ]

        return ctx0


 -- EqX_App
 | TApp tL1 tL2 <- tL
 , TApp tR1 tR2 <- tR
 = do
        ctrace  $ vcat
                [ text "*>  EqX_App" 
                , text "    tL: " <> ppr tL
                , text "    tR: " <> ppr tR
                , empty ]

        ctx1    <- makeEqX config a ctx0 tL1 tR1 err
        tL2'    <- applyContext ctx1 tL2
        tR2'    <- applyContext ctx1 tR2
        ctx2    <- makeEqX config a ctx1 tL2' tR2' err

        ctrace  $ vcat
                [ text "*<  EqX_App"
                , text "    tL: " <> ppr tL
                , text "    tR: " <> ppr tR
                , indent 4 $ ppr ctx0
                , indent 4 $ ppr ctx2
                , empty ]

        return ctx2


 -- EqX_Equiv
 | equivT (contextEnvT ctx0) tL tR 
 = do   ctrace  $ vcat
                [ text "**  EqX_Equiv" 
                , text "    tL: " <> ppr tL
                , text "    tR: " <> ppr tR
                , empty ]

        return ctx0


 -- EqX_Fail
 | otherwise
 = do
        ctrace  $ vcat
                [ text "EqX_Fail"
                , text "  tL: " <> ppr tL
                , text "  tR: " <> ppr tR
                , indent 2 $ ppr ctx0
                , empty ]

        throw err


