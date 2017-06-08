
module DDC.Core.Check.Judge.EqT
        (makeEqT)
where
import DDC.Core.Check.Base 
import qualified DDC.Core.Env.EnvT      as EnvT
import qualified Data.Map.Strict        as Map


-- | Make two types equivalent to each other,
--   or throw the provided error if this is not possible.
makeEqT :: (Eq n, Ord n, Pretty n)
        => Config n
        -> Context n
        -> Type n
        -> Type n
        -> Error  a n
        -> CheckM a n (Context n)

makeEqT config ctx0 tL tR err

 -- EqT_SynL
 --   Expand type synonym on the left.
 | TCon (TyConBound (UName n) _) <- tL
 , Just tL' <- Map.lookup n $ EnvT.envtEquations $ contextEnvT ctx0
 = do
        ctrace  $ vcat
                [ text "**  EqT_SynL"
                , text "    tL : " <> ppr tL
                , text "    tL': " <> ppr tL'
                , text "    tR : " <> ppr tR
                , empty ]

        makeEqT config ctx0 tL' tR err


 -- EqT_SynR
 --   Expand type synonym on the right.
 | TCon (TyConBound (UName n) _) <- tR
 , Just tR' <- Map.lookup n $ EnvT.envtEquations $ contextEnvT ctx0
 = do
        ctrace  $ vcat
                [ text "**  EqT_SynR"
                , text "    tL : " <> ppr tL
                , text "    tR : " <> ppr tR
                , text "    tR': " <> ppr tR'
                , empty ]

        makeEqT config ctx0 tL tR' err

 -- EqT_SolveL
 | Just iL <- takeExists tL
 , not $ isTExists tR
 = do   
        ctrace  $ vcat
                [ text "**  EqT_SolveL"
                , text "    tL:  " <> ppr tL
                , text "    tR:  " <> ppr tR
                , empty ]

        let Just ctx1   = updateExists [] iL tR ctx0
        
        return ctx1


 -- EqT_SolveR
 | Just iR <- takeExists tR
 , not $ isTExists tL
 = do   
        ctrace  $ vcat
                [ text "**  EqT_SolveR"
                , text "    tL:  " <> ppr tL
                , text "    tR:  " <> ppr tR
                , empty ]
  
        let Just ctx1   = updateExists [] iR tL ctx0
        
        return ctx1


 -- EqT_EachL
 --  Both types are existentials, and the left is bound earlier in the stack.
 --  CAREFUL: The returned location is relative to the top of the stack,
 --           hence we need lL > lR here.
 | Just iL <- takeExists tL,    Just lL <- locationOfExists iL ctx0
 , Just iR <- takeExists tR,    Just lR <- locationOfExists iR ctx0
 , lL > lR
 = do   let Just ctx1   = updateExists [] iR tL ctx0
        
        ctrace  $ vcat
                [ text "**  EqT_EachL"
                , text "    tL: " <> ppr tL
                , text "    tR: " <> ppr tR
                , indent 4 $ ppr ctx0
                , indent 4 $ ppr ctx1
                , empty ]

        return ctx1


 -- EqT_EachR
 --  Both types are existentials, and the right is bound earlier in the stack.
 --  CAREFUL: The returned location is relative to the top of the stack,
 --           hence we need lR > lL here.
 | Just iL <- takeExists tL,    Just lL <- locationOfExists iL ctx0
 , Just iR <- takeExists tR,    Just lR <- locationOfExists iR ctx0
 , lR > lL
 = do   let Just ctx1   = updateExists [] iL tR ctx0

        ctrace  $ vcat
                [ text "**  EqT_EachR"
                , text "    tL: " <> ppr tL
                , text "    tR: " <> ppr tR
                , indent 4 $ ppr ctx0
                , indent 4 $ ppr ctx1
                , empty ]

        return ctx1


 -- EqT_Var
 | TVar u1      <- tL
 , TVar u2      <- tR
 , u1 == u2
 = do   
        -- Suppress tracing of boring rule.
        -- ctrace  $ vcat
        --         [ text "**  EqT_Var"
        --         , text "    tL: " <> ppr tL
        --         , text "    tR: " <> ppr tR
        --         , indent 4 $ ppr ctx0
        --         , empty ]

        return ctx0


 -- EqT_Con
 | TCon tc1     <- tL
 , TCon tc2     <- tR
 , equivTyCon tc1 tc2
 = do
        -- Only trace rule if it's done something interesting.
        when (not $ tc1 == tc2)
         $ ctrace  $ vcat
                [ text "**  EqT_Con"
                , text "    tL: " <> ppr tL
                , text "    tR: " <> ppr tR
                , empty ]

        return ctx0


 -- EqT_App
 | TApp tL1 tL2 <- tL
 , TApp tR1 tR2 <- tR
 = do
        ctrace  $ vcat
                [ text "*>  EqT_App" 
                , text "    tL: " <> ppr tL
                , text "    tR: " <> ppr tR
                , empty ]

        ctx1    <- makeEqT config ctx0 tL1  tR1  err
        tL2'    <- applyContext ctx1 tL2
        tR2'    <- applyContext ctx1 tR2
        ctx2    <- makeEqT config ctx1 tL2' tR2' err

        ctrace  $ vcat
                [ text "*<  EqT_App"
                , text "    tL: " <> ppr tL
                , text "    tR: " <> ppr tR
                , indent 4 $ ppr ctx0
                , indent 4 $ ppr ctx2
                , empty ]

        return ctx2


 -- EqT_Equiv
 | equivT (contextEnvT ctx0) tL tR 
 = do   ctrace  $ vcat
                [ text "**  EqT_Equiv" 
                , text "    tL: " <> ppr tL
                , text "    tR: " <> ppr tR
                , empty ]

        return ctx0


 -- EqT_Fail
 | otherwise
 = do
        ctrace  $ vcat
                [ text "EqT_Fail"
                , text "  tL: " <> ppr tL
                , text "  tR: " <> ppr tR
                , indent 2 $ ppr ctx0
                , empty ]

        throw err

