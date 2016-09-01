
module DDC.Core.Check.Judge.EqX
        (makeEqX)
where
import DDC.Core.Check.Config
import DDC.Core.Check.Base
import DDC.Core.Check.Judge.EqT
import DDC.Core.Exp.Annot.AnTEC
import qualified DDC.Core.Env.EnvT      as EnvT
import qualified DDC.Type.Sum           as Sum
import qualified Data.Map.Strict        as Map


-- | Make two types equivalent to each other,
--   or throw the provided error if this is not possible.
makeEqX :: (Eq n, Ord n, Pretty n, Show n)
        => Config n                     -- ^ Type checker configuration.
        -> a                            -- ^ Current annotation.
        -> Context n                    -- ^ Input context.
        -> Exp (AnTEC a n) n            -- ^ Expression that we've inferred the type of.
        -> Type n                       -- ^ Inferred type of expression.
        -> Type n                       -- ^ Expected type of expression.
        -> Error a n                    -- ^ Error to throw if we can't force equality.
        -> CheckM a n 
                ( Exp (AnTEC a n) n     -- ^ Expression after instantiations and running.
                , Context n)

makeEqX config a ctx0 xL tL tR err

 -- Expand type equations.
 | TCon (TyConBound (UName n) _) <- tL
 , Just tL' <- Map.lookup n $ EnvT.envtEquations $ contextEnvT ctx0
 = makeEqX config a ctx0 xL tL' tR err


 | TCon (TyConBound (UName n) _) <- tR
 , Just tR' <- Map.lookup n $ EnvT.envtEquations $ contextEnvT ctx0
 = makeEqX config a ctx0 xL tL tR' err


 -- EqLSolve
 | Just iL <- takeExists tL
 , not $ isTExists tR
 = do   
        let Just ctx1   = updateExists [] iL tR ctx0

        ctrace  $ vcat
                [ text "**  EqLSolve"
                , text "    LEFT:  " <> ppr tL
                , text "    RIGHT: " <> ppr tR
                , indent 4 $ ppr ctx0
                , indent 4 $ ppr ctx1
                , empty ]

        return  ( xL
                , ctx1)


 -- EqRSolve
 | Just iR <- takeExists tR
 , not $ isTExists tL
 = do   
        let Just ctx1   = updateExists [] iR tL ctx0

        ctrace  $ vcat
                [ text "**  EqRSolve"
                , text "    LEFT:  " <> ppr tL
                , text "    RIGHT: " <> ppr tR
                , indent 4 $ ppr ctx0
                , indent 4 $ ppr ctx1
                , empty ]

        return  ( xL
                , ctx1)


 -- EqLReach
 --  Both types are existentials, and the left is bound earlier in the stack.
 --  CAREFUL: The returned location is relative to the top of the stack,
 --           hence we need lL > lR here.
 | Just iL <- takeExists tL,    Just lL <- locationOfExists iL ctx0
 , Just iR <- takeExists tR
 , True    <- case locationOfExists iR ctx0 of
                Just lR -> lL > lR      -- Left is earlier in the stack.
                Nothing -> True         -- Right has already been popped off.
 = do   
        let Just ctx1   = updateExists [] iR tL ctx0

        ctrace  $ vcat
                [ text "**  EqLReach"
                , text "    LEFT:  " <> ppr tL
                , text "    RIGHT: " <> ppr tR
                , indent 4 $ ppr ctx0
                , indent 4 $ ppr ctx1
                , empty ]

        return  ( xL
                , ctx1)

 -- EqRReach
 --  Both types are existentials, and the right is bound earlier in the stack.
 --  CAREFUL: The returned location is relative to the top of the stack,
 --           hence we need lR > lL here.
 | Just iL <- takeExists tL
 , Just iR <- takeExists tR,    Just lR <- locationOfExists iR ctx0
 , True    <- case locationOfExists iL ctx0 of
                Just lL -> lR > lL      -- Right is earlier in the stack.
                Nothing -> True         -- Left has already been popped off.
 = do
        let Just ctx1   = updateExists [] iL tR ctx0

        ctrace  $ vcat
                [ text "**  EqRReach"
                , text "    LEFT:  " <> ppr tL
                , text "    RIGHT: " <> ppr tR
                , indent 4 $ ppr ctx0
                , indent 4 $ ppr ctx1
                , empty ]

        return  ( xL
                , ctx1)


 -- EqVar
 | TVar u1      <- tL
 , TVar u2      <- tR
 , u1 == u2
 = do
        ctrace  $ vcat
                [ text "**  EqVar"
                , text "    LEFT:  " <> ppr tL
                , text "    RIGHT: " <> ppr tR
                , indent 4 $ ppr ctx0
                , empty ]

        return  ( xL
                , ctx0)


 -- EqCon
 | TCon tc1     <- tL
 , TCon tc2     <- tR
 , equivTyCon tc1 tc2
 = do
        ctrace  $ vcat
                [ text "**  EqCon"
                , text "    LEFT:  " <> ppr tL
                , text "    RIGHT: " <> ppr tR
                , indent 4 $ ppr ctx0
                , empty ]

        return  ( xL
                , ctx0)


 -- EqApp
 | TApp tL1 tL2 <- tL
 , TApp tR1 tR2 <- tR
 = do
        ctrace  $ vcat
                [ text "*>  EqApp" 
                , empty ]

        ctx1    <- makeEqT config ctx0 tL1 tR1 err
        tL2'    <- applyContext ctx1 tL2
        tR2'    <- applyContext ctx1 tR2
        ctx2    <- makeEqT config ctx1 tL2' tR2' err

        ctrace  $ vcat
                [ text "*<  EqApp"
                , text "    LEFT:   " <> ppr tL
                , text "    RIGHT:  " <> ppr tR
                , indent 4 $ ppr ctx0
                , indent 4 $ ppr ctx2
                , empty ]

        return  ( xL
                , ctx2)


 -- EqEquiv
 | equivT (contextEnvT ctx0) tL tR 
 = do   ctrace  $ vcat
                [ text "**  EqEquiv" ]

        return  ( xL
                , ctx0)


 -- Error
 | otherwise
 = do   let env  = contextEnvT ctx0
        let tL'  = crushEffect env $ unpackSumT tL
        let tR'  = crushEffect env $ unpackSumT tR

        ctrace  $ vcat
                [ text "DDC.Core.Check.Judge.Eq.makeEq: no match"
                , text "  LEFT:   " <> (text $ show tL)
                , text "  RIGHT:  " <> (text $ show tR)
                , text "  LEFTC:  " <> (text $ show tL')
                , text "  RIGHTC: " <> (text $ show tR')
                , indent 2 $ ppr ctx0 ]

        throw err


-- | Unpack single element sums into plain types.
unpackSumT :: Type n -> Type n
unpackSumT (TSum ts)
        | [t]   <- Sum.toList ts = t
unpackSumT tt                    = tt

