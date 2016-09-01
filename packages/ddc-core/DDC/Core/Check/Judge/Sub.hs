
module DDC.Core.Check.Judge.Sub
        (makeSub)
where
import DDC.Type.Transform.SubstituteT
import DDC.Core.Exp.Annot.AnTEC
import DDC.Core.Check.Judge.EqX
import DDC.Core.Check.Judge.Inst
import DDC.Core.Check.Base
import qualified DDC.Core.Check.Context as Context
import qualified DDC.Core.Env.EnvT      as EnvT
import qualified Data.Map.Strict        as Map
import qualified DDC.Type.Sum           as Sum


-- | Make the left type a subtype of the right type,
--   or throw the provided error if this is not possible.
--
--   The inferred type may already be a subtype of the expected type,
--   and in that case we don't need to do anything extra.
--
--   If the inferred type is a 'S e a' computation type and the expected
--   type is 'a' then we can force the inferred type to be the expected one
--   by running the computation. In this case we end up with more effects.
--
makeSub :: (Eq n, Ord n, Show n, Pretty n)
        => Config n                     -- ^ Type checker configuration.
        -> a                            -- ^ Current annotation.
        -> Context n                    -- ^ Input context.
        -> Exp  (AnTEC a n) n           -- ^ Expression that we've inferred the type of.
        -> Type n                       -- ^ Inferred type of the expression.
        -> Type n                       -- ^ Expected type of the expression.
        -> Error a n                    -- ^ Error to throw if we can't force subsumption.
        -> CheckM a n
                ( Exp (AnTEC a n) n     --   Expression after instantiations and running.
                , TypeSum n             --   More effects we might get from running the computation.
                , Context n)            --   Output context.

-- NOTE: The order of cases matters here.
--       For example, we do something different when both sides are
--       existentials, vs the case when only one side is an existential.
makeSub config a ctx0 xL tL tR err

 -- Expand type equations.
 | TCon (TyConBound (UName n) _) <- tL
 , Just tL'  <- Map.lookup n $ EnvT.envtEquations
                             $ Context.contextEnvT ctx0
 = makeSub config a ctx0 xL tL' tR err


 | TCon (TyConBound (UName n) _) <- tR
 , Just tR'  <- Map.lookup n $ EnvT.envtEquations
                             $ Context.contextEnvT ctx0
 = makeSub config a ctx0 xL tL tR' err


 -- SubExVar
 --  Both sides are the same existential.
 | Just iL <- takeExists tL
 , Just iR <- takeExists tR
 , iL == iR
 = do
        ctrace  $ vcat
                [ text "**  SubExVar"
                , text "    xL: " <> ppr xL
                , text "    tL: " <> ppr tL
                , text "    tR: " <> ppr tR
                , indent 4 $ ppr ctx0
                , empty ]

        return  ( xL
                , Sum.empty kEffect
                , ctx0)


 -- SubInstL
 --  Left is an existential.
 | isTExists tL
 = do   ctx1    <- makeInst config a ctx0 tR tL err

        ctrace  $ vcat
                [ text "**  SubInstL"
                , text "    xL: " <> ppr xL
                , text "    tL: " <> ppr tL
                , text "    tR: " <> ppr tR
                , indent 4 $ ppr ctx0
                , indent 4 $ ppr ctx1
                , empty ]

        return  ( xL
                , Sum.empty kEffect
                , ctx1)


 -- SubInstR
 --  Right is an existential.
 | isTExists tR
 = do   ctx1    <- makeInst config a ctx0 tL tR err

        ctrace  $ vcat
                [ text "**  SubInstR"
                , text "    xL: " <> ppr xL
                , text "    tL: " <> ppr tL
                , text "    tR: " <> ppr tR
                , indent 4 $ ppr ctx0
                , indent 4 $ ppr ctx1
                , empty ]

        return  ( xL
                , Sum.empty kEffect
                , ctx1)


 -- SubCon
 --  Both sides are the same type constructor.
 | TCon tc1     <- tL
 , TCon tc2     <- tR
 , equivTyCon tc1 tc2
 = do
        ctrace  $ vcat
                [ text "**  SubCon"
                , text "    xL: " <> ppr xL
                , text "    tL: " <> ppr tL
                , text "    tR: " <> ppr tR
                , indent 4 $ ppr ctx0
                , empty ]

        return  ( xL
                , Sum.empty kEffect
                , ctx0)


 -- SubVar
 --  Both sides are the same (rigid) type variable.
 | TVar u1      <- tL
 , TVar u2      <- tR
 , u1 == u2
 = do
        ctrace  $ vcat
                [ text "**  SubVar"
                , text "    xL: " <> ppr xL
                , text "    tL: " <> ppr tL
                , text "    tR: " <> ppr tR
                , indent 4 $ ppr ctx0
                , empty ]

        return  ( xL
                , Sum.empty kEffect
                , ctx0)


 -- SubEquiv
 --  Both sides are equivalent
 | equivT (contextEnvT ctx0) tL tR
 = do
        ctrace  $ vcat
                [ text "**  SubEquiv"
                , text "    xL: " <> ppr xL
                , text "    tL: " <> ppr tL
                , text "    tR: " <> ppr tR
                , indent 4 $ ppr ctx0
                , empty ]

        return  ( xL
                , Sum.empty kEffect
                , ctx0)


 -- SubArr
 --  Both sides are arrow types.
 | Just (tL1, tL2)  <- takeTFun tL
 , Just (tR1, tR2)  <- takeTFun tR
 = do
        ctrace  $ vcat
                [ text "*>  SubArr"
                , empty ]

        (_, effs1, ctx1) <- makeSub config a ctx0 xL tR1 tL1 err
        tL2'             <- applyContext     ctx1 tL2
        tR2'             <- applyContext     ctx1 tR2
        (_, effs2, ctx2) <- makeSub config a ctx1 xL tL2' tR2' err

        ctrace  $ vcat
                [ text "*<  SubArr"
                , text "    xL: " <> ppr xL
                , text "    tL: " <> ppr tL
                , text "    tR: " <> ppr tR
                , indent 4 $ ppr ctx0
                , indent 4 $ ppr ctx1
                , indent 4 $ ppr ctx2
                , empty ]

        return  ( xL
                , Sum.union effs1 effs2
                , ctx2)


 -- SubApp
 --   Both sides are type applications.
 --   Assumes non-function type constructors are invariant.
 | TApp tL1 tL2 <- tL
 , TApp tR1 tR2 <- tR
 = do
        ctrace  $ vcat
                [ text "*>  SubApp"
                , empty ]

        (xL1, ctx1)     <- makeEqX config a ctx0 xL  tL1  tR1  err
        tL2'            <- applyContext ctx1 tL2
        tR2'            <- applyContext ctx1 tR2
        (xL2, ctx2)     <- makeEqX config a ctx1 xL1 tL2' tR2' err

        ctrace  $ vcat
                [ text "*<  SubApp"
                , text "    xL: " <> ppr xL
                , text "    tL: " <> ppr tL
                , text "    tR: " <> ppr tR
                , indent 4 $ ppr ctx0
                , indent 4 $ ppr ctx1
                , indent 4 $ ppr ctx2
                , empty ]

        return  ( xL2
                , Sum.empty kEffect
                , ctx2)


 -- SubForall-L
 --   Left side is a forall type.
 | TForall b t1 <- tL
 = do
        ctrace  $ vcat
                [ text "*>  SubForallL"
                , text "    xL:     " <> ppr xL
                , text "    LEFT:   " <> ppr tL
                , text "    RIGHT:  " <> ppr tR
                , empty ]

        -- Make a new existential to instantiate the quantified
        -- variable and substitute it into the body.
        iA        <- newExists (typeOfBind b)
        let tA    = typeOfExists iA
        let t1'   = substituteT b tA t1

        -- Check the new body against the right type,
        -- so that the existential we just made is instantiated
        -- to match the right.
        let (ctx1, pos1) =  markContext ctx0
        let ctx2         =  pushExists  iA ctx1

        -- Wrap the expression with a type application to cause
        -- the instantiation.
        let AnTEC _ e0 c0 _
                 = annotOfExp xL
        let aFn  = AnTEC t1' (substituteT b tA e0) (substituteT b tA c0) a
        let aArg = AnTEC (typeOfBind b) (tBot kEffect) (tBot kClosure) a
        let xL1  = XApp aFn xL (XType aArg tA)

        (xL2, effs3, ctx3) <- makeSub config a ctx2 xL1 t1' tR err

        -- Pop the existential and constraints above it back off
        -- the stack.
        let ctx4  = popToPos pos1 ctx3

        ctrace  $ vcat
                [ text "*<  SubForall"
                , text "    xL:    " <> ppr xL
                , text "    LEFT:  " <> ppr tL
                , text "    RIGHT: " <> ppr tR
                , text "    xL2:   " <> ppr xL2
                , indent 4 $ ppr ctx0
                , indent 4 $ ppr ctx4
                , empty ]

        return  ( xL2
                , effs3
                , ctx4)


 -- SubForall-R
 | TForall bParamR tBodyR  <- tR
 = do
        ctrace  $ vcat
                [ text "*>  SubForallR"
                , text "    xL:    " <> ppr xL
                , text "    LEFT:  " <> ppr tL
                , text "    RIGHT: " <> ppr tR
                , empty ]

        -- Make a new existential to instantiate the quantified
        -- variable and substitute it into the body.
        let Just uParam = takeSubstBoundOfBind bParamR
        let tA          = TVar uParam
        let tBodyR'     = substituteT bParamR tA tBodyR

        -- Check the new body against the left type,
        -- so that the existential is instantiated
        -- to match the left.
        let (ctx1, pos1)  =  markContext ctx0
        let ctx2          =  pushType bParamR ctx1

        (xL2, eff2, ctx3) <- makeSub config a ctx2 xL tL tBodyR' err
        when (not $ eff2 == Sum.empty kEffect)
         $ error "makeSub: body is not pure"

        let tR'           =  TForall bParamR tBodyR      
                                -- TODO: this will be the wrong tBodyR
                                -- need to get the type produced by makeSub
        let aApp          =  AnTEC tR' (tBot kEffect) (tBot kClosure) a
        let xL_abs        =  XLAM aApp bParamR xL2

        -- Pop the existential and constraints above it off the stack.
        let ctx4        = popToPos pos1 ctx3

        ctrace  $ vcat
                [ text "*<  SubForallR"
                , text "    xL:     " <> ppr xL
                , text "    LEFT:   " <> ppr tL
                , text "    RIGHT:  " <> ppr tR
                , text "    xL_abs: " <> ppr xL_abs
                , empty ]

        return  ( xL_abs
                , Sum.empty kEffect
                , ctx4)

 -- SubRun
 | Just (tEffect, tResult) <- takeTSusp tL
 = do   
        ctrace  $ vcat
                [ text "**  SubRun"
                , text "    xL:      " <> ppr xL
                , text "    tL:      " <> ppr tL
                , text "    tEffect: " <> ppr tEffect
                , text "    tResult: " <> ppr tResult
                , empty ]

        let aRun    = AnTEC tResult tEffect (tBot kClosure) a
        let xL_run  = XCast aRun CastRun xL

        (xL2, eff2, ctx2) <- makeSub config a ctx0 xL_run tResult tR err

        let eff = Sum.unions    kEffect
                [ Sum.singleton kEffect tEffect
                , eff2 ]

        return  ( xL2
                , eff
                , ctx2)

 -- Error
 | otherwise
 = do   ctrace  $ vcat
                [ text "DDC.Core.Check.Exp.Inst.makeSub: no match"
                , text "  LEFT:   " <> ppr tL
                , text "  RIGHT:  " <> ppr tR ]

        throw err

