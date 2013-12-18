
module DDC.Core.Check.Judge.Sub
        ( makeSub)
where
import DDC.Type.Transform.SubstituteT
import DDC.Core.Annot.AnTEC
import DDC.Core.Check.Judge.Eq
import DDC.Core.Check.Judge.Inst
import DDC.Core.Check.Base


-- | Make the left type a subtype of the right type,
--   or throw the provided error if this is not possible.
makeSub :: (Eq n, Ord n, Pretty n)
        => a
        -> Error a n
        -> Context n
        -> Exp  (AnTEC a n) n
        -> Type n
        -> Type n
        -> CheckM a n 
                ( Exp (AnTEC a n) n
                , Context n)

-- NOTE: The order of cases matters here.
--       For example, we do something different when both sides are
--       existentials, vs the case when only one side is an existential.
makeSub a err ctx0 xL tL tR

 -- SubCon
 --  Both sides are the same type constructor.
 | TCon tc1     <- tL
 , TCon tc2     <- tR
 , tc1 == tc2
 = do   
        ctrace  $ vcat
                [ text "* SubCon"
                , text "  LEFT:  " <> ppr tL
                , text "  RIGHT: " <> ppr tR
                , indent 2 $ ppr ctx0
                , empty ]

        return (xL, ctx0)


 -- SubVar
 --  Both sides are the same (rigid) type variable.
 | TVar u1      <- tL
 , TVar u2      <- tR
 , u1 == u2
 = do   
        ctrace  $ vcat
                [ text "* SubVar"
                , text "  LEFT:  " <> ppr tL
                , text "  RIGHT: " <> ppr tR
                , indent 2 $ ppr ctx0
                , empty ]

        return (xL, ctx0)


 -- SubExVar
 --  Both sides are the same existential.
 | Just iL <- takeExists tL
 , Just iR <- takeExists tR
 , iL == iR
 = do   
        ctrace  $ vcat
                [ text "* SubExVar"
                , text "  LEFT:  " <> ppr tL
                , text "  RIGHT: " <> ppr tR
                , indent 2 $ ppr ctx0 
                , empty ]

        return (xL, ctx0)


 -- SubInstL
 --  Left is an existential.
 --  TODO: do free variables check  tL /= FV(tR)
 | isTExists tL
 = do   ctx1    <- makeInst a err ctx0 tR tL

        ctrace  $ vcat
                [ text "* SubInstL"
                , text "  LEFT:   " <> ppr tL
                , text "  RIGHT:  " <> ppr tR
                , indent 2 $ ppr ctx0
                , indent 2 $ ppr ctx1
                , empty ]

        return (xL, ctx1)


 -- SubInstR
 --  Right is an existential.
 --  TODO: do free variables check  tR /= FV(tL)
 | isTExists tR
 = do   ctx1    <- makeInst a err ctx0 tL tR

        ctrace  $ vcat
                [ text "* SubInstR"
                , text "  LEFT:  " <> ppr tL
                , text "  RIGHT: " <> ppr tR
                , indent 2 $ ppr ctx0
                , indent 2 $ ppr ctx1 
                , empty ]

        return (xL, ctx1)


 -- SubArr
 --  Both sides are arrow types.
 | Just (tL1, tL2)      <- takeTFun tL
 , Just (tR1, tR2)      <- takeTFun tR
 = do   
        -- TODO: will need to eta-expand to pass type applications
        --       when instantiating higher ranked types.
        (_, ctx1)   <- makeSub a err ctx0 xL tR1 tL1
        let tL2'    =  applyContext  ctx1    tL2
        let tR2'    =  applyContext  ctx1    tR2
        (_, ctx2)   <- makeSub a err ctx1 xL tL2' tR2'

        ctrace  $ vcat
                [ text "* SubArr"
                , text "  LEFT:  " <> ppr tL
                , text "  RIGHT: " <> ppr tR
                , indent 2 $ ppr ctx0
                , indent 2 $ ppr ctx1
                , indent 2 $ ppr ctx2 
                , empty ]

        return (xL, ctx2)


 -- SubApp 
 --   Both sides are type applications.
 --   Assumes non-function type constructors are invariant.
 | TApp tL1 tL2 <- tL
 , TApp tR1 tR2 <- tR
 = do   
        ctx1     <- makeEq a err ctx0 tL1 tR1
        let tL2' =  applyContext ctx1 tL2
        let tR2' =  applyContext ctx1 tR2
        ctx2     <- makeEq a err ctx1 tL2' tR2'

        ctrace  $ vcat
                [ text "* SubApp"
                , text "  LEFT:  " <> ppr tL
                , text "  RIGHT: " <> ppr tR
                , indent 2 $ ppr ctx0
                , indent 2 $ ppr ctx1
                , indent 2 $ ppr ctx2 
                , empty ]

        return (xL, ctx2)


 -- SubForall
 --   Left side is a forall type.
 | TForall b t1 <- tL
 = do   
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
        (xL1, ctx3)      <- makeSub a err ctx2 xL t1' tR

        -- Pop the existential and constraints above it back off
        -- the stack.
        let ctx4  = popToPos pos1 ctx3

        ctrace  $ vcat
                [ text "* SubForall"
                , text "  LEFT:  " <> ppr tL
                , text "  RIGHT: " <> ppr tR
                , indent 2 $ ppr ctx0
                , indent 2 $ ppr ctx4 
                , empty ]

        -- Wrap the expression with a type application to cause
        -- the instantiation.
        -- TODO: Substitute into e0 and c0. 
        -- We've added a type application here.
        let AnTEC _ e0 c0 _    
                 = annotOfExp xL
        let aFn  = AnTEC t1' e0 c0 a
        let aArg = AnTEC (typeOfBind b) (tBot kEffect) (tBot kClosure) a
        let xL2  = XApp aFn xL1 (XType aArg tA) 

        return (xL2, ctx4)


 -- Error
 | otherwise
 = do   ctrace  $ vcat
                [ text "DDC.Core.Check.Exp.Inst.makeSub: no match" 
                , text "  LEFT:   " <> ppr tL
                , text "  RIGHT:  " <> ppr tR ]

        throw err

