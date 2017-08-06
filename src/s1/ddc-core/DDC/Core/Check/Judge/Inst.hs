
module DDC.Core.Check.Judge.Inst
        ( makeInstL
        , makeInstR)
where
import DDC.Core.Check.Base
import DDC.Core.Check.Judge.Kind


---------------------------------------------------------------------------------------------------
-- | Instantiate an existential so it becomes a subtype of the given type.
makeInstL
        :: (Eq n, Ord n, Pretty n, Show n)
        => Config n     -- ^ Checker configuration.
        -> a            -- ^ Annotation on the AST node being checker, for error reporting.
        -> Context n    -- ^ Checker context.
        -> Exists n     -- ^ Instantiate this existential...
        -> Type n       -- ^   ... to be a subtype of this one.
        -> Error a n    -- ^ Error to throw if the instantiation does not work.
        -> CheckM a n (Context n)

makeInstL !config !a !ctx0 !iL !tR !err

 -- InstLReach
 --  Both types are existentials, and the left is bound earlier in the stack.
 --  CAREFUL: The returned location is relative to the top of the stack,
 --           hence we need lL > lR here.
 | Just lL <- locationOfExists iL ctx0
 , Just iR <- takeExists tR
 , Just lR <- locationOfExists iR ctx0
 = do
        let Just ctx1
                = if lL > lR
                        then updateExists [] iR (typeOfExists iL) ctx0
                        else updateExists [] iL (typeOfExists iR) ctx0

        ctrace  $ vcat
                [ text "**  InstLReach"
                , text "    LEFT:  " <> ppr iL
                , text "    RIGHT: " <> ppr tR
                , indent 4 $ ppr ctx0
                , indent 4 $ ppr ctx1
                , empty ]

        return ctx1

 -- InstLArr
 --  Left is an existential, right is a function arrow.
 | Just (tR1, tR2)      <- takeTFun tR
 = do
        -- Make new existentials to match the function type and parameter.
        iL1     <- newExists kData
        let tL1 =  typeOfExists iL1

        iL2     <- newExists kData
        let tL2 =  typeOfExists iL2

        -- Update the context with the new constraint.
        let Just ctx1   =  updateExists [iL2, iL1] iL (tFun tL1 tL2) ctx0

        -- Instantiate the parameter type.
        ctx2    <- makeInstR config a ctx1 tR1 iL1 err

        -- Substitute into tR2
        tR2'    <- applyContext ctx2 tR2

        -- Instantiate the return type.
        ctx3    <- makeInstL config a ctx2 iL2 tR2' err

        ctrace  $ vcat
                [ text "**  InstLArr"
                , text "    LEFT:  " <> ppr iL
                , text "    RIGHT: " <> ppr tR
                , indent 4 $ ppr ctx0
                , indent 4 $ ppr ctx3
                , empty ]

        return ctx3

 -- InstLTApp
 | (tCon : tArgs)       <- takeTApps tR
 = do
        ctrace  $ vcat
                [ text "*>  InstLTyConApp"
                , text "    LEFT:    " <> ppr iL
                , text "    RIGHT:   " <> ppr tR
                , indent 4 $ ppr ctx0
                , empty ]

        (_, kCon, _)    <- checkTypeM config ctx0 UniverseSpec tCon Recon
        let (ksParam, kResult) = takeKFuns kCon

        -- Make new existentials to match the function and argument parts.
        isParam         <- mapM newExists ksParam
        let tsParam     =  map typeOfExists isParam

        -- Update the context with the new constraint.
        let tInst       = tApps tCon tsParam
        let Just ctx1   = updateExists isParam iL tInst ctx0

        -- Instantiate the parameter types.
        -- ISSUE #438: Type checker instantiation judgment assume all tycon params
        -- are covariant. We don't have real variance info about the tycon,
        -- so just use makeInstL, which assumes all the params are covariant.
        let go ctxAcc [] _  = return ctxAcc
            go ctxAcc _  [] = return ctxAcc
            go ctxAcc (iParam : isParam') (tRArg : tRArgs')
                = do    tRArg'  <- applyContext ctxAcc tRArg
                        ctxAcc' <- makeInstL config a ctxAcc iParam tRArg' err
                        go ctxAcc' isParam' tRArgs'

        ctx2    <- go ctx1 isParam tArgs

        ctrace  $ vcat
                [ text "*<  InstLTyConApp"
                , text "    LEFT:    " <> ppr iL
                , text "    RIGHT:   " <> ppr tR
                , text "    kCon:    " <> ppr kCon
                , text "    ksParam: " <> ppr ksParam
                , text "    kResult: " <> ppr kResult
                , text "    tInst:   " <> ppr tInst
                , indent 4 $ ppr ctx0
                , indent 4 $ ppr ctx2
                , empty ]

        return ctx2


 -- InstLSolve
 | not $ isTExists tR
 = do   let Just ctx1   = updateExists [] iL tR ctx0

        ctrace  $ vcat
                [ text "**  InstLSolve"
                , text "    LEFT:  " <> ppr iL
                , text "    RIGHT: " <> ppr tR
                , indent 4 $ ppr ctx0
                , indent 4 $ ppr ctx1
                , empty ]

        return ctx1

 -- Error
 | otherwise
 = do
        ctrace  $ vcat
                [ text "DDC.Core.Check.Exp.Inst.makeInstL: no match"
                , text "  LEFT:  " <> ppr iL
                , text "  RIGHT: " <> ppr tR
                , indent 2 $ ppr ctx0
                , empty ]

        throw err


---------------------------------------------------------------------------------------------------
-- | Instantiate an existential so it becomes a supertype of the given type.
makeInstR
        :: (Eq n, Ord n, Pretty n, Show n)
        => Config n     -- ^ Checker configuration.
        -> a            -- ^ Annotation on the AST node being checker, for error reporting.
        -> Context n    -- ^ Checker context.
        -> Type n       -- ^ Use this type as a subtype
        -> Exists n     --   .. to instantiate this existential.
        -> Error a n    -- ^ Error to throw if the instantiation does not work.
        -> CheckM a n (Context n)

makeInstR !config !a !ctx0 !tL !iR !err

 -- InstRReach
 --  Both types are existentials, and the right is bound earlier in the stack.
 --  CAREFUL: The returned location is relative to the top of the stack,
 --           hence we need lR > lL here.
 | Just iL <- takeExists tL
 , Just lL <- locationOfExists iL ctx0
 , Just lR <- locationOfExists iR ctx0
 = do
        let Just ctx1
                = if lR > lL
                        then updateExists [] iL (typeOfExists iR) ctx0
                        else updateExists [] iR (typeOfExists iL) ctx0

        ctrace  $ vcat
                [ text "**  InstRReach"
                , text "    LEFT:  " <> ppr tL
                , text "    RIGHT: " <> ppr iR
                , indent 4 $ ppr ctx0
                , indent 4 $ ppr ctx1
                , empty ]

        return ctx1

 -- InstRSolve
 | not $ isTExists tL
 = do   let Just ctx1   = updateExists [] iR tL ctx0

        ctrace  $ vcat
                [ text "**  InstRSolve"
                , text "    LEFT:  " <> ppr tL
                , text "    RIGHT: " <> ppr iR
                , indent 4 $ ppr ctx0
                , indent 4 $ ppr ctx1
                , empty ]

        return ctx1

 -- InstRArr
 --  Left is an function arrow, and right is an existential.
 | Just (tL1, tL2)      <- takeTFun tL
 = do
        -- Make new existentials to match the function type and parameter.
        iR1     <- newExists kData
        let tR1 =  typeOfExists iR1

        iR2     <- newExists kData
        let tR2 =  typeOfExists iR2

        -- Update the context with the new constraint.
        let Just ctx1   =  updateExists [iR2, iR1] iR (tFun tR1 tR2) ctx0

        -- Instantiate the parameter type.
        ctx2    <- makeInstL config a ctx1 iR1 tL1 err

        -- Substitute into tL2
        tL2'    <- applyContext ctx2 tL2

        -- Instantiate the return type.
        ctx3    <- makeInstR config a ctx2 tL2' iR2 err

        ctrace  $ vcat
                [ text "**  InstRArr"
                , text "    LEFT:  " <> ppr tL
                , text "    RIGHT: " <> ppr iR
                , indent 4 $ ppr ctx0
                , indent 4 $ ppr ctx3
                , empty ]

        return ctx3

 -- InstRTApp
 | (tCon : tArgs)       <- takeTApps tL
 = do
        ctrace  $ vcat
                [ text "*>  InstRTyConApp"
                , text "    LEFT:    " <> ppr iR
                , text "    RIGHT:   " <> ppr tL
                , indent 4 $ ppr ctx0
                , empty ]

        (_, kCon, _)    <- checkTypeM config ctx0 UniverseSpec tCon Recon
        let (ksParam, kResult) = takeKFuns kCon

        -- Make new existentials to match the function and argument parts.
        isParam         <- mapM newExists ksParam
        let tsParam     =  map typeOfExists isParam

        -- Update the context with the new constraint.
        let tInst       = tApps tCon tsParam
        let Just ctx1   = updateExists isParam iR tInst ctx0

        -- Instantiate the parameter types.
        -- ISSUE #438: Type checker instantiation judgment assume all tycon params
        -- are covariant. We don't have real variance info about the tycon,
        -- so just use makeInstR, which assumes all the params are covariant.
        let go ctxAcc [] _  = return ctxAcc
            go ctxAcc _  [] = return ctxAcc
            go ctxAcc (iParam : isParam') (tRArg : tRArgs')
                = do    tRArg'  <- applyContext ctxAcc tRArg
                        ctxAcc' <- makeInstR config a ctxAcc tRArg' iParam err
                        go ctxAcc' isParam' tRArgs'

        ctx2    <- go ctx1 isParam tArgs

        ctrace  $ vcat
                [ text "*<  InstRTyConApp"
                , text "    LEFT:    " <> ppr tL
                , text "    RIGHT:   " <> ppr iR
                , text "    kCon:    " <> ppr kCon
                , text "    ksParam: " <> ppr ksParam
                , text "    kResult: " <> ppr kResult
                , text "    tInst:   " <> ppr tInst
                , indent 4 $ ppr ctx0
                , indent 4 $ ppr ctx2
                , empty ]

        return ctx2

 -- Error
 | otherwise
 = do
        ctrace  $ vcat
                [ text "DDC.Core.Check.Exp.Inst.makeInstR: no match"
                , text "  LEFT:  " <> ppr tL
                , text "  RIGHT: " <> ppr iR
                , indent 2 $ ppr ctx0
                , empty ]

        throw err
