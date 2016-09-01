
module DDC.Core.Check.Judge.Type.Sub
        (checkSub)
where
import DDC.Core.Check.Judge.Type.Base
import qualified DDC.Type.Sum           as Sum


-- This is the subtyping rule for the type checking judgment.
checkSub table !a ctx0 demand xx0 tExpect
 = do   
        ctrace  $ vcat 
                [ text "*>  Sub Check"
                , text "    demand:  " <> (text $ show demand)
                , text "    tExpect: " <> (ppr tExpect) 
                , empty ]

        let config      = tableConfig table

        -- Synthesise a type for the expression.
        (xx1, tSynth, effs1, ctx1)
         <- tableCheckExp table table ctx0 Synth demand xx0 

        -- Substitute context into synthesised and expected types.
        tSynth_ctx1     <- applyContext ctx1 tSynth
        tExpect_ctx1    <- applyContext ctx1 tExpect

        -- If the synthesised type is not quantified, 
        -- but the expected one is then instantiate it at some
        -- new existentials.
        (xx_dequant, tDequant, ctx2)
         <- dequantify table a ctx1 xx1 tSynth_ctx1 tExpect_ctx1

        ctrace  $ vcat
                [ text "*.  Sub Check"
                , text "    demand:   " <> (text $ show demand)
                , text "    tExpect:  " <> ppr tExpect_ctx1
                , text "    tSynth:   " <> ppr tSynth_ctx1
                , text "    tDequant: " <> ppr tDequant
                , empty ]

        -- Make the synthesised type a subtype of the expected one.
        (xx2, effs3, ctx3)
         <- makeSub config a ctx2 xx_dequant tDequant tExpect_ctx1
         $  ErrorMismatch  a tDequant tExpect_ctx1 xx0

        let effs' = Sum.union effs1 effs3

        ctrace  $ vcat
                [ text "*<  Sub"
                , indent 4 $ ppr xx0
                , text "    tExpect:  " <> ppr tExpect
                , text "    tSynth:   " <> ppr tSynth
                , text "    tDequant: " <> ppr tDequant
                , text "    tExpect': " <> ppr tExpect_ctx1
                , text "    tSynth':  " <> ppr tSynth_ctx1
                , indent 4 $ ppr ctx0
                , indent 4 $ ppr ctx1
                , indent 4 $ ppr ctx3
                , empty ]

        returnX a
                (\_ -> xx2)
                tExpect
                effs' ctx3


dequantify !_table !aApp ctx0 xx0 tSynth tExpect 
 | TCon (TyConExists _n _k)  <- tExpect
 , shouldDequantifyX xx0
 = do   
        (bsParam, tBody)     <- stripQuantifiers tSynth
        case bsParam of
         []     -> return (xx0, tSynth, ctx0)
         _      -> addTypeApps aApp ctx0 xx0 (reverse bsParam) tBody

 | otherwise
 = return (xx0, tSynth, ctx0)


shouldDequantifyX :: Exp a n -> Bool
shouldDequantifyX xx
 = case xx of
        XLAM{}  -> False
        _       -> True


-- | Add applications of type existentials to the given expression.
addTypeApps 
        :: Ord n
        => a                    -- ^ Annotation for new AST nodes.
        -> Context n            -- ^ Current type checker context.
        -> Exp (AnTEC a n) n    -- ^ Expression to add type applications to.
        -> [Bind n]             -- ^ Forall quantifiers.
        -> Type n               -- ^ Body of the forall.
        -> CheckM a n 
                ( Exp (AnTEC a n) n
                , Type n
                , Context n)

addTypeApps !_aApp ctx0 xx0 [] tBody
 = return (xx0, tBody, ctx0)

addTypeApps !aApp ctx0 xx0 (bParam : bsParam) tBody
 = do   
        let kParam = typeOfBind bParam

        (xx1, tBody', ctx1)
         <- addTypeApps aApp ctx0 xx0 bsParam tBody 

        iArg      <- newExists kParam
        let tArg  =  typeOfExists iArg
        let ctx2  =  pushExists iArg ctx1

        let tResult = substituteT bParam tArg tBody'

        let aApp' = AnTEC tResult (tBot kEffect) (tBot kClosure) aApp
        let aArg' = AnTEC kParam  (tBot kEffect) (tBot kClosure) aApp
        let xx2   = XApp aApp' xx1 (XType aArg' tArg)

        return (xx2, tResult, ctx2)


-- | Strip quantifiers from the front of a type.
--   TODO: need to look through type synonyms when checking for foralls.
stripQuantifiers :: Type n -> CheckM a n ([Bind n], Type n)
stripQuantifiers tt
 = case tt of
        TForall bParam tBody
         -> do  (bsParam, tBody')
                 <- stripQuantifiers tBody
                return (bParam : bsParam, tBody')

        _ ->    return ([], tt)

