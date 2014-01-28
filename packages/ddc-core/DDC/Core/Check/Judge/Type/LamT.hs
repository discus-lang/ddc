
module DDC.Core.Check.Judge.Type.LamT
        (checkLamT)
where
import DDC.Core.Check.Judge.Type.Sub
import DDC.Core.Check.Judge.Type.Base
import qualified DDC.Type.Sum   as Sum
import qualified Data.Set       as Set


-- Check a spec abstraction.
checkLamT :: Checker a n
checkLamT !table !ctx xx mode
 = case xx of
        XLAM a b1 x2    -> checkLAM table ctx a b1 x2 mode
        _               -> error "ddc-core.checkLamT: no match."

-- When reconstructing the type of a type abstraction,
--  the formal parameter must have a kind annotation: eg (/\v : K. x2)
checkLAM !table !ctx0 a b1 x2 Recon
 = do   let config      = tableConfig table
        let kenv        = tableKindEnv table
        let xx          = XLAM a b1 x2

        -- Check the parameter ------------------
        -- If the bound variable is named then it cannot shadow
        -- shadow others in the environment.
        when (memberKindBind b1 ctx0)
         $ throw $ ErrorLamShadow a xx b1

        -- The parameter must have an explict kind annotation.
        let kA  = typeOfBind b1
        when (isBot kA)
         $ throw $ ErrorLAMParamUnannotated a xx

        -- Check the kind annotation is well-sorted.
        (kA', sA, ctxA)
         <- checkTypeM config kenv ctx0 UniverseKind kA Recon

        let b1'         = replaceTypeOfBind kA' b1

        -- The kind annotation must have sort Comp or Prop.
        when (not (sA == sComp) && not (sA == sProp))
         $ throw $ ErrorLAMParamBadSort a xx b1 sA

        
        -- Check the body -----------------------
        let (ctx2, pos1) = markContext ctxA
        let ctx3         = pushKind b1' RoleAbstract ctx2
        let ctx4         = liftTypes 1  ctx3

        (x2', t2, e2, c2, ctx5)
         <- tableCheckExp table table ctx4 x2 Recon
        
        -- Reconstruct the kind of the body.
        (t2', k2, ctx6) 
         <- checkTypeM config kenv ctx5 UniverseSpec t2 Recon
        
        -- The type of the body must have data kind.
        when (not $ isDataKind k2)
         $ throw $ ErrorLamBodyNotData a xx b1 t2' k2

        -- The body of a spec abstraction must be pure.
        when (e2 /= Sum.empty kEffect)
         $ throw $ ErrorLamNotPure a xx UniverseSpec (TSum e2)

        -- Mask closure terms due to locally bound region vars.
        let c2_cut      = Set.fromList
                        $ mapMaybe (cutTaggedClosureT b1)
                        $ Set.toList c2

        -- Cut the bound kind and elems under it from the context.
        let ctx_cut     = lowerTypes 1
                        $ popToPos pos1 ctx6
                                   
        -- Build the result type.
        let tResult     = TForall b1' t2'

        ctrace  $ vcat
                [ text "* LAM Recon"
                , indent 2 $ ppr (XLAM a b1' x2)
                , text "  OUT: " <> ppr tResult
                , indent 2 $ ppr ctx0
                , indent 2 $ ppr ctx_cut 
                , empty ]

        returnX a
                (\z -> XLAM z b1' x2')
                tResult (Sum.empty kEffect) c2_cut
                ctx_cut


checkLAM !table !ctx0 a b1 x2 Synth
 = do   let config      = tableConfig table
        let kenv        = tableKindEnv table
        let xx          = XLAM a b1 x2

        -- Check the parameter ------------------
        -- If the bound variable is named then it cannot shadow
        -- shadow others in the environment.
        when (memberKindBind b1 ctx0)
         $ throw $ ErrorLamShadow a xx b1

        -- If the annotation is missing then make a new existential for it.
        let kA  = typeOfBind b1
        (kA', sA, ctxA)
         <- if isBot kA 
             then do   
                iA       <- newExists sComp
                let kA'  = typeOfExists iA
                let ctxA = pushExists   iA ctx0
                return (kA', sComp, ctxA)

             else
                checkTypeM config kenv ctx0 UniverseKind kA Synth

        let b1'         = replaceTypeOfBind kA' b1

        -- The kind annotation must have sort Comp or Prop.
        when (not (sA == sComp) && not (sA == sProp))
         $ throw $ ErrorLAMParamBadSort a xx b1 sA

        -- Check the body -----------------------
        let (ctx2, pos1) = markContext ctxA
        let ctx3         = pushKind b1' RoleAbstract ctx2
        let ctx4         = liftTypes 1  ctx3

        (x2', t2, e2, c2, ctx5)
         <- tableCheckExp table table ctx4 x2 Synth
        
        -- Force the kind of the body to be data.
        --  This is needed when the type of the body is an existential
        --  which doesn't yet have a resolved kind.
        (_, _, ctx6) 
         <- checkTypeM config kenv ctx5 UniverseSpec 
                (applyContext ctx5 t2) (Check kData)
        
        -- The body of a spec abstraction must be pure.
        when (e2 /= Sum.empty kEffect)
         $ throw $ ErrorLamNotPure a xx UniverseSpec (TSum e2)

        -- Mask closure terms due to locally bound region vars.
        let c2_cut      = Set.fromList
                        $ mapMaybe (cutTaggedClosureT b1)
                        $ Set.toList c2

        -- Cut the bound kind and elems under it from the context.
        let ctx_cut     = lowerTypes 1
                        $ popToPos pos1 ctx6
        
        -- Build the result type.
        let tResult     = TForall b1' t2

        ctrace  $ vcat
                [ text "* LAM Synth"
                , indent 2 $ ppr (XLAM a b1' x2)
                , text "  OUT: " <> ppr tResult
                , indent 2 $ ppr ctx0
                , indent 2 $ ppr ctx_cut 
                , empty ]

        returnX a
                (\z -> XLAM z b1' x2')
                tResult (Sum.empty kEffect) c2_cut
                ctx_cut


checkLAM !table !ctx0 a b1 x2 (Check (TForall b tBody))
 = do   let config      = tableConfig table
        let kenv        = tableKindEnv table
        let xx          = XLAM a b1 x2

        ctrace  $ vcat
                [ text "* LAM Check start"
                , ppr b
                , ppr tBody
                , ppr ctx0 ]

        -- Check the parameter ------------------
        -- If the bound variable is named then it cannot shadow
        -- shadow others in the environment.
        when (memberKindBind b1 ctx0)
         $ throw $ ErrorLamShadow a xx b1

        -- If we have an expected kind for the parameter then it needs
        -- to be the same as any existing annotation.
        let kParam      = typeOfBind b1
        let kExpected   = typeOfBind b
        when (  (not $ isBot  kParam)
             && (not $ equivT kParam kExpected))
         $ throw $ ErrorLAMParamUnexpected a xx b1 kExpected

        -- If both the kind annotation is missing and there is no
        -- expected kind then we need to make an existential for it.
        (kA', sA, ctxA)
         <- if (isBot kParam && isBot kExpected) 
             then do
                iA       <- newExists sComp
                let kA'  = typeOfExists iA
                let ctxA = pushExists   iA ctx0
                return (kA', sComp, ctxA)

             else if isBot (typeOfBind b) 
              then do
                checkTypeM config kenv ctx0 UniverseKind kParam Synth

              else do
                checkTypeM config kenv ctx0 UniverseKind kParam Synth 

        let b1' = replaceTypeOfBind kA' b1

        -- The kind annotation must have sort Comp or Prop.
        when (not (sA == sComp) && not (sA == sProp))
         $ throw $ ErrorLAMParamBadSort a xx b1 sA


        -- Check the body -----------------------
        let (ctx2, pos1) = markContext ctxA
        let ctx3         = pushKind b1' RoleAbstract ctx2
        let ctx4         = liftTypes 1  ctx3

        -- As the names used on the spec abstraction and quantifier are
        -- probably different, we use the binder name to instantiate
        -- the expected type.
        tBody_skol
         <- case takeSubstBoundOfBind b1 of
                Nothing -> return tBody
                Just u1 -> return $ substituteT b (TVar u1) tBody

        (x2', t2, e2, c2, ctx5)
         <- tableCheckExp table table ctx4 x2 (Check tBody_skol)
        
        -- Force the body of the spec abstraction must have data kind.
        --  This is needed when the type of the body is an existential
        --  which doesn't yet have a resolved kind.
        (t2', _k2, ctx6)
         <- checkTypeM config kenv ctx5 UniverseSpec t2 (Check kData)

        -- The body of a spec abstraction must be pure.
        when (e2 /= Sum.empty kEffect)
         $ throw $ ErrorLamNotPure a xx UniverseSpec (TSum e2)

        -- Mask closure terms due to locally bound region vars.
        let c2_cut      = Set.fromList
                        $ mapMaybe (cutTaggedClosureT b1)
                        $ Set.toList c2

        -- Apply context to synthesised type.
        -- We're about to pop the context back to how it was before the 
        -- type lambda, and want to keep information gained from synthing
        -- the body.
        let t2_sub      = applyContext ctx6 t2'

        -- Cut the bound kind and elems under it from the context.
        let ctx_cut     = lowerTypes 1
                        $ popToPos pos1 ctx6
        
        -- Build the result type.
        let tResult     = TForall b1' t2_sub

        ctrace  $ vcat
                [ text "* LAM"
                , indent 2 $ ppr (XLAM a b1' x2)
                , text "  OUT: " <> ppr tResult
                , indent 2 $ ppr ctx0
                , indent 2 $ ppr ctx_cut 
                , empty ]

        returnX a
                (\z -> XLAM z b1' x2')
                tResult (Sum.empty kEffect) c2_cut
                ctx_cut

checkLAM table ctx0 a b1 x2 (Check tExpected)
 = checkSub table a ctx0 (XLAM a b1 x2) tExpected

