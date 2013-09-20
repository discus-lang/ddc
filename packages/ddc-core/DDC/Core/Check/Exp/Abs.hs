
module DDC.Core.Check.Exp.Abs
        (checkAbs)
where
import DDC.Core.Check.Exp.Inst
import DDC.Core.Check.Exp.Base
import qualified DDC.Type.Sum   as Sum
import qualified Data.Set       as Set


-- Dispatch -------------------------------------------------------------------
checkAbs :: Checker a n
checkAbs !table !ctx (XLAM a b1 x2) _
 = do   checkAbsLAM table ctx a b1 x2
         
checkAbs !table !ctx xx@(XLam a b1 x2) dXX
 -- If there is no annotation then assume it should be a data binding.
 | isBot (typeOfBind b1)
 = checkAbsLamData table a ctx b1 (tBot kData) x2 dXX

 -- Otherwise decide what to do based on the kind.
 --  TODO: merge checkAbsLamData and checkAbsLamWitness into same case.
 | otherwise
 = do   let config      = tableConfig table
        let kenv        = tableKindEnv table

        -- The rule we use depends on the kind of the binder.
        (b1', k1)       <- checkBindM config kenv ctx b1

        case universeFromType2 k1 of
         Just UniverseData
           -> checkAbsLamData    table a ctx b1' k1 x2 dXX
         Just UniverseWitness
           -> checkAbsLamWitness table a ctx b1' k1 x2 dXX
         _ -> throw $ ErrorLamBindBadKind a xx (typeOfBind b1') k1

checkAbs _ _ _ _
        = error "ddc-core.checkAbs: no match."


-- AbsLAM ---------------------------------------------------------------------
checkAbsLAM !table !ctx a b1 x2
 = do   let config      = tableConfig table
        let kenv        = tableKindEnv table
        let xx          = XLAM a b1 x2
        
        -- Check the type of the binder.
        (b1', _)        <- checkBindM config kenv ctx b1

        -- If the bound variable is named then it cannot shadow
        -- shadow others in the environment.
        when (memberKindBind b1' ctx)
         $ throw $ ErrorLamShadow a xx b1
        
        -- Check the body of the abstraction.
        let (ctx1, pos1) = pushKind b1' ctx
        let ctx2         = liftTypes 1  ctx1
        (x2', t2, e2, c2, ctx3)
                <- tableCheckExp table table ctx2 x2 Recon
        (_, k2) <- checkTypeM config kenv ctx3 t2

        -- The body of a spec abstraction must have data kind.
        when (not $ isDataKind k2)
         $ throw $ ErrorLamBodyNotData a xx b1 t2 k2

        -- The body of a spec abstraction must be pure.
        when (e2 /= Sum.empty kEffect)
         $ throw $ ErrorLamNotPure a xx UniverseSpec (TSum e2)

        -- Mask closure terms due to locally bound region vars.
        let c2_cut      = Set.fromList
                        $ mapMaybe (cutTaggedClosureT b1)
                        $ Set.toList c2

        -- Cut the bound kind and elems under it from the context.
        let Just ctx'   = liftM (lowerTypes 1)
                        $ popToPos pos1 ctx3
                                                                
        returnX a
                (\z -> XLAM z b1' x2')
                (TForall b1' t2)
                (Sum.empty kEffect)
                c2_cut
                ctx'


-- AbsLamData -----------------------------------------------------------------
-- When reconstructing the type of a lambda abstraction,
--  the formal parameter must have a type annotation: eg (\v : T. x2)
checkAbsLamData !table !a !ctx !b1 !_k1 !x2 !Recon
 = do   let config      = tableConfig table
        let kenv        = tableKindEnv table
        let t1          = typeOfBind b1
        let xx          = XLam a b1 x2

        -- The formal parameter must have a type annotation.
        when (isBot t1)
         $ throw $ ErrorLamParamTypeMissing a xx b1

        -- Reconstruct a type for the body, under the extended environment.
        let (ctx1, pos1) = pushType b1 ctx
        (x2', t2, e2, c2, ctx2)
         <- tableCheckExp table table ctx1 x2 Recon

        -- The body of the function must produce data.
        (_, k2)         <- checkTypeM config kenv ctx2 t2
        when (not $ isDataKind k2)
         $ throw $ ErrorLamBodyNotData a xx b1 t2 k2 

        -- Cut closure terms due to locally bound value vars.
        -- This also lowers deBruijn indices in un-cut closure terms.
        let c2_cut      = Set.fromList
                        $ mapMaybe (cutTaggedClosureX b1)
                        $ Set.toList c2

        -- Build the resulting function type.
        --   The way the effect and closure term is captured depends on
        --   the configuration flags.
        (tResult, cResult)
         <- makeFunctionType config a xx t1 t2 e2 c2_cut

        -- Cut the bound type and elems under it from the context.
        let Just ctx'   = popToPos pos1 ctx2
        
        returnX a
                (\z -> XLam z b1 x2')
                tResult 
                (Sum.empty kEffect)
                cResult
                ctx'


-- When synthesizing the type of a lambda abstraction
--   we produce a type (?1 -> ?2) with new unification variables.
--
--   TOOD: If there was a parameter type annot, then use that instead,
--         and just synthesize a type for the body.
--
checkAbsLamData !table !a !ctx !b1 !_k1 !x2 !Synth
 = do   let config      = tableConfig table

        -- Existential for the parameter type.
        tA      <- newExists
        let b1' = replaceTypeOfBind tA b1

        -- Existential for the result type.
        tB      <- newExists

        -- Check the body against the existential for it.
        let (ctxA, _)    = pushExists tA ctx
        let (ctxB, _)    = pushExists tB ctxA
        let (ctx1, pos1) = pushType  b1' ctxB
        (x2', _, e2, c2, ctx2)
         <- tableCheckExp table table ctx1 x2 (Check tB)

        -- Cut closure terms due to locally bound value vars.
        -- This also lowers deBruijn indices in un-cut closure terms.
        let c2_cut      = Set.fromList
                        $ mapMaybe (cutTaggedClosureX b1')
                        $ Set.toList c2

        -- Build the resulting function type.
        --   The way the effect and closure term is captured depends on
        --   the configuration flags.
        (tResult, cResult)
         <- makeFunctionType config a (XLam a b1 x2) tA tB e2 c2_cut

        -- Cut the bound type and elems under it from the context.
        let Just ctx'   = popToPos pos1 ctx2
        
        returnX a
                (\z -> XLam z b1' x2')
                tResult 
                (Sum.empty kEffect)
                cResult
                ctx'

-- When checking type type of a lambda abstraction against an existing
--   functional type we allow the formal paramter to be missing its
--   type annotation, and in this case we replace it with the expected type.
checkAbsLamData !table !a !ctx !b1 !_k1 !x2 !(Check tXX)
 | Just (tX1, tX2)      <- takeTFun tXX
 = do   let config      = tableConfig table
        let t1          = typeOfBind b1
        let xx          = XLam a b1 x2

        -- If the parameter has no type annotation then we can use the
        --   expected type we were passed down from above.
        -- If it does have an annotation, then it needs to match the
        --   expected type.
        b1' <- if isBot t1             
                then return (replaceTypeOfBind tX1 b1)
               else if equivT t1 tX1   
                then return b1
               else  throw $ ErrorLamParamUnexpected a xx b1 tX1
                        
        -- Check the body of the abstraction under the extended environment.
        let (ctx1, pos1) = pushType b1' ctx
        (x2', t2, e2, c2, ctx2)
         <- tableCheckExp table table ctx1 x2 (Check tX2)

        -- Cut closure terms due to locally bound value vars.
        -- This also lowers deBruijn indices in un-cut closure terms.
        let c2_cut      = Set.fromList
                        $ mapMaybe (cutTaggedClosureX b1)
                        $ Set.toList c2

        -- Build the resulting function type.
        --   The way the effect and closure term is captured depends on
        --   the configuration flags.
        (tResult, cResult)
         <- makeFunctionType config a (XLam a b1' x2) t1 t2 e2 c2_cut

        -- Cut the bound type and elems under it from the context.
        let Just ctx'   = popToPos pos1 ctx2
        
        returnX a
                (\z -> XLam z b1' x2')
                tResult 
                (Sum.empty kEffect)
                cResult
                ctx'

 | otherwise
 = checkSub table a ctx (XLam a b1 x2) tXX


-- | Construct a function type with the given effect and closure.
--   The way the effect and closure is handled depends on the Config.
makeFunctionType 
        :: Ord n
        => Config n              -- ^ Type checker config.
        -> a                     -- ^ Annotation for error messages.
        -> Exp a n               -- ^ Expression for error messages.
        -> Type n                -- ^ Parameter type of the function.
        -> Type n                -- ^ Result type of the function.
        -> TypeSum n             -- ^ Effect sum.
        -> Set (TaggedClosure n) -- ^ Closure terms.
        -> CheckM a n (Type n, Set (TaggedClosure n))

makeFunctionType config a xx t1 t2 e2 c2
 = do   -- Trim the closure before we annotate the returned function
        -- type with it. This should always succeed because trimClosure
        -- only returns Nothing if the closure is miskinded, and we've
        -- already already checked that.
        let c2_captured
                -- If we're not tracking closure information then just drop it 
                -- on the floor.
                | not  $ configTrackedClosures config   = tBot kClosure
                | otherwise
                = let Just c = trimClosure $ closureOfTaggedSet c2 in c

        let e2_captured
                -- If we're not tracking effect information then just drop it 
                -- on the floor.
                | not  $ configTrackedEffects config    = tBot kEffect
                | otherwise                             = TSum e2


        -- If the function type for the current fragment supports
        -- latent effects and closures then just use that.
        if      (  configFunctionalEffects  config
                && configFunctionalClosures config)
         then   return  ( tFunEC t1 e2_captured c2_captured t2
                        , c2)

        -- If the function type for the current fragment does not support
        -- latent effects, then the body expression needs to be pure.
        else if (  e2_captured == tBot kEffect
                && c2_captured == tBot kClosure)
         then   return  ( tFun t1 t2
                        , Set.empty)

        -- We don't have a way of forming a function with an impure effect.
        else if (e2_captured /= tBot kEffect)
         then   throw $ ErrorLamNotPure  a xx UniverseData e2_captured

        -- We don't have a way of forming a function with an non-empty closure.
        else if (c2_captured /= tBot kClosure)
         then   throw $ ErrorLamNotEmpty a xx UniverseData c2_captured

        -- One of the above error reporting cases should have fired already.
        else    error $ "ddc-core.makeFunctionType: is broken."


-- AbsLamWitness --------------------------------------------------------------
checkAbsLamWitness !table !a !ctx !b1 !_k1 !x2 !_dXX
 = do   let config      = tableConfig table
        let kenv        = tableKindEnv table
        let t1          = typeOfBind b1

        -- Check the body of the abstraction.
        let (ctx1, pos1) = pushType b1 ctx
        (x2', t2, e2, c2, ctx2) 
                <- tableCheckExp table table ctx1 x2 Recon
        (_, k2) <- checkTypeM config kenv ctx2 t2

        -- The body of a witness abstraction must be pure.
        when (e2 /= Sum.empty kEffect)
         $ throw $ ErrorLamNotPure      a (XLam a b1 x2) UniverseWitness (TSum e2)

        -- The body of a witness abstraction must produce data.
        when (not $ isDataKind k2)
         $ throw $ ErrorLamBodyNotData  a (XLam a b1 x2) b1 t2 k2

        -- Cut the bound type and elems under it from the context.
        let Just ctx'   = popToPos pos1 ctx2

        returnX a
                (\z -> XLam z b1 x2')
                (tImpl t1 t2)
                (Sum.empty kEffect)
                c2
                ctx'

