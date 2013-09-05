
module DDC.Core.Check.Exp.Abs
        (checkAbs)
where
import DDC.Core.Check.Exp.Base
import qualified DDC.Type.Sum   as Sum
import qualified DDC.Type.Env   as Env
import qualified Data.Set       as Set


-- Dispatch -------------------------------------------------------------------
checkAbs :: Checker a n
checkAbs !table !kenv !tenv (XLAM a b1 x2) _
 = do   checkAbsLAM table kenv tenv a b1 x2
         
checkAbs !table !kenv !tenv xx@(XLam a b1 x2) dXX
 = do   let config      = tableConfig table

        -- The rule we use depends on the kind of the binder.
        (b1', k1)       <- checkBindM config kenv b1

        case universeFromType2 k1 of
         Just UniverseData
           -> checkAbsLamData    table kenv tenv a b1' k1 x2 dXX
         Just UniverseWitness
           -> checkAbsLamWitness table kenv tenv a b1' k1 x2 dXX
         _ -> throw $ ErrorLamBindBadKind a xx (typeOfBind b1') k1

checkAbs _ _ _ _ _
        = error "ddc-core.checkAbs: no match."


-- AbsLAM ---------------------------------------------------------------------
checkAbsLAM !table !kenv !tenv a b1 x2
 = do   let xx          = XLAM a b1 x2
        let config      = tableConfig table
        
        -- Check the type of the binder.
        (b1', _)        <- checkBindM config kenv b1

        -- The bound variable cannot shadow others in the environment.
        when (Env.memberBind b1' kenv)
         $ throw $ ErrorLamShadow a xx b1
        
        -- Check the body of the abstraction.
        let kenv'       = Env.extend b1' kenv
        let tenv'       = Env.lift   1  tenv
        (x2', t2, e2, c2) 
                <- tableCheckExp table table  kenv' tenv' x2 Synth
        (_, k2) <- checkTypeM config kenv' t2

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

        returnX a
                (\z -> XLAM z b1' x2')
                (TForall b1' t2)
                (Sum.empty kEffect)
                c2_cut


-- AbsLamData -----------------------------------------------------------------
checkAbsLamData !table !kenv !tenv !a !b1 !_k1 !x2 !dXX 
 = do   let config      = tableConfig table
        let t1          = typeOfBind b1

        -- If we have an expected type for the abstraction then split off
        -- the expected type of the body.
        let dX2 = case dXX of
                        Synth           -> Synth
                        Check tXX
                         | Just (_, tX2) <- takeTFun tXX -> Check tX2
                         | otherwise    -> Synth        

        -- Check the body of the abstraction.
        let tenv'       = Env.extend b1 tenv
        (x2', t2, e2, c2) 
         <- tableCheckExp table table kenv tenv' x2 dX2

        -- The body of a data abstraction must produce data.
        (_, k2) <- checkTypeM config kenv t2
        when (not $ isDataKind k2)
         $ throw $ ErrorLamBodyNotData a (XLam a b1 x2) b1 t2 k2 

        -- Cut closure terms due to locally bound value vars.
        -- This also lowers deBruijn indices in un-cut closure terms.
        let c2_cut      = Set.fromList
                        $ mapMaybe (cutTaggedClosureX b1)
                        $ Set.toList c2

        -- Build the resulting function type.
        --   The way the effect and closure term is captured depends on the config.
        (tResult, cResult)
         <- makeFunctionType config a (XLam a b1 x2) t1 t2 e2 c2_cut

        returnX a
                (\z -> XLam z b1 x2')
                tResult 
                (Sum.empty kEffect)
                cResult


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
checkAbsLamWitness !table !kenv !tenv !a !b1 !_k1 !x2 !_dXX
 = do   let config      = tableConfig table
        let t1          = typeOfBind b1

        -- Check the body of the abstraction.
        let tenv'       = Env.extend b1 tenv
        (x2', t2, e2, c2) 
                <- tableCheckExp table table kenv tenv' x2 Synth
        (_, k2) <- checkTypeM config kenv t2

        -- The body of a witness abstraction must be pure.
        when (e2 /= Sum.empty kEffect)
         $ throw $ ErrorLamNotPure      a (XLam a b1 x2) UniverseWitness (TSum e2)

        -- The body of a witness abstraction must produce data.
        when (not $ isDataKind k2)
         $ throw $ ErrorLamBodyNotData  a (XLam a b1 x2) b1 t2 k2

        returnX a
                (\z -> XLam z b1 x2')
                (tImpl t1 t2)
                (Sum.empty kEffect)
                c2

