{-# OPTIONS_HADDOCK hide #-}
module DDC.Core.Check.Judge.Type.LetPrivate
        (checkLetPrivate)
where
import DDC.Core.Check.Judge.Kind
import DDC.Core.Check.Judge.EqT
import DDC.Core.Check.Judge.Type.Base
import qualified DDC.Core.Env.EnvT      as EnvT
import qualified DDC.Type.Sum           as Sum
import qualified DDC.Type.Env           as Env
import qualified Data.Set               as Set
import Data.List                        as L


checkLetPrivate :: Checker a n

-- private --------------------------------------
checkLetPrivate !table !ctx mode demand
        xx@(XLet a (LPrivate bsRgn mtParent bsWit) x)

 = case takeSubstBoundsOfBinds bsRgn of
    []   -> tableCheckExp table table ctx Recon demand x

    us   -> do
        let config      = tableConfig table
        let depth       = length $ map isBAnon bsRgn

        ctrace  $ vcat
                [ text "*>  Let Private"
                , text "    mode             =" <+> ppr mode
                , text "    demand           =" <+> text (show demand)
                , text "    in region binds  =" <+> ppr bsRgn
                , text "    in parent bind   =" <+> text (show mtParent)
                , text "    in witness binds =" <+> ppr bsWit
                , empty ]

        -- Check the kinds of the region binders.
        -- These must already set to kind Region.
        (bsRgn', _, _)
         <- liftM unzip3
         $  mapM (\b -> checkBindM config ctx UniverseKind b Recon) bsRgn
        let ksRgn       = map typeOfBind bsRgn'

        -- The binders must have region kind.
        when (any (not . isRegionKind) ksRgn)
         $ throw $ ErrorPrivateNotRegion a xx bsRgn ksRgn

        -- We can't shadow region binders because we might have witnesses
        -- in the environment that conflict with the ones created here.
        let rebounds    = filter (flip memberKindBind ctx) bsRgn'
        when (not $ null rebounds)
         $ throw $ ErrorPrivateRebound a xx rebounds

        -- Check the witness binders.
        -- These must have full type annotations, as we don't infer
        -- the types of introduced witnesses.
        let (ctx', pos1) = markContext ctx
        let ctx1         = pushKinds [(b, RoleConcrete) | b <- bsRgn] ctx'
        let ctx2         = liftTypes depth ctx1
        (bsWit', _, _)
         <- liftM unzip3
         $  mapM (\b -> checkBindM config ctx2 UniverseSpec b Recon)
                 bsWit

        -- Check that the witnesses bound here are for the region,
        -- and they don't conflict with each other.
        checkWitnessBindsM config a ctx xx us bsWit'

        -- Check the body expression.
        --   We always want to do this in 'Synth' mode as the expected
        --   type uses the region names visible from outside, and will
        --   not mention local regions are introduced by the 'private'
        --   construct.
        let ctx3        = pushTypes bsWit' ctx2
        (xBody3, tBody3, effs3, ctx4)
          <- tableCheckExp table table ctx3 (Synth []) demand x

        -- The body type must have data kind.
        (tBody4, kBody4, ctx5)
          <- checkTypeM config ctx4 UniverseSpec tBody3
          $  case mode of
                Recon   -> Recon
                _       -> Check kData

        tBody5      <- applyContext ctx5 tBody4
        kBody5      <- applyContext ctx5 kBody4
        TSum effs5  <- applyContext ctx5 (TSum effs3)
        when (not $ isDataKind kBody5)
         $ throw $ ErrorMismatch a kBody5 kData xx

        -- The final body type.
        tBody_final
         <- case mtParent of
                -- If the bound region variables are children of some parent
                -- region then they are merged into the parent when the
                -- private/extend construct ends.
                Just tParent
                 -> do  return $ foldl  (\t b -> substituteTX b tParent t)
                                        tBody5 bsRgn

                -- If the bound region variables have no parent then they are
                -- deallocated when the private construct ends.
                -- The bound region variables cannot be free in the body type.
                _
                 -> do  let fvsT         = freeT Env.empty tBody5
                        when (any (flip Set.member fvsT) us)
                         $ throw $ ErrorPrivateEscape a xx bsRgn tBody5
                        return $ lowerT depth tBody5

        -- Check that the result matches any expected type.
        ctx6    <- case mode of
                    Check tExpected
                     -> do  makeEqT config ctx5 tExpected tBody_final
                             $  ErrorMismatch a tExpected tBody_final xx

                    _ -> return ctx5

        tBody_final' <- applyContext ctx6 tBody_final

        -- Delete effects on the bound region from the result.
        let delEff es u = Sum.delete (tRead  (TVar u))
                        $ Sum.delete (tWrite (TVar u))
                        $ Sum.delete (tAlloc (TVar u))
                        $ es

        -- The final effect type.
        effs_cut
         <- case mtParent of
                -- If the bound region variables are children of some parent
                -- region then the overall effect is to allocate into
                -- the parent.
                Just tParent
                  -> return $ (lowerT depth $ foldl delEff effs5 us)
                           `Sum.union` (Sum.singleton kEffect (tAlloc tParent))

                -- If the bound region variables have no parent then they
                -- are deallocated when the private construct ends and no
                -- effect on these regions is visible.
                _ -> return $ lowerT depth
                            $ foldl delEff effs5 us

        -- Cut stack back to the length we started with,
        --  remembering to lower to undo the lift we applied previously.
        let ctx_cut     = lowerTypes depth
                        $ popToPos pos1 ctx6

        returnX a
                (\z -> XLet z (LPrivate bsRgn mtParent bsWit) xBody3)
                tBody_final' effs_cut ctx_cut


checkLetPrivate _ _ _ _ _
        = error "ddc-core.checkLetPrivate: no match"


-------------------------------------------------------------------------------
-- | Check the set of witness bindings bound in a letregion for conflicts.
checkWitnessBindsM
        :: (Show n, Ord n)
        => Config n             -- ^ Type checker config.
        -> a                    -- ^ Annotation for error messages.
        -> Context n            -- ^ Context
        -> Exp a n              -- ^ The whole expression, for error messages.
        -> [Bound n]            -- ^ Region variables bound in the letregion.
        -> [Bind n]             -- ^ Other witness bindings in the same set.
        -> CheckM a n ()

checkWitnessBindsM !config !a !ctx !xx !uRegions !bsWit
 = mapM_ checkWitnessBindM  bsWit
 where
        -- Check if some type variable or constructor is already in the
        -- environment. NOTE: The constructor case is for region handles
        -- when using the Eval fragment.
        inEnv tt
         = case tt of
             TVar u'
                | EnvT.member u' (contextEnvT ctx) -> True
                | memberKind u' ctx                -> True

             TCon (TyConBound u' _)
                | EnvT.member u' (contextEnvT ctx) -> True
                | memberKind u' ctx                -> True
             _                                     -> False


        -- Check the argument of a witness type is for the region we're
        -- introducing here.
        checkWitnessArg bWit t2
         = case t2 of
            TVar u'
             |  all (/= u') uRegions
             -> throw $ ErrorPrivateWitnessOther a xx uRegions bWit
             | otherwise -> return ()

            TCon (TyConBound u' _)
             | all (/= u') uRegions
             -> throw $ ErrorPrivateWitnessOther a xx uRegions bWit
             | otherwise -> return ()

            -- The parser should ensure the right of a witness is a
            -- constructor or variable.
            _            -> throw $ ErrorPrivateWitnessInvalid a xx bWit

        -- Associate each witness binder with its type.
        btsWit  = [(typeOfBind b, b) | b <- bsWit]

        -- Check a single witness binder for conflicts with other witnesses.
        checkWitnessBindM bWit
         = case typeOfBind bWit of

            TApp (TCon (TyConWitness TwConConst))    t2
             | Just bConflict <- L.lookup (tMutable t2) btsWit
             -> throw $ ErrorPrivateWitnessConflict a xx bWit bConflict
             | otherwise    -> checkWitnessArg bWit t2

            TApp (TCon (TyConWitness TwConMutable))  t2
             | Just bConflict <- L.lookup (tConst t2)    btsWit
             -> throw $ ErrorPrivateWitnessConflict a xx bWit bConflict
             | otherwise    -> checkWitnessArg bWit t2

            (takeTyConApps -> Just (TyConWitness (TwConDistinct 2), [t1, t2]))
             | inEnv t1  -> checkWitnessArg bWit t2
             | inEnv t2  -> checkWitnessArg bWit t1
             | t1 /= t2  -> mapM_ (checkWitnessArg bWit) [t1, t2]
             | otherwise -> throw $ ErrorPrivateWitnessInvalid a xx bWit

            (takeTyConApps -> Just (TyConWitness (TwConDistinct _), ts))
             -> mapM_ (checkWitnessArg bWit) ts

            TApp (TCon (TyConSpec TcConRead))  t2
             | configEffectCapabilities config
             -> checkWitnessArg bWit t2

            TApp (TCon (TyConSpec TcConWrite)) t2
             | configEffectCapabilities config
             -> checkWitnessArg bWit t2

            TApp (TCon (TyConSpec TcConAlloc)) t2
             | configEffectCapabilities config
             -> checkWitnessArg bWit t2

            _ -> throw $ ErrorPrivateWitnessInvalid a xx bWit



