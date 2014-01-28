
module DDC.Core.Check.Judge.Type.LetPrivate
        (checkLetPrivate)
where
import DDC.Core.Check.Judge.Type.Base
import qualified DDC.Type.Sum   as Sum
import qualified DDC.Type.Env   as Env
import qualified Data.Set       as Set
import Data.List                as L


checkLetPrivate :: Checker a n

-- private --------------------------------------
checkLetPrivate !table !ctx 
        xx@(XLet a (LPrivate bsRgn mtParent bsWit) x) mode
 = case takeSubstBoundsOfBinds bsRgn of
    []   -> tableCheckExp table table ctx x Recon
    us   -> do
        let config      = tableConfig table
        let kenv        = tableKindEnv table
        let depth       = length $ map isBAnon bsRgn

        -- Check the kinds of the region binders.
        -- These must already set to kind Region.
        (bsRgn', _, _)  
         <- liftM unzip3
         $  mapM (\b -> checkBindM config kenv ctx UniverseKind b Recon)
                 bsRgn
        let ksRgn       = map typeOfBind bsRgn'
        
        -- The binders must have region kind.
        when (any (not . isRegionKind) ksRgn) 
         $ throw $ ErrorLetRegionsNotRegion a xx bsRgn ksRgn

        -- We can't shadow region binders because we might have witnesses
        -- in the environment that conflict with the ones created here.
        let rebounds    = filter (flip memberKindBind ctx) bsRgn'
        when (not $ null rebounds)
         $ throw $ ErrorLetRegionsRebound a xx rebounds
        
        -- Check the witness binders.
        -- These must have full type annotations, as we don't infer
        -- the types of introduced witnesses.
        let (ctx', pos1) = markContext ctx
        let ctx1         = pushKinds [(b, RoleConcrete) | b <- bsRgn] ctx'
        let ctx2         = liftTypes depth ctx1
        (bsWit', _, _)   
         <- liftM unzip3
         $  mapM (\b -> checkBindM config kenv ctx2 UniverseSpec b Recon) 
                 bsWit
        
        -- Check that the witnesses bound here are for the region,
        -- and they don't conflict with each other.
        checkWitnessBindsM config a kenv ctx xx us bsWit'

        -- Check the body expression.
        let ctx3        = pushTypes bsWit' ctx2
        (xBody3, tBody3, effs3, clo, ctx4)  
          <- tableCheckExp table table ctx3 x mode

        -- The body type must have data kind.
        (tBody4, kBody4, ctx5)   
         <- checkTypeM config kenv ctx4 UniverseSpec tBody3
         $  case mode of
                Recon   -> Recon
                _       -> Check kData

        let tBody5      = applyContext ctx5 tBody4
        let kBody5      = applyContext ctx5 kBody4
        let TSum effs5  = applyContext ctx5 (TSum effs3)
        when (not $ isDataKind kBody5)
         $ throw $ ErrorLetBodyNotData a xx tBody5 kBody5

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
                         $ throw $ ErrorLetRegionFree a xx bsRgn tBody5
                        return $ lowerT depth tBody5

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

        -- Delete the bound region variable from the closure.
        -- Mask closure terms due to locally bound region vars.
        let cutClo c r  = mapMaybe (cutTaggedClosureT r) c
        let clos_cut    = Set.fromList 
                        $ foldl cutClo (Set.toList clo) bsRgn

        -- Cut stack back to the length we started with,
        --  remembering to lower to undo the lift we applied previously.
        let ctx_cut     = lowerTypes depth
                        $ popToPos pos1 ctx5

        returnX a
                (\z -> XLet z (LPrivate bsRgn mtParent bsWit) xBody3)
                tBody_final effs_cut clos_cut
                ctx_cut


-- withregion -----------------------------------
checkLetPrivate !table !ctx0
        xx@(XLet a (LWithRegion u) x) mode
 = do   let config      = tableConfig table
        let kenv        = tableKindEnv table

        -- The handle must have region kind.
        -- We need to look in the KindEnv as well as the Context here, 
        --  because the KindEnv knows the types of primitive variables.
        (case listToMaybe  
                $ catMaybes [ Env.lookup u kenv
                            , liftM fst $ lookupKind u ctx0] of
          Nothing -> throw $ ErrorUndefinedVar a u UniverseSpec

          Just k  |  not $ isRegionKind k
                  -> throw $ ErrorWithRegionNotRegion a xx u k

          _       -> return ())
        
        -- Check the body expression.
        (xBody0, tBody0, effs0, clo, ctx1) 
         <- tableCheckExp table table ctx0 x mode

        -- The body type must have data kind.
        (tBody1, kBody1, ctx2) 
         <- checkTypeM config kenv ctx1 UniverseSpec tBody0
         $  case mode of
                Recon   -> Recon
                _       -> Check kData

        let tBody2      = applyContext ctx2 tBody1
        let kBody2      = applyContext ctx2 kBody1
        let TSum effs2  = applyContext ctx2 (TSum effs0)
        
        when (not $ isDataKind kBody2)
         $ throw $ ErrorLetBodyNotData a xx tBody2 kBody2
        
        -- The bound region variable cannot be free in the body type.
        let tcs         = supportTyCon
                        $ support Env.empty Env.empty tBody2
        
        when (Set.member u tcs)
         $ throw $ ErrorWithRegionFree a xx u tBody2

        -- Delete effects on the bound region from the result.
        let tu          = TVar u
        let effs_cut    = Sum.delete (tRead  tu)
                        $ Sum.delete (tWrite tu)
                        $ Sum.delete (tAlloc tu)
                        $ effs2
        
        -- Delete the bound region handle from the closure.
        let clos_cut    = Set.delete (GBoundRgnCon u) clo

        returnX a
                (\z -> XLet z (LWithRegion u) xBody0)
                tBody2 effs_cut clos_cut
                ctx2

checkLetPrivate _ _ _ _
        = error "ddc-core.checkLetPrivate: no match"        


-------------------------------------------------------------------------------
-- | Check the set of witness bindings bound in a letregion for conflicts.
checkWitnessBindsM 
        :: (Show n, Ord n) 
        => Config n             -- ^ Type checker config.
        -> a                    -- ^ Annotation for error messages.
        -> KindEnv n            -- ^ Kind Environment.
        -> Context n            -- ^ Context
        -> Exp a n              -- ^ The whole expression, for error messages.
        -> [Bound n]            -- ^ Region variables bound in the letregion.
        -> [Bind n]             -- ^ Other witness bindings in the same set.
        -> CheckM a n ()

checkWitnessBindsM !config !a !kenv !ctx !xx !uRegions !bsWit
 = mapM_ checkWitnessBindM  bsWit
 where
        -- Check if some type variable or constructor is already in the
        -- environment. NOTE: The constructor case is for region handles
        -- when using the Eval fragment.
        inEnv tt
         = case tt of
             TVar u'                
                | Env.member u' kenv    -> True
                | memberKind u' ctx     -> True
             
             TCon (TyConBound u' _) 
                | Env.member u' kenv    -> True
                | memberKind u' ctx     -> True
             _                          -> False 


        -- Check the argument of a witness type is for the region we're
        -- introducing here.
        checkWitnessArg bWit t2
         = case t2 of
            TVar u'
             |  all (/= u') uRegions 
                         -> throw $ ErrorLetRegionsWitnessOther a xx uRegions bWit
             | otherwise -> return ()

            TCon (TyConBound u' _)
             | all (/= u') uRegions 
                         -> throw $ ErrorLetRegionsWitnessOther a xx uRegions bWit
             | otherwise -> return ()
            
            -- The parser should ensure the right of a witness is a 
            -- constructor or variable.
            _            -> throw $ ErrorLetRegionWitnessInvalid a xx bWit
    
        -- Associate each witness binder with its type.
        btsWit  = [(typeOfBind b, b) | b <- bsWit]
  
        -- Check a single witness binder for conflicts with other witnesses.7
        checkWitnessBindM bWit
         = case typeOfBind bWit of
            TApp (TCon (TyConWitness TwConGlobal))   t2
             -> checkWitnessArg bWit t2

            TApp (TCon (TyConWitness TwConConst))    t2
             | Just bConflict <- L.lookup (tMutable t2) btsWit
             -> throw $ ErrorLetRegionWitnessConflict a xx bWit bConflict
             | otherwise    -> checkWitnessArg bWit t2

            TApp (TCon (TyConWitness TwConMutable))  t2
             | Just bConflict <- L.lookup (tConst t2)    btsWit
             -> throw $ ErrorLetRegionWitnessConflict a xx bWit bConflict
             | otherwise    -> checkWitnessArg bWit t2

            TApp (TCon (TyConWitness TwConLazy))     t2
             | Just bConflict <- L.lookup (tManifest t2) btsWit
             -> throw $ ErrorLetRegionWitnessConflict a xx bWit bConflict
             | otherwise    -> checkWitnessArg bWit t2

            TApp (TCon (TyConWitness TwConManifest)) t2
             | Just bConflict <- L.lookup (tLazy t2)     btsWit
             -> throw $ ErrorLetRegionWitnessConflict a xx bWit bConflict
             | otherwise    -> checkWitnessArg bWit t2
         
            (takeTyConApps -> Just (TyConWitness (TwConDistinct 2), [t1, t2]))
             | inEnv t1  -> checkWitnessArg bWit t2
             | inEnv t2  -> checkWitnessArg bWit t1
             | t1 /= t2  -> mapM_ (checkWitnessArg bWit) [t1, t2]
             | otherwise -> throw $ ErrorLetRegionWitnessInvalid a xx bWit

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

            _ -> throw $ ErrorLetRegionWitnessInvalid a xx bWit

