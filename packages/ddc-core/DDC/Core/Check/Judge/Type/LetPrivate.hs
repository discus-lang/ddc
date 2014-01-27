
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
-- TODO: when checking this we need to make sure to update the effect
--       from the context before checking for escaping regions.
--
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
        (bsRgn', _, _)  <- liftM unzip3
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
        (bsWit', _, _)   <- liftM unzip3
                         $  mapM (\b -> checkBindM config kenv ctx2 UniverseSpec b Recon) 
                                 bsWit
        
        -- Check that the witnesses bound here are for the region,
        -- and they don't conflict with each other.
        checkWitnessBindsM config a kenv ctx xx us bsWit'

        -- Check the body expression.
        let ctx3        = pushTypes bsWit' ctx2
        (xBody', tBody, effs, clo, ctx4)  
                        <- tableCheckExp table table ctx3 x mode

        -- The body type must have data kind.
        (tBody', kBody, ctx5)   
         <- checkTypeM config kenv ctx4 UniverseSpec tBody
         $  case mode of
                Recon   -> Recon
                _       -> Check kData

        let kBody'      = applyContext ctx5 kBody
        when (not $ isDataKind kBody')
         $ throw $ ErrorLetBodyNotData a xx tBody kBody'

        -- The final body type.
        tBody_final
         <- case mtParent of
                -- If the bound region variables are children of some parent
                -- region then they are merged into the parent when the 
                -- private/extend construct ends.
                Just tParent
                 -> do  return $ foldl  (\t b -> substituteTX b tParent t) 
                                        tBody' bsRgn

                -- If the bound region variables have no parent then they are 
                -- deallocated when the private construct ends.
                -- The bound region variables cannot be free in the body type.
                _
                 -> do  let fvsT         = freeT Env.empty tBody
                        when (any (flip Set.member fvsT) us)
                         $ throw $ ErrorLetRegionFree a xx bsRgn tBody'
                        return $ lowerT depth tBody

        -- Delete effects on the bound region from the result.
        let delEff es u = Sum.delete (tRead  (TVar u))
                        $ Sum.delete (tWrite (TVar u))
                        $ Sum.delete (tAlloc (TVar u))
                        $ es
        
        -- The final effect type.
        tEffs'      
         <- case mtParent of
                -- If the bound region variables are children of some parent
                -- region then the overall effect is to allocate into 
                -- the parent.
                Just tParent
                  -> return $ (lowerT depth $ foldl delEff effs us)
                           `Sum.union` (Sum.singleton kEffect (tAlloc tParent))

                -- If the bound region variables have no parent then they
                -- are deallocated when the private construct ends and no
                -- effect on these regions is visible.
                _ -> return $ lowerT depth 
                            $ foldl delEff effs us 

        -- Delete the bound region variable from the closure.
        -- Mask closure terms due to locally bound region vars.
        let cutClo c r  = mapMaybe (cutTaggedClosureT r) c
        let c2_cut      = Set.fromList 
                        $ foldl cutClo (Set.toList clo) bsRgn

        -- Cut stack back to the length we started with,
        --  remembering to lower to undo the lift we applied previously.
        let ctx_cut     = lowerTypes depth
                        $ popToPos pos1 ctx5

        returnX a
                (\z -> XLet z (LPrivate bsRgn mtParent bsWit) xBody')
                tBody_final tEffs' c2_cut
                ctx_cut


-- withregion -----------------------------------
checkLetPrivate !table !ctx 
        xx@(XLet a (LWithRegion u) x) mode
 = do   let config      = tableConfig table
        let kenv        = tableKindEnv table

        -- The handle must have region kind.
        -- We need to look in the KindEnv as well as the Context here, 
        --  because the KindEnv knows the types of primitive variables.
        (case listToMaybe  
                $ catMaybes [ Env.lookup u kenv
                            , liftM fst $ lookupKind u ctx] of
          Nothing -> throw $ ErrorUndefinedVar a u UniverseSpec

          Just k  |  not $ isRegionKind k
                  -> throw $ ErrorWithRegionNotRegion a xx u k

          _       -> return ())
        
        -- Check the body expression.
        (xBody', tBody, effs, clo, ctx') 
                        <- tableCheckExp table table ctx x mode

        -- The body type must have data kind.
        (tBody', kBody, _) 
         <- checkTypeM config kenv ctx UniverseSpec tBody
         $  case mode of
                Recon   -> Recon
                _       -> Check kData

        when (not $ isDataKind kBody)
         $ throw $ ErrorLetBodyNotData a xx tBody' kBody
        
        -- The bound region variable cannot be free in the body type.
        let tcs         = supportTyCon
                        $ support Env.empty Env.empty tBody'
        when (Set.member u tcs)
         $ throw $ ErrorWithRegionFree a xx u tBody'

        -- Delete effects on the bound region from the result.
        let tu          = TVar u
        let effs'       = Sum.delete (tRead  tu)
                        $ Sum.delete (tWrite tu)
                        $ Sum.delete (tAlloc tu)
                        $ effs
        
        -- Delete the bound region handle from the closure.
        let clo_masked  = Set.delete (GBoundRgnCon u) clo

        returnX a
                (\z -> XLet z (LWithRegion u) xBody')
                tBody' effs' clo_masked
                ctx'

checkLetPrivate _ _ _ _
        = error "ddc-core.checkLetPrivate: no match"        


-------------------------------------------------------------------------------
-- | Check the set of witness bindings bound in a letregion for conflicts.
--   TODO: squash duplicated sigs.
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

checkWitnessBindsM !config !a !kenv !ctx !xx !nRegions !bsWits
 = mapM_ (checkWitnessBindM config a kenv ctx xx nRegions bsWits) bsWits

checkWitnessBindM 
        :: (Show n, Ord n)
        => Config n             -- ^ Type checker config.
        -> a                    -- ^ Annotation for error messages.
        -> KindEnv n            -- ^ Kind environment.
        -> Context n
        -> Exp a n
        -> [Bound n]            -- ^ Region variables bound in the letregion.
        -> [Bind n]             -- ^ Other witness bindings in the same set.
        -> Bind  n              -- ^ The witness binding to check.
        -> CheckM a n ()

checkWitnessBindM !config !a !kenv !ctx !xx !uRegions !bsWit !bWit
 = let  btsWit  = [(typeOfBind b, b) | b <- bsWit]

        -- Check the argument of a witness type is for the region we're
        -- introducing here.
        checkWitnessArg t
         = case t of
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
            
        inEnv t
         = case t of
             TVar u'                
                | Env.member u' kenv    -> True
                | memberKind u' ctx     -> True
             

             TCon (TyConBound u' _) 
                | Env.member u' kenv    -> True
                | memberKind u' ctx     -> True
             _                          -> False 
       
   in  case typeOfBind bWit of
        TApp (TCon (TyConWitness TwConGlobal))  t2
         -> checkWitnessArg t2

        TApp (TCon (TyConWitness TwConConst))   t2
         | Just bConflict <- L.lookup (tMutable t2) btsWit
         -> throw $ ErrorLetRegionWitnessConflict a xx bWit bConflict
         | otherwise    -> checkWitnessArg t2

        TApp (TCon (TyConWitness TwConMutable)) t2
         | Just bConflict <- L.lookup (tConst t2)   btsWit
         -> throw $ ErrorLetRegionWitnessConflict a xx bWit bConflict
         | otherwise    -> checkWitnessArg t2

        TApp (TCon (TyConWitness TwConLazy))    t2
         | Just bConflict <- L.lookup (tManifest t2)  btsWit
         -> throw $ ErrorLetRegionWitnessConflict a xx bWit bConflict
         | otherwise    -> checkWitnessArg t2

        TApp (TCon (TyConWitness TwConManifest))  t2
         | Just bConflict <- L.lookup (tLazy t2)    btsWit
         -> throw $ ErrorLetRegionWitnessConflict a xx bWit bConflict
         | otherwise    -> checkWitnessArg t2
         
        (takeTyConApps -> Just (TyConWitness (TwConDistinct 2), [t1, t2]))
         | inEnv t1  -> checkWitnessArg t2
         | inEnv t2  -> checkWitnessArg t1
         | t1 /= t2  -> mapM_ checkWitnessArg [t1, t2]
         | otherwise -> throw $ ErrorLetRegionWitnessInvalid a xx bWit

        (takeTyConApps -> Just (TyConWitness (TwConDistinct _), ts))
          -> mapM_ checkWitnessArg ts

        TApp (TCon (TyConSpec TcConRead)) t2
         | configEffectCapabilities config
         -> checkWitnessArg t2

        TApp (TCon (TyConSpec TcConWrite)) t2
         | configEffectCapabilities config
         -> checkWitnessArg t2

        TApp (TCon (TyConSpec TcConAlloc)) t2
         | configEffectCapabilities config
         -> checkWitnessArg t2

        _ -> throw $ ErrorLetRegionWitnessInvalid a xx bWit

