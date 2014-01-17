
module DDC.Core.Check.Judge.Type.Let
        (checkLet)
where
import DDC.Core.Check.Judge.Type.Base
import qualified DDC.Type.Sum   as Sum
import qualified DDC.Type.Env   as Env
import qualified Data.Set       as Set
import Data.List                as L


checkLet :: Checker a n

-- let --------------------------------------------
checkLet !table !ctx xx@(XLet a lts xBody) mode
 | case lts of
        LLet{}  -> True
        LRec{}  -> True
        _       -> False

 = do   let config  = tableConfig table
        let kenv    = tableKindEnv table

        -- Check the bindings -------------------
        -- Decide whether to use bidirectional type inference when checking
        -- the types of the bindings.
        let useBidirChecking   
                = case mode of
                        Recon   -> False
                        Check{} -> True
                        Synth   -> True
        
        (lts', bs', effsBinds, closBinds, ctx1)
         <- checkLetsM useBidirChecking xx table ctx lts 

        
        -- Check the body -----------------------
        -- Check the body expression in a context
        -- extended with the types of the bindings.
        let (ctx1', pos1) = markContext ctx1
        let ctx2          = pushTypes bs' ctx1'

        (xBody', tBody, effsBody, closBody, ctx3)
         <- tableCheckExp table table ctx2 xBody mode

        -- The body must have data kind.
        (tBody', kBody, ctx4)      
         <- checkTypeM config kenv ctx3 UniverseSpec tBody
         $  case mode of
                Recon   -> Recon
                _       -> Check kData
        
        let kBody' = applyContext ctx4 kBody
        when (not $ isDataKind kBody')
         $ throw $ ErrorLetBodyNotData a xx tBody kBody'


        -- Build the result ---------------------
        -- Mask closure terms due to locally bound value vars.
        let clos_cut    = Set.fromList
                        $ mapMaybe (cutTaggedClosureXs bs')
                        $ Set.toList closBody

        -- The new effect and closure.
        let tResult     = applyContext ctx4 tBody'
        let effs'       = effsBinds `Sum.union` effsBody
        let clos'       = closBinds `Set.union` clos_cut

        -- Pop the elements due to the let-bindings from the context.
        let ctx_cut     = popToPos pos1 ctx4

        ctrace  $ vcat
                [ text "* Let"
                , indent 2 $ ppr xx
                , text "  tResult:  " <> ppr tResult
                , indent 2 $ ppr ctx3
                , indent 2 $ ppr ctx_cut ]

        returnX a (\z -> XLet z lts' xBody')
                tResult effs' clos' ctx_cut


-- private --------------------------------------
-- TODO: when checking this we need to make sure to update the effect
--       from the context before checking for escaping regions.
--
checkLet !table !ctx xx@(XLet a (LPrivate bsRgn mtParent bsWit) x) tXX
 = case takeSubstBoundsOfBinds bsRgn of
    []   -> tableCheckExp table table ctx x Recon
    us   -> do
        let config      = tableConfig table
        let kenv        = tableKindEnv table
        let depth       = length $ map isBAnon bsRgn

        -- Check the type on the region binders.
        (bsRgn', _, _)  <- liftM unzip3                                         -- TODO: ctx
                        $  mapM (\b -> checkBindM config kenv ctx UniverseKind b Recon) bsRgn
        let ksRgn       = map typeOfBind bsRgn'
        
        -- The binders must have region kind.
        when (any (not . isRegionKind) ksRgn) 
         $ throw $ ErrorLetRegionsNotRegion a xx bsRgn ksRgn

        -- We can't shadow region binders because we might have witnesses
        -- in the environment that conflict with the ones created here.
        let rebounds    = filter (flip memberKindBind ctx) bsRgn'
        when (not $ null rebounds)
         $ throw $ ErrorLetRegionsRebound a xx rebounds
        
        -- Check the witness types.
        let (ctx', pos1) = markContext ctx
        let ctx1         = pushKinds [(b, RoleConcrete) | b <- bsRgn] ctx'
        let ctx2         = liftTypes depth ctx1
        (bsWit', _, _)   <- liftM unzip3                                        -- TODO: ctx
                         $  mapM (\b -> checkBindM config kenv ctx2 UniverseSpec b Recon) bsWit
        
        -- Check that the witnesses bound here are for the region,
        -- and they don't conflict with each other.
        checkWitnessBindsM config a kenv ctx xx us bsWit'

        -- Check the body expression.
        let ctx3        = pushTypes bsWit' ctx2
        (xBody', tBody, effs, clo, ctx4)  
                        <- tableCheckExp table table ctx3 x tXX

        -- The body type must have data kind.
        (_, kBody, _)   <- checkTypeM config kenv ctx4 UniverseSpec tBody Recon        -- TODO: ctx
        when (not $ isDataKind kBody)
         $ throw $ ErrorLetBodyNotData a xx tBody kBody

        -- The final body type.
        tBody'
         <- case mtParent of
                -- If the bound region variables are children of some parent
                -- region then they are merged into the parent when the 
                -- private/extend construct ends.
                Just tParent
                 -> do  return $ foldl  (\t b -> substituteTX b tParent t) 
                                        tBody bsRgn

                -- If the bound region variables have no parent then they are 
                -- deallocated when the private construct ends.
                -- The bound region variables cannot be free in the body type.
                _
                 -> do  let fvsT         = freeT Env.empty tBody
                        when (any (flip Set.member fvsT) us)
                         $ throw $ ErrorLetRegionFree a xx bsRgn tBody
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
                        $ popToPos pos1 ctx4

        returnX a
                (\z -> XLet z (LPrivate bsRgn mtParent bsWit) xBody')
                tBody' tEffs' c2_cut ctx_cut


-- withregion -----------------------------------
checkLet !table !ctx xx@(XLet a (LWithRegion u) x) tXX
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
                        <- tableCheckExp table table ctx x tXX

        -- The body type must have data kind.
        (tBody', kBody, _) <- checkTypeM config kenv ctx UniverseSpec tBody Recon      -- TODO: ctx
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
                tBody
                effs'
                clo_masked
                ctx'
        
-- others ---------------------------------------
checkLet _ _ _ _
        = error "ddc-core.checkLet: no match"        


-------------------------------------------------------------------------------
-- | Check some let bindings.
checkLetsM 
        :: (Show n, Pretty n, Ord n)
        => Bool         -- ^ Allow synthesis when checking the types of bindings.
        -> Exp a n      -- ^ Enclosing expression, for error messages.
        -> Table a n    -- ^ Static config.
        -> Context n    -- ^ Input context.
        -> Lets a n
        -> CheckM a n
                ( Lets (AnTEC a n) n
                , [Bind n]
                , TypeSum n
                , Set (TaggedClosure n)
                , Context n)

checkLetsM !bidir xx !table !ctx0 (LLet b xBind)
 
 -- Reconstruct the type of a non-recursive let-binding.
 | False  <- bidir
 = do   
        let config      = tableConfig table
        let kenv        = tableKindEnv table
        let a           = annotOfExp xx

        -- Reconstruct the type of the binding.
        (xBind', tBind, effsBind, closBind, ctx1) 
         <- tableCheckExp table table ctx0 xBind Recon
        
        -- The reconstructed kind of the binding must be Data.
        (_, kBind', _) <- checkTypeM config kenv ctx1 UniverseSpec tBind Recon
        when (not $ isDataKind kBind')
         $ throw $ ErrorLetBindingNotData a xx b kBind'
        
        -- If there is a type annotation on the binding then this
        -- must match the reconstructed type.
        when (not $ isBot (typeOfBind b))
         $ if equivT (typeOfBind b) tBind
                then return ()
                else (throw $ ErrorLetMismatch a xx b tBind)          
        
        -- Update the annotation on the binder with the actual type of
        -- the binding.
        let b'  = replaceTypeOfBind tBind b

        return  ( LLet b' xBind'
                , [b']
                , effsBind, closBind
                , ctx1)

 -- Synthesise the type of a non-recursive let-binding,
 -- using any annotation on the binder as the expected type.
 | True   <- bidir
 = do   
        let config      = tableConfig table
        let kenv        = tableKindEnv table
        
        -- If the binder has a type annotation then we use that as the expected
        -- type when checking the binding. Any annotation must also have kind
        -- Data, which we check for here.
        let tAnnot      = typeOfBind b
        (modeCheck, ctx1)
         <- if isBot tAnnot
             -- There is no annotation on the binder.
             then return (Synth, ctx0)
             
             -- We have an annotation on the binder, so check it's kind.
             else do
                (tAnnot', _kAnnot, ctx1)
                 <- checkTypeM config kenv ctx0 UniverseSpec tAnnot (Check kData)
                return (Check tAnnot', ctx1)

        -- Check the binding itself.
        (xBind', tBind, effsBind, closBind, ctx2)
         <- tableCheckExp table table ctx1 xBind modeCheck

        -- Update the annotation on the binder with the actual type of
        -- the binding.
        let b'  = replaceTypeOfBind tBind b

        return  ( LLet b' xBind'
                , [b']
                , effsBind, closBind
                , ctx2)


-- letrec ---------------------------------------
checkLetsM !bidir !xx !table !ctx (LRec bxs)
 = do   let config      = tableConfig table
        let kenv        = tableKindEnv table
        let (bs, xs)    = unzip bxs
        let a           = annotOfExp xx

        -- Named binders cannot be multiply defined.
        (case duplicates $ filter isBName bs of
          []    -> return ()
          b : _ -> throw $ ErrorLetrecRebound a xx b)

        -- Check the types on all the binders.
        (bs', ks, _)    <- liftM unzip3                                 -- TODO: use ctx
                        $  mapM (\b -> checkBindM config kenv ctx UniverseSpec b Recon) bs
                        
        -- Check all the binders have data kind.
        zipWithM_ (\b k
         -> when (not $ isDataKind k)
                $ throw $ ErrorLetBindingNotData a xx b k)
                bs' ks

        -- All right hand sides must be syntactic abstractions.
        forM_ xs $ \x 
         -> when (not $ (isXLam x || isXLAM x))
                $ throw $ ErrorLetrecBindingNotLambda a xx x

        -- All variables are in scope in all right hand sides.
        let (ctx', pos1) = markContext ctx
        let ctx1         = pushTypes bs' ctx'

        -- Check the right hand sides.
        --   The context will not contain any more variable bindings,
        --   but it may contain more solved existentials.
        (results, ctx2) <- checkRecBinds bidir xx table ctx1 (zip bs xs)
        let (_, xsRight', tsRight, _effssBinds, clossBinds)
                        = unzip5 results

        -- Check annots on binders against inferred types of the bindings.
        zipWithM_ (\b t
                -> if not $ equivT (typeOfBind b) t
                        then throw $ ErrorLetMismatch a xx b t
                        else return ())
                bs tsRight

        -- Cut closure terms due to locally bound value vars.
        let clos_cut 
                = Set.fromList
                $ mapMaybe (cutTaggedClosureXs bs)
                $ Set.toList 
                $ Set.unions clossBinds

        -- Pop types of the bindings from the stack.
        let ctx_cut = popToPos pos1 ctx2

        return  ( LRec (zip bs' xsRight')
                , zipWith replaceTypeOfBind tsRight bs'
                , Sum.empty kEffect
                , clos_cut
                , ctx_cut)

checkLetsM _synthOK _xx _config _ctx _lts
        = error "checkLetsM: case should have been handled in checkExpM"


-------------------------------------------------------------------------------
-- | Check some recursive bindings.
--   Doing this won't push any more bindings onto the context,
--   though it may solve some existentials in it.
checkRecBinds 
        :: (Pretty n, Show n, Ord n)
        => Bool                         -- ^ Allow bidirectional checking.
        -> Exp a n                      -- ^ Expression for error messages.
        -> Table a n
        -> Context n                    -- ^ Original context.
        -> [(Bind n, Exp a n)]          -- ^ Bindings and exps for rec bindings.
        -> CheckM a n 
                ( [ ( Bind n                   -- Result bindiner.
                    , Exp (AnTEC a n) n        -- Result expression.
                    , Type n                   -- Result type.
                    , TypeSum n                -- Result effect.
                    , Set (TaggedClosure n))]  -- Result closure.
                , Context n)

checkRecBinds bidir _xx table ctx0 bxs0
 = go bxs0 ctx0
 where  go [] ctx       
         =      return ([], ctx)
        
        go ((b, x) : bxs) ctx
         = do   let mode  = checkModeFromBind bidir b

                (x', t, effs, clos, ctx') 
                 <- tableCheckExp table table ctx x mode

                (moar, ctx'') <- go bxs ctx'

                return $ ((b, x', t, effs, clos) : moar, ctx'')


-- | Based on the annotation of a let-binding,
--   decide how to type check the right of the binding.
checkModeFromBind 
        :: Bool         -- ^ Allow bi-directional type checking.
        -> Bind n       -- ^ Binder of the let-binding.
        -> Mode n

checkModeFromBind bidir b 
        -- If we're not doing bidirectional type inference then just
        -- reconstruct the type.
        | not bidir     
        = Recon

        -- With bidirectional type inferece, if we have an annotation
        -- for the binder then use that as the expected type.
        | tBind <- typeOfBind b
        , not $ isBot tBind
        = Check tBind

        -- With bidirectional type inference, if there is no annotation
        -- then we synthesise the type of the binding.
        | otherwise
        = Synth


-- | Take elements of a list that have more than once occurrence.
duplicates :: Eq a => [a] -> [a]
duplicates []           = []
duplicates (x : xs)
        | L.elem x xs   = x : duplicates (filter (/= x) xs)
        | otherwise     = duplicates xs
        

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

