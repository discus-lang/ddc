
module DDC.Core.Check.Judge.Type.Let
        (checkLet)
where
import DDC.Core.Check.Judge.Type.Base
import qualified DDC.Type.Sum   as Sum
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


-- others ---------------------------------------
checkLet _ _ _ _
        = error "ddc-core.checkLet: no match"        


-------------------------------------------------------------------------------
-- | Check some let bindings.
checkLetsM 
        :: (Show n, Pretty n, Ord n)
        => Bool                         -- ^ Use bidirectional inference.
        -> Exp a n                      -- ^ Expression for error messages.
        -> Table a n                    -- ^ Static configuration.
        -> Context n                    -- ^ Input context.
        -> Lets a n                     -- ^ Let-bindings to check.
        -> CheckM a n
                ( Lets (AnTEC a n) n    --   Let-bindings annotated with types.
                , [Bind n]              --   Binding occs of vars, with types.
                , TypeSum n             --   Effect of evaluating all the bindings.
                , Set (TaggedClosure n) --   Closure of all the bindings.
                , Context n)            --   Output context.

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
        
        -- The kind of the binding must be Data.
        (_, kBind', _) 
         <- checkTypeM config kenv ctx1 UniverseSpec tBind Recon

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
        -- Data, which we verify here.
        let tAnnot      = typeOfBind b
        (modeCheck, ctx1)
         <- if isBot tAnnot
             -- There is no annotation on the binder.
             then return (Synth, ctx0)
             
             -- Check the type annotation on the binder,
             -- expecting the kind to be Data.
             else do
                (tAnnot', _kAnnot, ctx1)
                 <- checkTypeM config kenv ctx0 UniverseSpec tAnnot (Check kData)
                return (Check tAnnot', ctx1)

        -- Check the expression in the right of the binding.
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
        checkNoDuplicateBindings a xx bs

        -- All right hand sides must be syntactic abstractions.
        checkSyntacticLambdas a xx xs

        -- Check the types on all the binders.
        (bs', ks, _)    <- liftM unzip3                                 -- TODO: use ctx
                        $  mapM (\b -> checkBindM config kenv ctx UniverseSpec b Recon) bs
                        
        -- Check all the binders have data kind.
        zipWithM_ (\b k
         -> when (not $ isDataKind k)
                $ throw $ ErrorLetBindingNotData a xx b k)
                bs' ks

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


-------------------------------------------------------------------------------
-- | Check that the given list of binds does not contain duplicates
--   that would conflict if we added them to the environment
--   at the same level. If so, then throw and error.
checkNoDuplicateBindings
        :: Eq n
        => a                    -- ^ Annotation for error messages.
        -> Exp a n              -- ^ Expression for error messages.
        -> [Bind n]             -- ^ List of bindings to check.
        -> CheckM a n ()

checkNoDuplicateBindings a xx bs
 = case duplicates $ filter isBName bs of
        []    -> return ()
        b : _ -> throw $ ErrorLetrecRebound a xx b


-- | Take elements of a list that have more than once occurrence.
duplicates :: Eq a => [a] -> [a]
duplicates []           = []
duplicates (x : xs)
        | L.elem x xs   = x : duplicates (filter (/= x) xs)
        | otherwise     = duplicates xs
        

-------------------------------------------------------------------------------
-- | Check that all the bindings in a recursive let are syntactic lambdas.
--   We don't support value recursion, so can only define recursive functions.
--   If one of the expression is not a lambda then throw an error.
checkSyntacticLambdas
        :: a                    -- ^ Annotation for error messages.
        -> Exp a n              -- ^ Expression for error message.
        -> [Exp a n]            -- ^ Expressions to check.
        -> CheckM a n ()

checkSyntacticLambdas a xx xs
 = forM_ xs $ \x 
        -> when (not $ (isXLam x || isXLAM x))
        $ throw $ ErrorLetrecBindingNotLambda a xx x

