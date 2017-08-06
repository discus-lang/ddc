{-# OPTIONS_HADDOCK hide #-}
module DDC.Core.Check.Judge.Type.Let
        (checkLet)
where
import DDC.Core.Check.Judge.Type.Base
import qualified DDC.Type.Sum   as Sum
import Data.List                as L


---------------------------------------------------------------------------------------------------
checkLet :: Checker a n

-- let --------------------------------------------
checkLet !table !ctx0 mode demand xx@(XLet a lts xBody)
 | case lts of
        LLet{}  -> True
        LRec{}  -> True
        _       -> False

 = do   ctrace  $ vcat
                [ text "*>  Let"
                , text "    mode   =" <+> ppr mode
                , text "    demand =" <+> (text $ show demand)
                , empty]

        let config  = tableConfig table

        -- Check the bindings -------------------
        -- Decide whether to use bidirectional type inference when checking
        -- the types of the bindings.
        let useBidirChecking
                = case mode of
                        Recon   -> False
                        Check{} -> True
                        Synth{} -> True

        (lts', _bs', effsBinds, pos1, ctx1)
         <- checkLetsM useBidirChecking xx table ctx0 demand lts


        -- Check the body -----------------------
        -- -- Check the body expression in a context
        -- -- extended with the types of the bindings.
        ctrace  $ vcat
                [ text "*.  Let Body " <> ppr mode
                , text "    demand = " <> (text $ show demand)
                , empty]

        (xBody', tBody, effsBody, ctx2)
         <- tableCheckExp table table ctx1 mode demand xBody

        -- The body must have data kind.
        (tBodyChecked, kBody, ctx3)
         <- checkTypeM config ctx2 UniverseSpec tBody
         $  case mode of
                Recon   -> Recon
                _       -> Check kData

        kBody' <- applyContext ctx3 kBody
        when (not $ isDataKind kBody')
         $ throw $ ErrorMismatch a kBody' kData xx

        tBody' <- applyContext ctx3 tBodyChecked

        -- Run the body if needed ---------------
        (xBodyRun, tBodyRun, eBodyRun)
         <- case mode of
                Synth{} -> runForDemand (tableConfig table) a demand
                                xBody' tBody' (TSum effsBody)

                _       -> return (xBody', tBody', TSum effsBody)


        -- Build the result ---------------------
        -- The new effect and closure.
        let eResult     = tSum kEffect [TSum effsBinds, eBodyRun]

        -- Pop the elements due to the let-bindings from the context.
        let ctx_cut     = popToPos pos1 ctx3

        ctrace  $ vcat
                [ text "*<  Let " <> ppr mode
                , text "    demand = " <> (text $ show demand)
                , text "    -- EXP IN  ----"
                , indent 4 $ ppr xx
                , text "    -- EXP OUT ----"
                , indent 4 $ ppr (XLet (AnTEC tBodyRun (tBot kEffect) (tBot kClosure) a)
                                        lts' xBodyRun)
                , text "    --"
                , text "    tBodyRun:  " <> ppr tBodyRun
                , indent 4 $ ppr ctx3
                , indent 4 $ ppr ctx_cut
                , empty ]

        returnX a
                (\z -> XLet z lts' xBodyRun)
                tBodyRun
                (Sum.fromList kEffect [eResult])
                ctx_cut


-- others ---------------------------------------
-- The dispatcher should only call checkLet with a XLet AST node.
checkLet _ _ _ _ _
        = error "ddc-core.checkLet: no match"


---------------------------------------------------------------------------------------------------
-- | Check some let bindings,
--   and push their binders onto the context.
checkLetsM
        :: (Show a, Show n, Pretty n, Ord n)
        => Bool                         -- ^ Use bidirectional inference.
        -> Exp a n                      -- ^ Expression for error messages.
        -> Table a n                    -- ^ Static configuration.
        -> Context n                    -- ^ Input context.
        -> Demand                       -- ^ Demand placed on the bindings.
        -> Lets a n                     -- ^ Let-bindings to check.
        -> CheckM a n
                ( Lets (AnTEC a n) n    --   Let-bindings annotated with types.
                , [Bind n]              --   Binding occs of vars, with types.
                , TypeSum n             --   Effect of evaluating all the bindings.
                , Pos                   --   Context position with bindings pushed.
                , Context n)            --   Output context.

checkLetsM !bidir xx !table !ctx0 !demand (LLet b xBind)

 -- Reconstruct the type of a non-recursive let-binding.
 | False  <- bidir
 = do
        let config      = tableConfig table
        let a           = annotOfExp xx

        -- Reconstruct the type of the binding.
        (xBind', tBind, effsBind, ctx1)
         <- tableCheckExp table table ctx0 Recon demand xBind

        -- The kind of the binding must be Data.
        (_, kBind', _)
         <- checkTypeM config ctx1 UniverseSpec tBind Recon

        when (not $ isDataKind kBind')
         $ throw $ ErrorMismatch a kBind' kData xx

        -- If there is a type annotation on the binding then this
        -- must match the reconstructed type.
        when (not $ isBot (typeOfBind b))
         $ if equivT (contextEnvT ctx0) (typeOfBind b) tBind
                then return ()
                else (throw $ ErrorMismatch a (typeOfBind b) tBind xx)

        -- Update the annotation on the binder with the actual type of
        -- the binding.
        let b'  = replaceTypeOfBind tBind b

        -- Push the binder on the context.
        let (ctx2, pos1) =  markContext ctx1
        let ctx3         =  pushType b' ctx2

        return  ( LLet b' xBind'
                , [b']
                , effsBind
                , pos1, ctx3)

 -- Synthesise the type of a non-recursive let-binding,
 -- using any annotation on the binder as the expected type.
 | True   <- bidir
 = do
        let config      = tableConfig table
        let a           = annotOfExp xx

        -- If the binder has a type annotation then we use that as the expected
        -- type when checking the binding. Any annotation must also have kind
        -- Data, which we verify here.
        let tAnnot      = typeOfBind b
        (mode, ctx1)
         <- if isBot tAnnot
             -- There is no annotation on the binder.
             then return (Synth [], ctx0)

             -- Check the type annotation on the binder,
             -- expecting the kind to be Data.
             else do
                (tAnnot', _kAnnot, ctx1)
                 <- checkTypeM config ctx0 UniverseSpec tAnnot (Check kData)
                return (Check tAnnot', ctx1)

        ctrace  $ vcat
                [ text "*>  Let Bind"  <+> ppr mode
                , text "    demand = " <> (text $ show demand)
                , text "    bind   = " <> (ppr b)
                , empty ]

        -- Check the expression in the right of the binding.
        (xBind_raw, tBind_raw, effs_raw, ctx2)
         <- tableCheckExp table table ctx1 mode demand xBind

        tBind_ctx <- applyContext ctx2 tBind_raw

        -- Handle ImplictRunBindings
        --   If the right of the binding is a suspended expression, but there is
        --   no binder then the expression is probably being evaluated for its
        --   effect only. If ImplicitRunBindings is enabled then we automatically
        --   run the suspension to release its effect.
        let (xBind_run, tBind_run, effs_run)
                | configImplicitRun $ tableConfig table
                , not $ isXCastBox xBind_raw
                , not $ isXCastRun xBind_raw
                , Just  (effs_susp, tBind_susp) <- takeTSusp tBind_ctx
                = let
                        -- Effect of overall expression is effect of computing
                        -- the suspension plus the effect we get by running
                        -- that suspension.
                        effs_result = Sum.insert effs_susp effs_raw

                        -- Annotation for the resulting cast expression.
                        a'          = AnTEC tBind_susp (TSum $ effs_result)
                                            (tBot kClosure) a
                  in    ( XCast a' CastRun xBind_raw
                        , tBind_susp
                        , effs_result)

                | otherwise
                = (xBind_raw, tBind_raw, effs_raw)


        -- Update the annotation on the binder with the actual type of
        -- the binding.
        let b'           = replaceTypeOfBind tBind_run b

        -- Push the binder on the context.
        let (ctx3, pos1) =  markContext ctx2
        let ctx4         =  pushType b' ctx3

        return  ( LLet b' xBind_run
                , [b']
                , effs_run
                , pos1, ctx4)


-- letrec ---------------------------------------
checkLetsM !bidir !xx !table !ctx0 !demand (LRec bxs)
 = do   let (bs, xs)    = unzip bxs
        let a           = annotOfExp xx

        ctrace  $ vcat
                [ text "*>  Let Rec"
                , text "    demand = " <> (text $ show demand)
                , text "    binds  = " <> (ppr  $ map fst bxs)
                , empty ]

        -- Named binders cannot be multiply defined.
        checkNoDuplicateBindings a xx bs

        -- All right hand sides must be syntactic abstractions.
        checkSyntacticLambdas table a xx xs

        -- Check the type annotations on all the binders.
        (bs', ctx1)
         <- checkRecBinds table bidir a xx ctx0 bs

        -- All variables are in scope in all right hand sides.
        let (ctx2, pos1) =  markContext ctx1
        let ctx3         =  pushTypes bs' ctx2

        -- Check the right hand sides.
        (results, ctx4)
         <- checkRecBindExps table bidir a ctx3 demand (zip bs' xs)

        let (bs'', xsRight')
                = unzip results

        return  ( LRec (zip bs'' xsRight')
                , bs''
                , Sum.empty kEffect
                , pos1, ctx4)


-- others ---------------------------------------
-- The dispatcher should only call checkLet with LLet and LRec AST nodes,
-- so we should not see the others here.
checkLetsM _ _ _ _ _ _
        = error "ddc-core.checkLetsM: no match"


---------------------------------------------------------------------------------------------------
-- | Check the annotations on a group of recursive binders.
checkRecBinds
        :: (Pretty n, Show n, Ord n)
        => Table a n
        -> Bool                         -- ^ Use bidirectional checking.
        -> a                            -- ^ Annotation for error messages.
        -> Exp a n                      -- ^ Expression for error messages.
        -> Context n                    -- ^ Original context.
        -> [Bind n]                     -- ^ Input binding group.
        -> CheckM a n
                ( [Bind n]              --   Result binding group.
                ,  Context n)           --   Output context.

checkRecBinds table bidir a xx ctx0 bs0
 = go bs0 ctx0
 where
        go [] ctx
         = return ([], ctx)

        go (b : bs) ctx
         = do   (b', ctx')      <- checkRecBind b ctx
                (moar, ctx'')   <- go bs ctx'
                return (b' : moar, ctx'')

        config  = tableConfig  table

        checkRecBind b ctx
         = case bidir of
            False
             -> do
                -- In Recon mode, all recursive let-bindings must have full
                -- type annotations.
                when (isBot $ typeOfBind b)
                 $ throw $ ErrorLetrecMissingAnnot a b xx

                -- Check the type on the binder.
                (b', k, ctx')
                 <- checkBindM config ctx UniverseSpec b Recon

                -- The type on the binder must have kind Data.
                when (not $ isDataKind k)
                 $ throw $ ErrorMismatch a k kData xx

                return (b', ctx')

            True
             -- Recursive let-binding is missing a type annotation,
             -- so make a new existential.
             | isBot (typeOfBind b)
             -> do i        <- newExists kData
                   let t    = typeOfExists i
                   let ctx' = pushExists i ctx
                   let b'   = replaceTypeOfBind t b
                   return (b', ctx')

             -- Recursive let-binding has a type annotation,
             -- so check it, expecting it to have kind Data.
             | otherwise
             -> do
                (b0, _k, ctx1)
                 <- checkBindM config ctx UniverseSpec b (Check kData)

                let t0  =  typeOfBind b0
                t1      <- applyContext ctx1 t0
                let b1  =  replaceTypeOfBind t1 b0

                return (b1, ctx1)


---------------------------------------------------------------------------------------------------
-- | Check some recursive bindings.
--   Doing this won't push any more bindings onto the context,
--   though it may solve some existentials in it.
checkRecBindExps
        :: (Show a, Show n, Ord n, Pretty n)
        => Table a n
        -> Bool                         -- ^ Use bidirectional checking.
        -> a                            -- ^ Annotation for error messages.
        -> Context n                    -- ^ Original context.
        -> Demand                       -- ^ Demand placed on bindings.
        -> [(Bind n, Exp a n)]          -- ^ Bindings and exps for rec bindings.
        -> CheckM a n
                ( [ ( Bind n                   -- Result bindiner.
                    , Exp (AnTEC a n) n)]      -- Result expression.
                , Context n)

checkRecBindExps table False a ctx0 demand bxs0
 = go bxs0 ctx0
 where
        go [] ctx
         = return ([], ctx)

        go ((b, xBind) : bxs) ctx
         = do
                ctrace  $ vcat
                        [ text "*>  Let Rec Bind RECON"
                        , text "    demand     = " <> (text $ show demand)
                        , text "    in binder  = " <> ppr (binderOfBind b)
                        , text "    in type    = " <> ppr (typeOfBind   b)
                        , empty ]

                -- Check the right of the binding.
                --  We checked that the expression is a syntactic lambda
                --  abstraction in checkLetsM, so we know the effect is pure.
                (xBind', t, _effs, ctx')
                 <- tableCheckExp table table ctx Recon demand xBind

                -- Check the annotation on the binder matches the reconstructed
                -- type of the binding.
                when (not $ equivT (contextEnvT ctx') (typeOfBind b) t)
                 $ throw $ ErrorMismatch a (typeOfBind b) t xBind

                -- Reconstructing the types of binders adds missing kind info to
                -- constructors etc, so update the binders with this new info.
                let b'  = replaceTypeOfBind t b

                ctrace  $ vcat
                        [ text "*<  Let Rec Bind RECON"
                        , text "    demand     =" <+> (text $ show demand)
                        , text "    in  binder =" <+> ppr (binderOfBind b)
                        , text "    in  type   =" <+> ppr (typeOfBind   b)
                        , text "    out type   =" <+> ppr t
                        , empty ]

                -- Check the rest of the bindings.
                (moar,   ctx'') <- go bxs ctx'

                return ((b', xBind') : moar, ctx'')

checkRecBindExps table True _a ctx0 demand bxs0
 = go bxs0 ctx0
 where
        go [] ctx
         = return ([], ctx)

        go ((b, xBind) : bxs) ctx
         = do
                ctrace  $ vcat
                        [ text "*>  Let Rec Bind BIDIR"
                        , text "    demand    =" <+> (text $ show demand)
                        , text "    in binder =" <+> ppr (binderOfBind b)
                        , text "    in type   =" <+> ppr (typeOfBind   b)
                        , empty ]

                -- Check the right of the binding.
                --  We checked that the expression is a syntactic lambda
                --  abstraction in checkLetsM, so we know the effect is pure.
                (xBind', t, _effs, ctx')
                 <- tableCheckExp table table ctx
                        (Check (typeOfBind b)) demand xBind

                -- Reconstructing the types of binders adds missing kind info to
                -- constructors etc, so update the binders with this new info.
                let b'  = replaceTypeOfBind t b

                ctrace  $ vcat
                        [ text "*<  Let Rec Bind BIDIR"
                        , text "    demand     =" <+> (text $ show demand)
                        , text "    in  binder =" <+> ppr (binderOfBind b)
                        , text "    in  type   =" <+> ppr (typeOfBind   b)
                        , text "    out type   =" <+> ppr t
                        , empty ]

                -- Check the rest of the bindings.
                (moar, ctx'') <- go bxs ctx'

                return ((b', xBind') : moar, ctx'')


---------------------------------------------------------------------------------------------------
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


---------------------------------------------------------------------------------------------------
-- | Check that all the bindings in a recursive let are syntactic lambdas.
--   We don't support value recursion, so can only define recursive functions.
--   If one of the expression is not a lambda then throw an error.
checkSyntacticLambdas
        :: Table a n
        -> a                    -- ^ Annotation for error messages.
        -> Exp a n              -- ^ Expression for error message.
        -> [Exp a n]            -- ^ Expressions to check.
        -> CheckM a n ()

checkSyntacticLambdas table a xx xs
        | configGeneralLetRec $ tableConfig table
        = return ()

        | otherwise
        = forM_ xs $ \x
                -> when (not $ (isXLam x || isXLAM x))
                $  throw $ ErrorLetrecBindingNotLambda a xx x

