
module DDC.Core.Check.Judge.Type.Let
        (checkLet)
where
import DDC.Core.Check.Judge.Type.Base
import qualified DDC.Type.Sum   as Sum
import Data.List                as L


checkLet :: Checker a n

-- let --------------------------------------------
checkLet !table !ctx0 xx@(XLet a lts xBody) mode
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

        (lts', _bs', effsBinds, pos1, ctx1)
         <- checkLetsM useBidirChecking xx table ctx0 lts


        -- Check the body -----------------------
        -- -- Check the body expression in a context
        -- -- extended with the types of the bindings.
        (xBody', tBody, effsBody, ctx2)
         <- tableCheckExp table table ctx1 xBody mode

        -- The body must have data kind.
        (tBody', kBody, ctx3)
         <- checkTypeM config kenv ctx2 UniverseSpec tBody
         $  case mode of
                Recon   -> Recon
                _       -> Check kData

        let kBody' = applyContext ctx3 kBody
        when (not $ isDataKind kBody')
         $ throw $ ErrorLetBodyNotData a xx tBody kBody'


        -- Build the result ---------------------
        -- The new effect and closure.
        let tResult     = applyContext ctx3 tBody'
        let effs'       = effsBinds `Sum.union` effsBody

        -- Pop the elements due to the let-bindings from the context.
        let ctx_cut     = popToPos pos1 ctx3

        ctrace  $ vcat
                [ text "* Let"
                , indent 2 $ ppr xx
                , text "  tResult:  " <> ppr tResult
                , indent 2 $ ppr ctx3
                , indent 2 $ ppr ctx_cut ]

        returnX a (\z -> XLet z lts' xBody')
                tResult effs' ctx_cut


-- others ---------------------------------------
-- The dispatcher should only call checkLet with a XLet AST node.
checkLet _ _ _ _
        = error "ddc-core.checkLet: no match"


-------------------------------------------------------------------------------
-- | Check some let bindings,
--   and push their binders onto the context.
checkLetsM
        :: (Show a, Show n, Pretty n, Ord n)
        => Bool                         -- ^ Use bidirectional inference.
        -> Exp a n                      -- ^ Expression for error messages.
        -> Table a n                    -- ^ Static configuration.
        -> Context n                    -- ^ Input context.
        -> Lets a n                     -- ^ Let-bindings to check.
        -> CheckM a n
                ( Lets (AnTEC a n) n    --   Let-bindings annotated with types.
                , [Bind n]              --   Binding occs of vars, with types.
                , TypeSum n             --   Effect of evaluating all the bindings.
                , Pos                   --   Context position with bindings pushed.
                , Context n)            --   Output context.

checkLetsM !bidir xx !table !ctx0 (LLet b xBind)

 -- Reconstruct the type of a non-recursive let-binding.
 | False  <- bidir
 = do
        let config      = tableConfig table
        let kenv        = tableKindEnv table
        let a           = annotOfExp xx

        -- Reconstruct the type of the binding.
        (xBind', tBind, effsBind, ctx1)
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
        (xBind', tBind1, effsBind, ctx2)
         <- tableCheckExp table table ctx1 xBind modeCheck

        -- Update the annotation on the binder with the actual type of
        -- the binding.
        let tBind2      = applyContext ctx2 tBind1
        let b'          = replaceTypeOfBind tBind2 b

        -- Push the binder on the context.
        let (ctx3, pos1) =  markContext ctx2
        let ctx4         =  pushType b' ctx3

        return  ( LLet b' xBind'
                , [b']
                , effsBind
                , pos1, ctx4)


-- letrec ---------------------------------------
checkLetsM !bidir !xx !table !ctx0 (LRec bxs)
 = do   let (bs, xs)    = unzip bxs
        let a           = annotOfExp xx

        -- Named binders cannot be multiply defined.
        checkNoDuplicateBindings a xx bs

        -- All right hand sides must be syntactic abstractions.
        checkSyntacticLambdas table a xx xs

        -- Check the type annotations on all the binders.
        (bs', ctx1)      <- checkRecBinds table bidir a xx ctx0 bs

        -- All variables are in scope in all right hand sides.
        let (ctx2, pos1) =  markContext ctx1
        let ctx3         =  pushTypes bs' ctx2

        -- Check the right hand sides.
        (results, ctx4)  <- checkRecBindExps table bidir a ctx3 (zip bs' xs)

        let (bs'', xsRight')
                = unzip results

        return  ( LRec (zip bs'' xsRight')
                , bs''
                , Sum.empty kEffect
                , pos1, ctx4)


-- others ---------------------------------------
-- The dispatcher should only call checkLet with LLet and LRec AST nodes,
-- so we should not see the others here.
checkLetsM _ _ _ _ _
        = error "ddc-core.checkLetsM: no match"


-------------------------------------------------------------------------------
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
        kenv    = tableKindEnv table

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
                 <- checkBindM config kenv ctx UniverseSpec b Recon

                -- The type on the binder must have kind Data.
                when (not $ isDataKind k)
                 $ throw $ ErrorLetBindingNotData a xx b' k

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
                 <- checkBindM config kenv ctx UniverseSpec b (Check kData)

                let t0  = typeOfBind b0
                let t1  = applyContext ctx1 t0
                let b1  = replaceTypeOfBind t1 b0

                return (b1, ctx1)


-------------------------------------------------------------------------------
-- | Check some recursive bindings.
--   Doing this won't push any more bindings onto the context,
--   though it may solve some existentials in it.
checkRecBindExps
        :: (Show a, Show n, Ord n, Pretty n)
        => Table a n
        -> Bool                         -- ^ Use bidirectional checking.
        -> a                            -- ^ Annotation for error messages.
        -> Context n                    -- ^ Original context.
        -> [(Bind n, Exp a n)]          -- ^ Bindings and exps for rec bindings.
        -> CheckM a n
                ( [ ( Bind n                   -- Result bindiner.
                    , Exp (AnTEC a n) n)]      -- Result expression.
                , Context n)

checkRecBindExps table bidir a ctx0 bxs0
 = go bxs0 ctx0
 where
        go [] ctx
         = return ([], ctx)

        go ((b, x) : bxs) ctx
         = do   (result, ctx')  <- checkRecBindExp b x ctx
                (moar,   ctx'') <- go bxs ctx'
                return (result : moar, ctx'')

        checkRecBindExp b x ctx
         = case bidir of
            False
             -> do
                -- Check the right of the binding.
                --  We checked that the expression is a syntactic lambda
                --  abstraction in checkLetsM, so we know the effect is pure.
                (x', t, _effs, ctx')
                 <- tableCheckExp table table ctx x Recon

                -- Check the annotation on the binder matches the reconstructed
                -- type of the binding.
                when (not $ equivT (typeOfBind b) t)
                 $ throw $ ErrorLetMismatch a x b t

                -- Reconstructing the types of binders adds missing kind info to
                -- constructors etc, so update the binders with this new info.
                let b'  = replaceTypeOfBind t b

                return ( (b', x'), ctx')

            True
             -> do
                -- Check the right of the binding.
                --  We checked that the expression is a syntactic lambda
                --  abstraction in checkLetsM, so we know the effect is pure.
                (x', t, _effs, ctx')
                 <- tableCheckExp table table ctx x (Check (typeOfBind b))

                -- Reconstructing the types of binders adds missing kind info to
                -- constructors etc, so update the binders with this new info.
                let b'  = replaceTypeOfBind t b

                return ((b', x'), ctx')


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



