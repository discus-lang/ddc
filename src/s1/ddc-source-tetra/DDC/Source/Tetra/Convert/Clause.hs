{-# OPTIONS_HADDOCK hide #-}
module DDC.Source.Tetra.Convert.Clause
        ( collectSigsFromClauses
        , collectBoundVarsFromClauses
        , makeBindingFromClause)
where
import DDC.Source.Tetra.Convert.Error
import DDC.Source.Tetra.Convert.Base
import qualified DDC.Source.Tetra.Exp                   as S


-- | Collect type signatures defined in a clause group.
collectSigsFromClauses      :: [S.Clause] -> [(S.BindVar, S.Type)]
collectSigsFromClauses cls
 = go cls
 where  go (S.SSig _ b t : cls')
                        = (b, t) : go cls'
        go (_ : cls')   = go cls'
        go []           = []


-- | Collect binders for values defined in a clause group.
collectBoundVarsFromClauses :: [S.Clause] -> [S.BindVar]
collectBoundVarsFromClauses cls
 = go cls
 where  go (S.SLet _ (S.XBindVarMT b _) _ _ : cls')
                        = b : go cls'
        go (_ : cls')   = go cls'
        go []           = []


-- | Strip a let-binding from a clause.
makeBindingFromClause
        :: [(S.BindVar, S.Type)]        -- ^ Type signatures in the same group.
        -> [ S.BindVar ]                -- ^ Bound values defined in the same group.
        -> S.Clause                     -- ^ Clause to consider.
        -> ConvertM S.Source
                    (Maybe (S.BindVarMT, (SP, S.Exp)))
                                        -- ^ Let-bindings with attached signatures.
makeBindingFromClause sigs vals cc
 = case cc of
        S.SLet sp bm@(S.XBindVarMT b mtHas) ps [S.GExp x]
         -- See if there was a type signature specified in the same group.
         |  Just tSig   <- lookup b sigs
         -> case mtHas of
                -- If the binder was already directly annotated with a signature
                -- then throw an error, as it might conflict with the separate
                -- signature provided in the same group.
                Just _          -> Left   $ ErrorMultipleSignatures sp b

                -- The binder was not directly annotated,
                -- so attach the provided signature.
                Nothing
                 -> case wrapParams ps x of
                        Nothing -> Left   $ ErrorConvertSugaredClause cc
                        Just x' -> return $ Just $ ( S.XBindVarMT b (Just tSig), (sp, x'))

         -- We don't have a separate signature for this binding.
         |  otherwise
         -> case wrapParams ps x of
                Nothing         -> Left   $ ErrorConvertSugaredClause cc
                Just x'         -> return $ Just $ (bm, (sp, x'))

        -- Some let binding with an expression that should have
        -- been desugared earlier.
        S.SLet{}                -> Left   $ ErrorConvertSugaredClause cc

        -- Check that signatures in the clause group have associated bindings.
        --   If we find a signature without a binding then one or
        --   the other is probably mis-spelled.
        S.SSig sp b _
         | elem b vals          -> return Nothing
         | otherwise            -> Left   $ ErrorTypeSignatureLacksBinding sp b


-- | Wrap an expression with lambda abstractions for each of the given parameters.
wrapParams :: [S.Param] -> S.Exp -> Maybe S.Exp
wrapParams [] x
 = pure x

wrapParams (p:ps) x
 = case p of
        S.MType  b mt
         -> S.XAbs (S.MType b mt)       <$> wrapParams ps x

        S.MTerm  p' mt
         -> S.XAbs (S.MTerm p' mt)      <$> wrapParams ps x

        S.MImplicit   p' mt
         -> S.XAbs (S.MImplicit p' mt)  <$> wrapParams ps x



