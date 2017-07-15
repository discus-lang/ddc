{-# LANGUAGE TypeFamilies, OverloadedStrings #-}
-- | Freshen shadowed names in the source file,
--   and rewrite anonymous binders to their named forms.
module DDC.Source.Tetra.Transform.Freshen
        ( type S, evalState, newName
        , freshenModule)
where
import DDC.Source.Tetra.Transform.Freshen.State
import DDC.Source.Tetra.Module
import DDC.Source.Tetra.Exp
import qualified Control.Monad.State    as S
import qualified Data.Set               as Set
import qualified Data.Map.Strict        as Map
import qualified Data.List              as List


-------------------------------------------------------------------------------
-- | Freshen the given module.
freshenModule :: Module Source -> S (Module Source)
freshenModule mm
 = do   tops'   <- freshenTops $ moduleTops mm
        return  $  mm { moduleTops = tops' }


-------------------------------------------------------------------------------
-- | Freshen a top level thing.
freshenTops :: [Top Source] -> S [Top Source]
freshenTops tops
 = do
        let (topCls, topRest) = List.partition isTopClause tops
        let (sps,    cls)     = unzip $ [(sp, cl) | TopClause sp cl <- topCls]
        cls'        <- freshenClauseGroup cls
        let topCls' =  zipWith TopClause sps cls'
        return  $ topRest ++ topCls'


-- | Check if this top level thing is a clause.
isTopClause :: Top Source -> Bool
isTopClause tt
 = case tt of
        TopClause{}     -> True
        _               -> False


-------------------------------------------------------------------------------
-- | Freshen a clause group.
freshenClauseGroup :: [Clause] -> S [Clause]
freshenClauseGroup cls
 =  freshenClauseGroupBinds cls $ \cls'
 -> mapM freshenClauseBody cls'


-- | Freshen the binders in a group of clauses.
--
--   In the group, we may have several clauses that define the same function.
--   If we freshen the name of one of those clauses, we need to use the same
--   name for the other associated ones.
--
--   All the new names for the clauses are in scope in the continuation,
--   which can then be used to freshen the bodies.
--
freshenClauseGroupBinds
        :: [Clause] -> ([Clause] -> S a) -> S a

freshenClauseGroupBinds cls0 cont
 = do   envTopX <- S.gets stateEnvX
        go envTopX [] cls0
 where
        go _envTopX clsAcc []
         = cont (reverse clsAcc)

        -- Signatures.
        go envTopX clsAcc ((SSig a b t) : clsRest)
         = do   -- Freshen the signature type.
                t'      <- freshenType t

                -- Check if we've already renamed this variable in the
                -- current clause group.
                envX    <- S.gets stateEnvX
                case b of
                 BName name
                  -- We've already renamed one of the clauses in the same
                  -- group to a new name, so use that for matching ones.
                  | Just name' <- Map.lookup name (envRename envX)
                  -> do let b'   = BName name'
                        let cls' = SSig a b' t'
                        go envTopX (cls' : clsAcc) clsRest

                 -- Bind the variable into the current environment,
                 -- renaming it if it was already visible in the context
                 -- of the current clause group.
                 _
                  -> bindCtxBX envTopX b $ \b'
                  -> do let cls' = SSig a b' t'
                        go envTopX (cls' : clsAcc) clsRest


        -- Binding Clauses.
        go envTopX clsAcc ((SLet a (XBindVarMT b mt) ps gxs) : clsRest)
         = do   -- Freshen the type on the binder.
                mt'     <- traverse freshenType mt

                -- Check if we've already renamed this variable in the
                -- current clause group.
                envX    <- S.gets stateEnvX
                case b of
                 BName name
                  -- We've already renamed one of the clauses in the same
                  -- group to a new name, so use that for matching ones.
                  |  Just name'  <- Map.lookup name (envRename envX)
                  -> do let b'   = BName name'
                        let cls' = SLet a (XBindVarMT b' mt') ps gxs
                        go envTopX (cls' : clsAcc) clsRest

                 -- Bind the variable into the current environment,
                 -- renaming it if it was already visible in the context
                 -- of the current clause group.
                 _
                  -> bindCtxBX envTopX b $ \b'
                  -> do let cls' = SLet a (XBindVarMT b' mt') ps gxs
                        go envTopX (cls' : clsAcc) clsRest


-- | Freshen a clause.
freshenClauseBody :: Clause -> S Clause
freshenClauseBody cl
 = case cl of
        SSig{}
         -> return cl

        SLet a b ps gxs
         -> mapFreshBinds bindParam ps $ \ps'
         -> SLet a b ps' <$> mapM freshenGuardedExp gxs


-------------------------------------------------------------------------------
-- | Freshen and bind a function parameter.
bindParam :: Param -> (Param -> S a) -> S a
bindParam pp cont
 = case pp of
        MType b mt
         -> bindBT b $ \b'
         -> do  mt'     <- traverse freshenType mt
                cont $ MType b' mt'

        MTerm p mt
         -> bindPat p $ \p'
         -> do  mt'     <- traverse freshenType mt
                cont $ MTerm p' mt'

        MImplicit p mt
         -> bindPat p $ \p'
         -> do  mt'     <- traverse freshenType mt
                cont $ MImplicit p' mt'


-------------------------------------------------------------------------------
-- | Freshen a guarded expression.
freshenGuardedExp :: GuardedExp -> S GuardedExp
freshenGuardedExp gx
 = case gx of
        GGuard g gx'
         -> bindGuard g $ \g'
         -> GGuard g' <$> freshenGuardedExp gx'

        GExp x
         -> do  x'      <- freshenExp x
                return  $  GExp x'


-------------------------------------------------------------------------------
-- | Freshen an expression.
freshenExp :: Exp -> S Exp
freshenExp xx
 = case xx of
        XAnnot a x      -> XAnnot a <$> freshenExp x
        XPrim{}         -> return xx
        XFrag{}         -> return xx
        XVar u          -> XVar     <$> boundUX u
        XCon{}          -> return xx

        XAbs (MType b mt) x
         -> do  mt'     <- traverse freshenType mt
                bindBT b $ \b'
                 -> XAbs (MType b' mt') <$> freshenExp x

        XAbs (MTerm p mt) x
         -> do  mt'     <- traverse freshenType mt
                bindPat p $ \p'
                 -> XAbs (MTerm p' mt') <$> freshenExp x

        XAbs (MImplicit p mt) x
         -> do  mt'     <- traverse freshenType mt
                bindPat p $ \p'
                 -> XAbs (MImplicit p' mt') <$> freshenExp x

        XApp x1 a2      -> XApp  <$> freshenExp x1 <*> freshenArg a2

        XLet lts x
         -> bindLets lts $ \lts'
         -> XLet lts' <$> freshenExp x

        XCase x alts    -> XCase    <$> freshenExp x
                                    <*> mapM freshenAltCase alts

        XCast c x       -> XCast    <$> freshenCast c <*> freshenExp x

        XDefix a rs     -> XDefix a <$> mapM freshenArg rs

        XInfixOp{}      -> return xx

        XInfixVar{}     -> return xx

        XMatch a as x   -> XMatch a <$> mapM freshenAltMatch as
                                    <*> freshenExp x

        XWhere a x cl   -> XWhere a <$> freshenExp x <*> freshenClauseGroup cl

        XAbsPat a ps p mt x
         -> do  mt'     <- traverse freshenType mt
                bindPat p $ \p'
                 -> XAbsPat a ps p' mt' <$> freshenExp x

        XLamCase a as
         -> XLamCase a   <$> mapM freshenAltCase as


-------------------------------------------------------------------------------
-- | Freshen an argument.
freshenArg :: Arg -> S Arg
freshenArg arg
 = case arg of
        RType     t     -> RType     <$> freshenType    t
        RWitness  w     -> RWitness  <$> freshenWitness w
        RTerm     x     -> RTerm     <$> freshenExp     x
        RImplicit arg'  -> RImplicit <$> freshenArg     arg'


-------------------------------------------------------------------------------
-- | Freshen and bind guards.
bindGuard   :: Guard -> (Guard -> S a) -> S a
bindGuard gg cont
 = case gg of
        GPat p x
         -> bindPat p $ \p'
         -> cont =<< (GPat p' <$> freshenExp x)

        GPred x
         -> cont =<< (GPred   <$> freshenExp x)

        GDefault
         -> cont GDefault


-------------------------------------------------------------------------------
-- | Freshen and bind let expressions.
bindLets :: Lets -> (Lets -> S a) -> S a
bindLets lts cont
 = case lts of
        LLet (XBindVarMT b mt) x
         -> do  mt'     <- traverse freshenType mt
                bindBX b $ \b'
                 -> cont =<< (LLet (XBindVarMT b' mt') <$> freshenExp x)

        LRec bxs
         -> do  let (bs, xs)    = unzip bxs
                mapFreshBinds bindBVX bs $ \bs'
                 -> do  xs'     <- mapM freshenExp xs
                        cont (LRec $ zip bs' xs')

        LPrivate brs mt bwts
         -> do  mt'     <- traverse freshenType mt
                mapFreshBinds bindBT brs $ \brs'
                 -> do  let (bws, ts)  = unzip bwts
                        ts'     <- mapM freshenType ts
                        mapFreshBinds bindBX bws $ \bws'
                         -> cont (LPrivate brs' mt' $ zip bws' ts')

        LGroup bRec cls
         -> cont =<< (LGroup bRec <$> freshenClauseGroup cls)


-------------------------------------------------------------------------------
-- | Freshen a case alternative.
freshenAltCase :: AltCase -> S AltCase
freshenAltCase (AAltCase p gxs)
 =  bindPat p $ \p'
 -> AAltCase p' <$> mapM freshenGuardedExp gxs


-- | Freshen a match alternative
freshenAltMatch :: AltMatch -> S AltMatch
freshenAltMatch (AAltMatch gx)
 = AAltMatch <$> freshenGuardedExp gx


-------------------------------------------------------------------------------
-- | Freshen a cast.
freshenCast :: Cast -> S Cast
freshenCast cc
 = case cc of
        CastWeakenEffect t -> CastWeakenEffect <$> freshenType t
        CastBox            -> return CastBox
        CastRun            -> return CastRun


-------------------------------------------------------------------------------
-- | Freshen a witness.
freshenWitness :: Witness -> S Witness
freshenWitness ww
 = case ww of
        WAnnot a w      -> WAnnot a <$> freshenWitness w
        WVar  u         -> WVar  <$> boundUX u
        WCon {}         -> return ww
        WApp  w1 w2     -> WApp  <$> freshenWitness w1 <*> freshenWitness w2
        WType t         -> WType <$> freshenType t


-------------------------------------------------------------------------------
-- | Freshen a type.
freshenType :: Type -> S Type
freshenType tt
 = case tt of
        TAnnot a t      -> TAnnot a <$> freshenType t

        TCon{}          -> return tt

        TVar u          -> TVar <$> boundUT u

        TAbs b t1 t2
         -> do  t1'     <- freshenType t1
                bindBT b $ \b'
                 -> TAbs b' t1' <$> freshenType t2

        TApp t1 t2      -> TApp <$> freshenType t1 <*> freshenType t2


-------------------------------------------------------------------------------
-- | Bind a pattern.
bindPat  :: Pat -> (Pat -> S a) -> S a
bindPat pp cont
 = case pp of
        PDefault
         -> cont pp

        PAt  b p
         -> bindBX  b $ \b'
         -> bindPat p $ \p'
         -> cont (PAt b' p')

        PVar b
         -> bindBX b  $ \b'
         -> cont (PVar b')

        PData dc ps
         -> mapFreshBinds bindPat ps $ \ps'
         -> cont (PData dc ps')


-------------------------------------------------------------------------------
-- | Bind a new type variable.
bindBT :: Bind -> (Bind -> S a) -> S a
bindBT b@BNone cont
 = cont b

bindBT BAnon cont
 = do   -- Create a new name for anonymous binders.
        name    <- newName "t"

        withModifiedEnvT
         (\envT -> envT { envStack    = name : envStack envT
                        , envStackLen = 1 + envStackLen envT })
         $ cont (BName name)

bindBT (BName n) cont
 =  S.get >>= \state0
 -> case Set.member n (envNames $ stateEnvT state0) of
     -- If the binder does not shadow an existing one
     -- then don't bother rewriting it.
     False
      ->     withModifiedEnvT
              (\envT -> envT { envNames = Set.insert n (envNames envT) })
              $ cont (BName n)

     -- The binder shadows an existing one, so rewrite it.
     True
      -> do name    <- newName "t"

            -- Run the continuation in the extended environment.
            withModifiedEnvT
             (\envT -> envT { envNames  = Set.insert   name (envNames  envT)
                            , envRename = Map.insert n name (envRename envT)})
             $ cont (BName name)


-------------------------------------------------------------------------------
-- | Bind a term variable with its attached type.
bindBVX :: BindVarMT -> (BindVarMT -> S a) -> S a
bindBVX (XBindVarMT b mt) cont
 = do   mt'     <- traverse freshenType mt
        bindBX b $ \b'
         -> cont (XBindVarMT b' mt')


-- | Bind a new term variable,
--   renaming it if it is already in the current environment.
bindBX :: Bind -> (Bind -> S a) -> S a
bindBX b cont
 = do   envX    <- S.gets stateEnvX
        bindCtxBX envX b cont


-- | Bind a new term variable,
--   renaming it if it is already in the given environment.
bindCtxBX :: Env -> Bind -> (Bind -> S a) -> S a

bindCtxBX _envCtx b@BNone cont
 = cont b

bindCtxBX _envCtx BAnon cont
 = do   -- Create a new name for anonymous binders.
        name    <- newName "x"

        withModifiedEnvX
         (\envX -> envX { envStack    = name : envStack envX
                        , envStackLen = 1 + envStackLen envX })
         $ cont (BName name)

bindCtxBX envCtx (BName name) cont
 = case Set.member name (envNames envCtx) of
        -- If the binder does not shadow an existing one
        -- then don't bother rewriting it.
        False
         -> do  withModifiedEnvX
                 (\envX -> envX
                        {  envNames = Set.insert name (envNames envX)})
                 $ cont (BName name)

        -- The binder shadows an existing one, so rewrite it.
        True
         -> do  name'   <- newName name

                -- Run the continuation in the environment extended with the
                -- new name.
                withModifiedEnvX
                 (\envX -> envX
                        {  envNames  = Set.insert      name' (envNames  envX)
                        ,  envRename = Map.insert name name' (envRename envX)})
                 $ cont (BName name')


-------------------------------------------------------------------------------
-- | Rewrite bound type variable if needed.
boundUT :: Bound -> S Bound
boundUT uu
 = case uu of
        UName n
         -> do  envT    <- S.gets (envRename   . stateEnvT)
                case Map.lookup n envT of
                 Just n'  -> return $ UName n'
                 _        -> return uu

        UIx i
         -> do  stack   <- S.gets (envStack    . stateEnvT)
                len     <- S.gets (envStackLen . stateEnvT)
                if (i < len)
                 then   return $ UName (stack !! i)
                 else   return uu

        UHole{}
         -> return uu


-------------------------------------------------------------------------------
-- | Rewrite bound term variable if needed.
boundUX :: Bound -> S Bound
boundUX uu
 = case uu of
        UName n
         -> do  envX    <- S.gets (envRename   . stateEnvX)
                case Map.lookup n envX of
                 Just n'  -> return $ UName n'
                 _        -> return uu

        UIx i
         -> do  stack   <- S.gets (envStack    . stateEnvX)
                len     <- S.gets (envStackLen . stateEnvX)
                if (i < len)
                 then   return $ UName (stack !! i)
                 else   return uu

        UHole{}
         -> return uu


