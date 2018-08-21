{-# OPTIONS_HADDOCK hide #-}
module DDC.Source.Discus.Convert.Type
        ( toCoreTypeDef

        , toCoreT
        , toCoreTC

        , toCoreTBCN,   toCoreTUCN
        , toCoreXUVN,   toCoreXBVN

        , toCoreTBK

        , toCoreParam
        , toCoreB,      toCoreBM
        , toCoreU
        , toCoreDaConBind
        , toCoreDaConBound)
where
import DDC.Source.Discus.Convert.Prim
import DDC.Source.Discus.Convert.Base
import DDC.Type.Universe                        (Universe (..), universeUp)
import qualified DDC.Source.Discus.Exp          as S
import qualified DDC.Core.Discus.Compounds      as C
import qualified DDC.Core.Discus.Prim           as C
import qualified DDC.Type.Sum                   as CSum


-- TypeDef ----------------------------------------------------------------------------------------
toCoreTypeDef
        :: (S.TyConBind, S.Type)
        -> ConvertM a (C.Name, (C.Kind C.Name, C.Type C.Name))

toCoreTypeDef (b, t)
 = do   n       <- toCoreTBCN b
        t'      <- toCoreT UniverseSpec t
        return  (n, (C.kData, t'))


-- Type -------------------------------------------------------------------------------------------
toCoreT :: Universe -> S.Type -> ConvertM a (C.Type C.Name)
toCoreT uu tt
 = case tt of
        S.TAnnot _ t
         -> toCoreT uu t

        S.TCon (S.TyConBot k)
         -> do  k'      <- toCoreT uu k
                return  $ C.tBot k'

        S.TCon (S.TyConVoid)
         -> do  return  $ C.tVoid

        S.TCon tc
         -> do  mtc'    <- toCoreTC uu tc
                case mtc' of
                 Nothing        -> error  $ "ddc-soure-discus.toCoreT: " ++ show tt
                 Just tc'       -> return $ C.TCon tc'

        S.TVar u
         -> C.TVar <$> toCoreU  u

        S.TAbs b k t
         -> do  b'      <- toCoreTBK (b, k)
                t'      <- toCoreT uu  t
                return  $  C.TAbs b' t'

        S.TApp (S.TCon (S.TyConForall _)) (S.TAbs b k t)
         -> let uu'     =  universeUp uu
            in  C.TForall <$> toCoreBM uu' (S.XBindVarMT b (Just k)) <*> toCoreT uu t

        S.TApp{}
         | Just (k, ts) <- S.takeTUnions tt
         -> do  let uu' =  universeUp uu
                k'      <- toCoreT uu' k
                ts'     <- sequence $ fmap (toCoreT uu) ts
                return  $  C.TSum (CSum.fromList k' ts')

        S.TApp t1 t2
         -> C.TApp      <$> toCoreT uu t1 <*> toCoreT uu t2


-- TyCon ------------------------------------------------------------------------------------------
-- | Convert a Source TyCon to Core, or Nothing if it cannot be converted in isolation.
toCoreTC :: Universe -> S.TyCon -> ConvertM a (Maybe (C.TyCon C.Name))
toCoreTC uu tc
 = case tc of
        S.TyConVoid             -> return Nothing
        S.TyConUnit             -> return $ Just $ C.TyConSpec C.TcConUnit

        S.TyConFunExplicit
         -> case uu of
                UniverseSpec    -> return $ Just $ C.TyConSpec C.TcConFunExplicit
                UniverseKind    -> return $ Just $ C.TyConKind C.KiConFun
                _               -> return Nothing

        S.TyConFunImplicit
         -> case uu of
                UniverseSpec    -> return $ Just $ C.TyConSpec C.TcConFunImplicit
                _               -> return Nothing


        S.TyConUnion _   -> return Nothing

        S.TyConBot _k    -> return Nothing
        S.TyConForall _k -> return Nothing
        S.TyConExists _k -> return Nothing

        -- Primitive type constructors.
        S.TyConPrim pt
         -> case pt of
                -- Ambient TyCons
                S.TyConPrimSoCon sc -> return $ Just $ C.TyConSort    sc
                S.TyConPrimKiCon kc -> return $ Just $ C.TyConKind    kc
                S.TyConPrimTwCon tw -> return $ Just $ C.TyConWitness tw
                S.TyConPrimTcCon ts -> return $ Just $ C.TyConSpec    ts

                -- Primitive TyCons
                S.TyConPrimTyCon tcy
                 ->     return  $ Just $ C.TyConBound (C.NamePrimTyCon tcy)

                S.TyConPrimDiscus tct
                 -> do  let tct' =  toCoreTyConDiscus tct
                        return  $ Just $ C.TyConBound (C.NameTyConDiscus tct')

        -- Bound type constructors.
        --   The embedded kind is set to Bot. We rely on the spreader
        --   to fill in the real kind before type checking.
        S.TyConBound (S.TyConBoundName tx)
         -> return $ Just $ C.TyConBound (C.NameCon tx)


-- Bind -------------------------------------------------------------------------------------------
-- | Convert a type constructor binding occurrence to a core name.
toCoreTBCN :: S.TyConBind  -> ConvertM a C.Name
toCoreTBCN (S.TyConBindName n)
 = return $ C.NameCon n


-- | Convert a type constructor bound occurrence to a core name.
toCoreTUCN :: S.TyConBound -> ConvertM a C.Name
toCoreTUCN (S.TyConBoundName n)
 = return $ C.NameCon n


-- | Convert a term variable bound occurrence to a core name.
toCoreXUVN :: S.Bound -> ConvertM a C.Name
toCoreXUVN uu
 = case uu of
        S.UName n -> return $ C.NameVar n
        S.UIx  _i -> error "ddc-source-discus.toCoreXBVN: anon bound"
        S.UHole   -> return $ C.NameHole


toCoreXBVN  :: S.Bind -> ConvertM a C.Name
toCoreXBVN bb
 = case bb of
        S.BNone   -> error "ddc-source-discus.toCoreXBVN: none bound"
        S.BAnon   -> error "ddc-source-discus.toCoreXBVN: anon bound"
        S.BName n -> return $ C.NameVar n


-- | Convert a type binder and kind to core.
toCoreTBK :: (S.Bind, S.GType S.SourcePos)
          -> ConvertM a (C.Bind C.Name)
toCoreTBK (bb, k)
 = case bb of
        S.BNone   -> C.BNone <$> (toCoreT UniverseKind k)
        S.BAnon   -> C.BAnon <$> (toCoreT UniverseKind k)
        S.BName n -> C.BName <$> (return $ C.NameVar n)
                             <*> (toCoreT UniverseKind k)


-- | Convert an unannoted binder to core.
toCoreB  :: S.Bind -> ConvertM a (C.Bind C.Name)
toCoreB bb
 = let hole     = C.TVar (C.UName C.NameHole)
   in case bb of
        S.BNone   -> return $ C.BNone hole
        S.BAnon   -> return $ C.BAnon hole
        S.BName n -> return $ C.BName (C.NameVar n) hole


-- | Convert a parameter to core.
toCoreParam  :: S.Param -> ConvertM a (C.Param C.Name)
toCoreParam pp
 = case pp of
        S.MType b mt
         -> C.MType     <$> toCoreBT UniverseKind b mt

        S.MTerm p mt
         -> C.MTerm     <$> toCorePT UniverseSpec p mt

        S.MImplicit p mt
         -> C.MImplicit <$> toCorePT UniverseSpec p mt


-- | Convert a typed pattern to core.
toCorePT :: Universe -> S.Pat -> Maybe S.Type -> ConvertM a (C.Bind C.Name)
toCorePT uu p mt
 = do   t' <- case mt of
               Nothing -> return $ C.TVar (C.UName C.NameHole)
               Just t  -> toCoreT uu t

        case p of
         S.PDefault         -> return $ C.BNone t'
         S.PVar (S.BNone)   -> return $ C.BNone t'
         S.PVar (S.BAnon)   -> return $ C.BAnon t'
         S.PVar (S.BName n) -> return $ C.BName (C.NameVar n) t'
         _                  -> error "ddc-source-discus.toCorePT: bad pattern"


toCoreBT :: Universe -> S.Bind -> Maybe S.Type -> ConvertM a (C.Bind C.Name)
toCoreBT uu b mt
 = do   t' <- case mt of
               Nothing -> return $ C.TVar (C.UName C.NameHole)
               Just t  -> toCoreT uu t

        case b of
         S.BNone        -> return $ C.BNone t'
         S.BAnon        -> return $ C.BAnon t'
         S.BName n      -> return $ C.BName (C.NameVar n) t'


-- | Convert a possibly annoted binding occurrence of a variable to core.
toCoreBM :: Universe -> S.GXBindVarMT S.SourcePos
         -> ConvertM a (C.Bind C.Name)
toCoreBM uu bb
 = case bb of
        S.XBindVarMT S.BNone     (Just t)
         -> C.BNone <$> toCoreT uu t

        S.XBindVarMT S.BNone     Nothing
         -> C.BNone <$> (return $ C.TVar (C.UName C.NameHole))


        S.XBindVarMT S.BAnon     (Just t)
         -> C.BAnon <$> toCoreT uu t

        S.XBindVarMT S.BAnon     Nothing
         -> C.BAnon <$> (return $ C.TVar (C.UName C.NameHole))


        S.XBindVarMT (S.BName n) (Just t)
         -> C.BName <$> (return $ C.NameVar n)
                    <*> toCoreT uu t

        S.XBindVarMT (S.BName n) Nothing
         -> C.BName <$> (return $ C.NameVar n)
                    <*> (return $ C.TVar (C.UName C.NameHole))


-- Bound ------------------------------------------------------------------------------------------
toCoreU :: S.Bound -> ConvertM a (C.Bound C.Name)
toCoreU uu
 = case uu of
        S.UName n       -> C.UName <$> pure (C.NameVar n)
        S.UIx   i       -> C.UIx   <$> (pure i)
        S.UHole         -> C.UName <$> pure (C.NameHole)


-- Name -------------------------------------------------------------------------------------------
-- | Convert a binding occurrences of a data constructor to a core name.
toCoreDaConBind :: S.DaConBind -> C.Name
toCoreDaConBind (S.DaConBindName tx)
 = C.NameCon tx


-- | Convert a bound occurrence of a data constructor to a core name.
toCoreDaConBound :: S.DaConBound -> C.Name
toCoreDaConBound dcb
 = case dcb of
        S.DaConBoundName tx     -> C.NameCon tx
        S.DaConBoundLit pl      -> toCorePrimLit pl
