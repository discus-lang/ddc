{-# OPTIONS_HADDOCK hide #-}
module DDC.Source.Tetra.Convert.Type
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
import DDC.Source.Tetra.Convert.Prim
import DDC.Source.Tetra.Convert.Base
import DDC.Type.Universe                                (Universe (..), universeUp)

import qualified DDC.Source.Tetra.Exp                   as S
import qualified DDC.Source.Tetra.Prim                  as S

import qualified DDC.Core.Tetra.Compounds               as C
import qualified DDC.Core.Tetra.Prim                    as C
import qualified DDC.Type.Sum                           as CSum
import qualified Data.Text                              as Text


-- TypeDef ----------------------------------------------------------------------------------------
toCoreTypeDef
        :: (S.TyConBind, S.Type)
        -> ConvertM a (C.Name, (C.Kind C.Name, C.Type C.Name))

toCoreTypeDef (b, t)
 = do   n       <- toCoreTBCN b
        t'      <- toCoreT UniverseSpec t
        let hole = C.TVar (C.UName C.NameHole)
        return  (n, (hole, t'))


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
                 Nothing        -> error  $ "ddc-soure-tetra.toCoreT: " ++ show tt
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
                S.PrimTypeSoCon sc -> return $ Just $ C.TyConSort    sc
                S.PrimTypeKiCon kc -> return $ Just $ C.TyConKind    kc
                S.PrimTypeTwCon tw -> return $ Just $ C.TyConWitness tw
                S.PrimTypeTcCon ts -> return $ Just $ C.TyConSpec    ts

                -- Primitive TyCons
                S.PrimTypeTyCon tcy
                 -> do  k       <- toCoreT UniverseKind $ S.kindPrimTyCon tcy
                        return  $ Just $ C.TyConBound (C.UPrim (C.NamePrimTyCon tcy) k) k

                S.PrimTypeTyConTetra tct
                 -> do  k       <- toCoreT UniverseKind $ S.kindPrimTyConTetra tct
                        let tct' =  toCoreTyConTetra tct
                        return  $ Just $ C.TyConBound (C.UPrim (C.NameTyConTetra tct') k) k

        -- Bound type constructors.
        --   The embedded kind is set to Bot. We rely on the spreader
        --   to fill in the real kind before type checking.
        S.TyConBound (S.TyConBoundName tx)
         -> return $ Just
         $  C.TyConBound (C.UName (C.NameCon (Text.unpack tx)))
                                  (C.TVar (C.UName C.NameHole))


-- Bind -------------------------------------------------------------------------------------------
-- | Convert a type constructor binding occurrence to a core name.
toCoreTBCN :: S.GTBindCon S.Source  -> ConvertM a C.Name
toCoreTBCN (S.TyConBindName n)
 = return $ C.NameCon (Text.unpack n)


-- | Convert a type constructor bound occurrence to a core name.
toCoreTUCN :: S.GTBoundCon S.Source -> ConvertM a C.Name
toCoreTUCN (S.TyConBoundName n)
 = return $ C.NameCon (Text.unpack n)


-- | Convert a term variable bound occurrence to a core name.
toCoreXUVN :: S.Bound -> ConvertM a C.Name
toCoreXUVN uu
 = case uu of
        S.UName n -> return $ C.NameVar (Text.unpack n)
        S.UIx  _i -> error "ddc-source-tetra.toCoreXBVN: anon bound"
        S.UHole   -> return $ C.NameHole


toCoreXBVN  :: S.GTBindVar S.Source -> ConvertM a C.Name
toCoreXBVN bb
 = case bb of
        S.BNone   -> error "ddc-source-tetra.toCoreXBVN: none bound"
        S.BAnon   -> error "ddc-source-tetra.toCoreXBVN: anon bound"
        S.BName n -> return $ C.NameVar (Text.unpack n)


-- | Convert a type binder and kind to core.
toCoreTBK :: (S.GTBindVar S.Source, S.GType S.Source)
          -> ConvertM a (C.Bind C.Name)
toCoreTBK (bb, k)
 = case bb of
        S.BNone   -> C.BNone <$> (toCoreT UniverseKind k)
        S.BAnon   -> C.BAnon <$> (toCoreT UniverseKind k)
        S.BName n -> C.BName <$> (return $ C.NameVar (Text.unpack n))
                             <*> (toCoreT UniverseKind k)


-- | Convert an unannoted binder to core.
toCoreB  :: S.Bind -> ConvertM a (C.Bind C.Name)
toCoreB bb
 = let hole     = C.TVar (C.UName C.NameHole)
   in case bb of
        S.BNone   -> return $ C.BNone hole
        S.BAnon   -> return $ C.BAnon hole
        S.BName n -> return $ C.BName (C.NameVar (Text.unpack n)) hole


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
         S.PVar (S.BName n) -> return $ C.BName (C.NameVar (Text.unpack n)) t'
         _                  -> error "ddc-source-tetra.toCorePT: bad pattern"


toCoreBT :: Universe -> S.Bind -> Maybe S.Type -> ConvertM a (C.Bind C.Name)
toCoreBT uu b mt
 = do   t' <- case mt of
               Nothing -> return $ C.TVar (C.UName C.NameHole)
               Just t  -> toCoreT uu t

        case b of
         S.BNone        -> return $ C.BNone t'
         S.BAnon        -> return $ C.BAnon t'
         S.BName n      -> return $ C.BName (C.NameVar (Text.unpack n)) t'


-- | Convert a possibly annoted binding occurrence of a variable to core.
toCoreBM :: Universe -> S.GXBindVarMT S.Source -> ConvertM a (C.Bind C.Name)
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
         -> C.BName <$> (return $ C.NameVar (Text.unpack n))
                    <*> toCoreT uu t

        S.XBindVarMT (S.BName n) Nothing
         -> C.BName <$> (return $ C.NameVar (Text.unpack n))
                    <*> (return $ C.TVar (C.UName C.NameHole))


-- Bound ------------------------------------------------------------------------------------------
toCoreU :: S.Bound -> ConvertM a (C.Bound C.Name)
toCoreU uu
 = case uu of
        S.UName n       -> C.UName <$> pure (C.NameVar (Text.unpack n))
        S.UIx   i       -> C.UIx   <$> (pure i)
        S.UHole         -> C.UName <$> pure (C.NameHole)


-- Name -------------------------------------------------------------------------------------------
-- | Convert a binding occurrences of a data constructor to a core name.
toCoreDaConBind :: S.DaConBind -> C.Name
toCoreDaConBind (S.DaConBindName tx)
 = C.NameCon (Text.unpack tx)


-- | Convert a bound occurrence of a data constructor to a core name.
toCoreDaConBound :: S.DaConBound -> C.Name
toCoreDaConBound dcb
 = case dcb of
        S.DaConBoundName tx
         -> C.NameCon (Text.unpack tx)

        S.DaConBoundLit pl
         -> toCorePrimLit pl



