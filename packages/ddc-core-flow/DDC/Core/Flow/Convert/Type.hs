
-- | Conversion of Flow types to Tetra types
-- This only handles the subset of flow that occurs after lowering.
module DDC.Core.Flow.Convert.Type
        ( convertType
        , convertBind
        , convertBound
        , convertName
        , rTop )
where

import DDC.Base.Pretty (ppr)

import DDC.Core.Flow.Convert.Base
import DDC.Core.Compounds
import DDC.Core.Exp
import DDC.Control.Monad.Check                  (throw)
import DDC.Type.Transform.LiftT

import qualified DDC.Core.Flow.Prim             as F
import qualified DDC.Core.Flow.Compounds        as F

import qualified DDC.Core.Tetra.Prim            as T
import qualified DDC.Core.Tetra.Prim.TyConPrim  as T
import qualified DDC.Core.Tetra.Prim.TyConTetra as T

import Control.Applicative


-- | Convert types from Flow to Tetra.
--
-- The majority of type conversions are just replacing one name with another,
-- so these are performed in @convertName@.
--
-- Others require removing arguments or adding regions are performed here, before name conversion:
-- * Rate foralls are removed
-- * @Series k a@ becomes @Ptr# rTop a@
-- * @RateNat  k@ becomes @Nat#@
-- * @Ref a@      becomes @Ref# rTop a@
-- * @a->b->c@    becomes @a -> b -> S (Read rT + Write rT + Alloc rT) c@
--
convertType :: Type F.Name -> ConvertM (Type T.Name)
convertType tt
 -- Remove [k : Rate] foralls.
 | TForall b t  <- tt
 , typeOfBind b == F.kRate
 = removeForall b <$> convertType t

 -- Convert @Vector a@ to just @Tuple2# (Ptr# a) (Ref# Nat#)@
 | Just (F.NameTyConFlow F.TyConFlowVector, [tA])   <- takePrimTyConApps tt
 = do   tA' <- convertType tA
        return $ T.tTupleN [T.tPtr rTop tA', T.tRef rTop T.tNat]

 -- Convert @Series k a@ to just @Ptr# a@
 | Just (F.NameTyConFlow F.TyConFlowSeries, [_K, tA])   <- takePrimTyConApps tt
 = T.tPtr rTop <$> convertType tA

 -- Convert @RateNat  k@ to @Nat#@
 | Just (F.NameTyConFlow F.TyConFlowRateNat, [_K])      <- takePrimTyConApps tt
 = return  $  T.tNat

 -- Convert Refs
 | Just (F.NameTyConFlow F.TyConFlowRef, [tA])          <- takePrimTyConApps tt
 = T.tRef rTop <$> convertType tA

 -- Add effects to the last part of an arrow
 | (args@(_:_), res)                                    <- takeTFunArgResult tt
 = do   args'   <- mapM convertType args
        res'    <-      convertType res

        let eff  = tSum kEffect [tRead rTop, tWrite rTop, tAlloc rTop]

        return   $ foldr tFun (tSusp eff res') args'
        

 -- For other primitives, convertName will handle convert them
 | otherwise
 = case tt of
    TVar b
     -> TVar    <$> convertBound b
    TCon c
     -> TCon    <$> convertTyCon c

    TForall b t
     -> TForall <$> convertBind  b <*> convertType t

    TApp p q
     -> TApp    <$> convertType  p <*> convertType q

    TSum _ts
     -> throw    $ ErrorUnexpectedSum


convertBind :: Bind F.Name -> ConvertM (Bind T.Name)
convertBind b
 = case b of
   BNone   t -> BNone <$> convertType t
   BAnon   t -> BAnon <$> convertType t
   BName n t -> BName <$> convertName n <*> convertType t


convertBound :: Bound F.Name -> ConvertM (Bound T.Name)
convertBound b
 = case b of
   UIx     i -> return $  UIx i
   UName n   -> UName <$> convertName n
   UPrim n t -> UPrim <$> convertName n <*> convertType t




convertName :: F.Name -> ConvertM T.Name
convertName nn
 = case nn of
   F.NameVar n
    -> return $ T.NameVar n
   F.NameVarMod n x
    -> flip T.NameExt x <$> convertName n
   F.NameCon n
    -> return $ T.NameCon n

   F.NameKiConFlow _
    -> throw $ ErrorPartialPrimitive nn

   F.NameTyConFlow tf
    -> case tf of
        F.TyConFlowTuple n
         -> return $ T.NameTyConTetra $ T.TyConTetraTuple n

        -- Vector, Series, RateNat and Ref are handled elsewhere as arguments must be changed
        _
         -> throw $ ErrorPartialPrimitive nn

   F.NameDaConFlow (F.DaConFlowTuple n)
    -> return $ T.NameDaConTetra $ T.DaConTetraTuple n

   -- Machine primitives ------------------
   F.NamePrimTyCon p
    -> return $ T.NamePrimTyCon p

   F.NamePrimArith p
    -> return $ T.NamePrimArith p

   F.NamePrimCast p
    -> return $ T.NamePrimCast p

   -- Literals -----------------------------
   F.NameLitBool l
    -> return $ T.NameLitBool l
   F.NameLitNat l
    -> return $ T.NameLitNat  l
   F.NameLitInt l
    -> return $ T.NameLitInt l
   F.NameLitWord l k
    -> return $ T.NameLitWord l k

   _
    -> return $ T.NameExt (T.NameVar $ show $ ppr $ nn) "UNHANDLED"
    -- This should be a throw:
    -- > throw  $ ErrorInvalidBinder nn
    -- but for debugging, just splatting it out is easier


convertTyCon :: TyCon F.Name -> ConvertM (TyCon T.Name)
convertTyCon tc
 = case tc of
   TyConSort s
    -> return $ TyConSort s
   TyConKind k
    -> return $ TyConKind k
   TyConWitness w
    -> return $ TyConWitness w
   TyConSpec s
    -> return $ TyConSpec s
   TyConBound b k
    -> TyConBound <$> convertBound b <*> convertType k
   TyConExists i k
    -> TyConExists    i              <$> convertType k


-- | When replacing @Forall b t@ with @t@, if @b@ is a de bruijn index then @t@ must be lowered.
-- @b@ must not be mentioned in @t@.
removeForall :: Bind F.Name -> Type T.Name -> Type T.Name
removeForall b t
 = case b of
   BAnon _
    -> lowerT 1 t
   _
    ->          t


-- | Top region
-- TODO: this needs to be fixed. See DDC.Core.Salt.Runtime
rTop :: Type T.Name
rTop = TVar $ UName $ T.NameVar "rT"

