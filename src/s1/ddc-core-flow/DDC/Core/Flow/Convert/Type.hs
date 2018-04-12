
-- | Conversion of Flow types to Tetra types
-- This only handles the subset of flow that occurs after lowering.
module DDC.Core.Flow.Convert.Type
        ( convertType
        , convertBind
        , convertBound
        , convertName
        , rTop
        , tVec
        , tRef )
where
import DDC.Core.Flow.Convert.Base
import DDC.Core.Exp.Annot
import DDC.Control.Check                        (throw)
import DDC.Type.Transform.BoundT

import qualified DDC.Core.Flow.Prim             as F
import qualified DDC.Core.Flow.Compounds        as F

import qualified DDC.Core.Salt.Name             as T
import qualified DDC.Core.Salt.Compounds        as T
import qualified Data.Text                      as T


tRef   :: Type T.Name -> Type T.Name -> Type T.Name
tRef = T.tPtr

tVec :: Type T.Name
tVec = T.tPtr rTop T.tObj


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

 -- Convert @Vector a@ to @Tuple2# (Ptr# a) (Ref# Nat#)@
 | Just (F.NameTyConFlow F.TyConFlowVector, [tA])   <- takePrimTyConApps tt
 = do   _tA' <- convertType tA
        return $ tVec -- T.tTupleN [T.tPtr rTop tA', T.tRef rTop T.tNat]

 -- Convert @Buffer a@ to @Ptr# a@
 | Just (F.NameTyConFlow F.TyConFlowBuffer, [tA])   <- takePrimTyConApps tt
 = do   tA' <- convertType tA
        return $ T.tPtr rTop tA'

 -- Convert @TupleN#@ to @Ptr# rTop Obj@
 | Just (F.NameTyConFlow (F.TyConFlowTuple _), ts)   <- takePrimTyConApps tt
 = do   -- Might as well attempt to convert the types, just so we know they're valid
        mapM_ convertType ts
        return $ tVec

 -- Convert @Series k a@ to just @Ptr# a@
 | Just (F.NameTyConFlow F.TyConFlowSeries, [_K, tA])   <- takePrimTyConApps tt
 = T.tPtr rTop <$> convertType tA

 -- Convert @RateNat  k@ to @Nat#@
 | Just (F.NameTyConFlow F.TyConFlowRateNat, [_K])      <- takePrimTyConApps tt
 = return  $  T.tNat

 -- Convert Refs
 | Just (F.NameTyConFlow F.TyConFlowRef, [tA])          <- takePrimTyConApps tt
 = tRef rTop <$> convertType tA

 -- Convert normal TFuns to TFunECs with pure and empty. why?
 | (args@(_:_), res)                                    <- takeTFunArgResult tt
 = do   args'   <- mapM convertType args
        res'    <-      convertType res

        return   $ foldr tFun res' args'


 -- For other primitives, convertName will handle convert them
 | otherwise
 = case tt of
    TVar b
     -> TVar    <$> convertBound b
    TCon c
     -> TCon    <$> convertTyCon c

    TAbs b t
     -> TAbs    <$> convertBind b <*> convertType t

    TApp p q
     -> TApp    <$> convertType p <*> convertType q

    TForall b t
     -> TForall <$> convertBind b <*> convertType t

    TSum _t
     -> return $ TSum $ TypeSumBot $ kData -- throw    $ ErrorUnexpectedSum


convertBind :: Bind F.Name -> ConvertM (Bind T.Name)
convertBind b
 = case b of
   BNone   t -> BNone <$> convertType t
   BAnon   t -> BAnon <$> convertType t
   BName n t -> BName <$> convertName n <*> convertType t


convertBound :: Bound F.Name -> ConvertM (Bound T.Name)
convertBound b
 = case b of
   UIx   i -> return $  UIx i
   UName n -> UName <$> convertName n
   UPrim n -> UPrim <$> convertName n




convertName :: F.Name -> ConvertM T.Name
convertName nn
 = case nn of
   F.NameVar n
    -> return $ T.NameVar (T.pack n)

   F.NameVarMod n x
    -> flip T.NameExt (T.pack x) <$> convertName n

   F.NameCon n
    -> return $ T.NameCon (T.pack n)

   F.NameKiConFlow _
    -> throw $ ErrorPartialPrimitive nn

   F.NameTyConFlow tf
    -> case tf of
        -- F.TyConFlowTuple n
        -- -> return $ T.NameTyConTetra $ T.TyConTetraTuple n

        -- Vector, Series, RateNat and Ref are handled elsewhere as arguments must be changed
        _
         -> throw $ ErrorPartialPrimitive nn

   -- Machine primitives ------------------
   -- F.NamePrimTyCon T.PrimTyConBool
   --  -> return $ T.NamePrimTyCon T.PrimTyConNat

   F.NamePrimTyCon p
    -> return $ T.NamePrimTyCon p

   F.NamePrimArith p
    -> return $ T.NamePrimOp $ T.PrimArith p

   F.NamePrimCast p
    -> return $ T.NamePrimOp $ T.PrimCast p

   -- Literals -----------------------------
   F.NameLitBool b
    -> return $ T.NamePrimLit (T.PrimLitBool b)

   F.NameLitNat l
    -> return $ T.NamePrimLit (T.PrimLitNat  l)

   F.NameLitInt l
    -> return $ T.NamePrimLit (T.PrimLitInt l)

   F.NameLitWord l k
    -> return $ T.NamePrimLit (T.PrimLitWord l k)

   _ -> throw  $ ErrorInvalidBinder nn


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


-- | When replacing @Forall b t@ with @t@, if @b@ is a de bruijn
--   index then @t@ must be lowered. @b@ must not be mentioned in @t@.
removeForall :: Bind F.Name -> Type T.Name -> Type T.Name
removeForall b t
 = case b of
   BAnon _
    -> lowerT 1 t
   _
    ->          t


-- | Top region
rTop :: Type T.Name
rTop = TVar $ UName $ T.NameVar "rT"

