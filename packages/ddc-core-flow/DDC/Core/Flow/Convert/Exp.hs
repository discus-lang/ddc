
-- | Conversion of Flow expressions to Tetra expressions
-- This only handles the subset of flow that occurs after lowering.
module DDC.Core.Flow.Convert.Exp
        ( convertX )
where

import DDC.Core.Flow.Convert.Base
import DDC.Core.Flow.Convert.Type
import DDC.Core.Compounds
import DDC.Core.Exp
import DDC.Type.Transform.LiftT

import qualified DDC.Core.Flow.Prim             as F
import qualified DDC.Core.Flow.Compounds        as F

import qualified DDC.Core.Tetra.Prim            as T

import Control.Applicative

convertX :: Exp a F.Name -> ConvertM (Exp a T.Name)
convertX xx
 | XLAM _ b x <- xx
 , typeOfBind b == F.kRate
 = removeXLAM b <$> convertX x 

 | Just (F.NameOpStore F.OpStoreNew, [xT, xA])  <- takeXPrimApps xx
 = do   xT' <- convertX xT
        xA' <- convertX xA
        return $ mk (T.NameOpStore T.OpStoreAllocRef)
                [xTop anno, xT', xA']

 -- TODO other store etc

 -- otherwise just boilerplate recursion
 | otherwise
 = case xx of
   XVar a b
    -> XVar a <$> convertBound b
   XCon a c
    -> XCon a <$> convertDaCon c
   XLAM a b x
    -> XLAM a <$> convertBind  b <*> convertX x
   XLam a b x
    -> XLam a <$> convertBind  b <*> convertX x
   XApp a p q
    -> XApp a <$> convertX     p <*> convertX q
   XLet a ls x
    -> XLet a <$> convertLets ls <*> convertX x
   XCase a x as
    -> XCase a<$> convertX     x <*> mapM convertAlt as
   XCast a c x
    -> XCast a<$> convertCast  c <*> convertX x
   XType a t
    -> XType a<$> convertType  t
   XWitness a w
    -> XWitness a <$> convertWit w
 where
  anno = annotOfExp xx

  mk n args
   | Just t <- T.takeTypeOfPrimOpName n
   = xApps anno (XVar anno (UPrim n t)) args
   | otherwise
   = error "Impossible"

convertDaCon :: DaCon F.Name -> ConvertM (DaCon T.Name)
convertDaCon dd
 = case dd of
   DaConUnit
    -> return $ DaConUnit
   DaConPrim n t
    -> DaConPrim  <$> convertName n <*> convertType t
   DaConBound n
    -> DaConBound <$> convertName n

convertLets :: Lets a F.Name -> ConvertM (Lets a T.Name)
convertLets ll
 = case ll of
   LLet b x
    -> LLet <$> convertBind b <*> convertX x
   LRec bxs
    -> LRec <$> mapM (both convertBind convertX) bxs
   LPrivate rs t ws
    -> LPrivate <$> mapM convertBind rs
                <*> liftMaybe convertType t -- ??
                <*> mapM convertBind ws
   -- This won't show up in source programs, but doesn't hurt to deal with it
   LWithRegion b
    -> LWithRegion <$> convertBound b

 where
  liftMaybe f m
   = case m of
     Just a  -> Just <$> f a
     Nothing -> return Nothing

  both f g
   = \(a,b) -> (,) <$> f a <*> g b

convertAlt  :: Alt a F.Name -> ConvertM (Alt a T.Name)
convertAlt aa
 = case aa of
   AAlt p x
    -> AAlt <$> convertPat p <*> convertX x

convertPat :: Pat F.Name -> ConvertM (Pat T.Name)
convertPat pp
 = case pp of
   PDefault
    -> return $ PDefault
   PData dc bs
    -> PData <$> convertDaCon dc <*> mapM convertBind bs

convertCast :: Cast a F.Name -> ConvertM (Cast a T.Name)
convertCast cc
 = case cc of
   CastWeakenEffect et
    -> CastWeakenEffect  <$> convertType et
   CastWeakenClosure cs
    -> CastWeakenClosure <$> mapM convertX cs
   CastPurify w
    -> CastPurify        <$> convertWit w
   CastForget w
    -> CastForget        <$> convertWit w
   CastBox
    -> return $ CastBox
   CastRun
    -> return $ CastRun


convertWit :: Witness a F.Name -> ConvertM (Witness a T.Name)
convertWit _ww
 = error "fix me"
 {-
 = case ww of
   WVar a b
    -> WVar a <$> convertBound b
   WCon a wc
    -> convertWitCon
   WApp a wp wq
    -> WApp  a <$> convertWit wp <*> convertWit wq
   WJoin a wp wq
    -> WJoin a <$> convertWit wp <*> convertWit wq
   WType a t
    -> WType a <$> convertType t
-}

-- | When replacing @/\(b : Rate). x@ with @x@, if @b@ is a de bruijn index then any type vars in @x@ must be lowered.
-- @b@ must not be mentioned in @x@.
removeXLAM :: Bind F.Name -> Exp a T.Name -> Exp a T.Name
removeXLAM b t
 = case b of
   BAnon _
    -> lowerT 1 t
   _
    ->          t


xTop :: a -> Exp a T.Name
xTop a = XType a rTop

