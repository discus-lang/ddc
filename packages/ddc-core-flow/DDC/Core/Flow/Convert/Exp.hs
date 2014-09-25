
-- | Conversion of Flow expressions to Tetra expressions
-- This only handles the subset of flow that occurs after lowering.
module DDC.Core.Flow.Convert.Exp
        ( convertX )
where

import DDC.Core.Flow.Convert.Base
import DDC.Core.Flow.Convert.Type
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
import Control.Monad

-- | These operators must just have a region inserted as the first argument
opsToAddRegion :: [(F.Name, T.Name)]
opsToAddRegion
 = [(F.NameOpStore F.OpStoreNew,                T.NameOpStore T.OpStoreAllocRef)
   ,(F.NameOpStore F.OpStoreRead,               T.NameOpStore T.OpStoreReadRef)
   ,(F.NameOpStore F.OpStoreWrite,              T.NameOpStore T.OpStoreWriteRef)
   ]
   -- ,(F.NameOpStore(F.OpStoreWriteVector 1),     T.NameOpStore T.OpStoreWritePtr)
   -- ,(F.NameOpStore(F.OpStoreReadVector  1),     T.NameOpStore T.OpStoreReadPtr)]

convertX :: Exp a F.Name -> ConvertM (Exp a T.Name)
convertX xx
 -- Remove any /\(k : Rate). They are not needed any more.
 | XLAM _ b x <- xx
 , typeOfBind b == F.kRate
 = withRateXLAM b $ removeXLAM b <$> convertX x

 | Just (bs,x) <- takeXLams xx
 = do   bs' <- mapM convertBind bs
        x'  <- convertX x
        return $ xLams anno bs' $ XCast anno CastBox x'

 -- Operators that just need a region added as first argument
 | Just (op, xs) <- takeXPrimApps xx
 , Just  op'     <- lookup op opsToAddRegion
 = do   xs'   <- mapM convertX xs
        return $ run $ mk op' (xRTop anno : xs')

 -- natOfRateNat becomes a noop, as RateNats become Nats.
 | Just (op, [_r, n])   <- takeXPrimApps xx
 , F.NameOpConcrete F.OpConcreteNatOfRateNat
                        <- op
 = convertX n

 -- runKernelN# [ty1]...[tyN] v1...vN proc
 -- becomes
 -- proc (length v1) (ptrOfVec v1) ... (ptrOfVec vN)
 | Just (op, xs)        <- takeXPrimApps xx
 , F.NameOpConcrete (F.OpConcreteRunKernel n)
                        <- op
 , (xts, xs')           <- splitAt n xs
 , Just ts              <- mapM takeXType xts
 , (vs, [proc])         <- splitAt n xs'

 = do   vs'   <- mapM convertX    vs
        ts'   <- mapM convertType ts

        -- smash out any kind lambdas, because they can only be rates
        let proc'
             = case takeXLAMs proc of
               Nothing    -> proc
               Just (_,x) -> x

        proc''<-      convertX    proc'

        case (vs',ts') of
         ((v':_), (t':_))
          -> return
           $ XLet anno (LLet (BNone F.tUnit)
                       ( run
                       $ xApps anno proc''
                         (xVecLen t' v' : zipWith xVecPtr ts' vs')))
             true
         (_, _)
          -> throw $ ErrorNotSupported op

 -- next# [t] [r] series nat
 -- becomes a pointer lookup
 | Just (op, [t, _r, v, i])
                        <- takeXPrimApps xx
 , F.NameOpConcrete (F.OpConcreteNext 1)
                        <- op
 = do   v'      <- convertX v
        i'      <- convertX i
        t'      <- convertX t
        return $ run $ mk (T.NameOpStore T.OpStoreReadPtr)
               [ xRTop anno, t', v', i' ]

 -- vlength# [t] vec
 -- becomes a projection
 | Just (op, [xt, v])   <- takeXPrimApps xx
 , F.NameOpVector F.OpVectorLength
                        <- op
 , Just t               <- takeXType xt
 = do   t'      <- convertType t
        v'      <- convertX    v
        return $ xVecLen t' v'


 -- vwrite# [t] vec ix val
 | Just (op, [xt, vec, ix, val])   <- takeXPrimApps xx
 , F.NameOpStore (F.OpStoreWriteVector 1)
                        <- op
 , Just t               <- takeXType xt
 = do   t'      <- convertType t
        vec'    <- convertX    vec
        ix'     <- convertX    ix
        val'    <- convertX    val

        return $ run 
               $   mk (T.NameOpStore T.OpStoreWritePtr)
                 [ XType anno rTop
                 , XType anno t'
                 , xVecPtr t' vec'
                 , ix'
                 , val' ]


 -- vnew# [t] len
 | Just (op, [xt, sz])   <- takeXPrimApps xx
 , F.NameOpStore F.OpStoreNewVector
                        <- op
 , Just t               <- takeXType xt
 = do   t'      <- convertType t
        sz'     <- convertX    sz

        let args o = [XType anno rTop, XType anno o, sz']
        let buf  = run $ mk (T.NameOpStore T.OpStoreAllocPtr) (args t')
        let sref = run $ mk (T.NameOpStore T.OpStoreAllocRef) (args T.tNat)

        return $ xApps anno (XCon anno T.dcTuple2)
                 [ XType anno $ T.tPtr rTop t'
                 , XType anno $ T.tRef rTop T.tNat
                 , buf
                 , sref ]


 | Just (f, args@(_:_)) <- takeXApps xx
 = do   f'      <- convertX f
        -- filter out any type args that reference deleted XLAMs
        let checkT arg
             | XType _ (TVar (UName n)) <- arg
             = isRateXLAM n
             | otherwise
             = return True
        args'   <-  filterM checkT args
                >>= mapM convertX

        let checkF
             | XVar _ (UName n) <- f
             = isSuspFn n
             | otherwise
             = return False

        isSusp <- checkF
        if   isSusp
        then return $ run $ xApps anno f' args'
        else return $       xApps anno f' args'

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
    -> let bs = valwitBindsOfLets ls
       in  withSuspFns bs $ XLet a <$> convertLets ls <*> convertX x
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

  run  = XCast anno CastRun

  mk n args
   | Just t <- T.takeTypeOfPrimOpName n
   = xApps anno (XVar anno (UPrim n t)) args
   | otherwise
   = error "Impossible"

  true = XVar anno $ UName $ T.NameLitBool True

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


-- TODO these are temporary
xRTop :: a -> Exp a T.Name
xRTop a = XType a rTop

-- | Get the Nat# of length from a Vector
xVecLen :: Type T.Name -> Exp a T.Name -> Exp a T.Name
xVecLen t x
 = XCast anno CastRun
 $ xApps anno (XVar anno $ UPrim (T.NameOpStore T.OpStoreReadRef) (T.typeOpStore T.OpStoreReadRef))
 [ xRTop anno
 , XType anno T.tNat
 , xApps anno (XVar anno $ UName $ T.NameOpStore $ T.OpStoreProj 2 2)
   [ XType anno (T.tPtr rTop t)
   , XType anno (T.tRef rTop T.tNat)
   , x ]
 ]
 where
  anno = annotOfExp x

-- | Get the pointer to the data from a Vector
xVecPtr :: Type T.Name -> Exp a T.Name -> Exp a T.Name
xVecPtr t x
 = xApps anno (XVar anno $ UName $ T.NameOpStore $ T.OpStoreProj 2 1)
 [ XType anno (T.tPtr rTop t)
 , XType anno (T.tRef rTop T.tNat)
 , x ]
 where
  anno = annotOfExp x

