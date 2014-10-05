
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

import qualified DDC.Core.Salt.Name            as T
import qualified DDC.Core.Salt.Compounds       as T
import qualified DDC.Core.Salt.Env             as T

import Control.Applicative
import Control.Monad


convertX :: Exp a F.Name -> ConvertM (Exp a T.Name)
convertX xx
 -- Remove any /\(k : Rate). They are not needed any more.
 | XLAM _ b x <- xx
 , typeOfBind b == F.kRate
 = withRateXLAM b $ removeXLAM b <$> convertX x

 -- Operators that just need a region added as first argument
 | Just (op, xs@(_:_)) <- takeXPrimApps xx
 = case op of
    F.NameOpStore F.OpStoreNew
     | [ ty, val ] <- xs
     , Just tY     <- takeXType ty
     -> do  tY'    <- convertType tY
            val'   <- convertX val
            return $ allocRef anno tY' val'

    F.NameOpStore F.OpStoreRead
     | [ ty, ref ] <- xs
     -> do  ty'  <- convertX ty
            ref' <- convertX ref

            return $ mk (T.PrimStore T.PrimStorePeek)
                     [ xRTop anno, ty', ref', T.xNat anno 0 ]

    F.NameOpStore F.OpStoreWrite
     | [ ty, ref, val ] <- xs
     -> do  ty'  <- convertX ty
            ref' <- convertX ref
            val' <- convertX val

            return
             $ XLet anno
                    (LLet (BNone T.tVoid)
                    $  mk (T.PrimStore T.PrimStorePoke)
                       [ xRTop anno, ty', ref', T.xNat anno 0, val' ])
               (XCon anno DaConUnit)


    -- natOfRateNat becomes a noop, as RateNats become Nats.
    F.NameOpConcrete F.OpConcreteNatOfRateNat
     | [ _r, n ] <- xs
     -> convertX n

    -- runKernelN# [ty1]...[tyN] v1...vN proc
    -- becomes
    -- proc (length v1) (ptrOfVec v1) ... (ptrOfVec vN)
    F.NameOpConcrete (F.OpConcreteRunKernel n)
     | (xts, xs')           <- splitAt n xs
     , Just ts              <- mapM takeXType xts
     , (vs, [proc])         <- splitAt n xs'
     -> do  vs'   <- mapM convertX    vs
            ts'   <- mapM convertType ts

            proc' <-      convertX    proc

            case (vs',ts') of
             ((v':_), (t':_))
              -> return
               $ XLet anno (LLet (BNone F.tUnit)
                           ( xApps anno proc'
                             (xVecLen t' v' : zipWith xVecPtr ts' vs')))
                 true
             (_, _)
              -> throw $ ErrorNotSupported op

    F.NameOpConcrete (F.OpConcreteNext 1)
     | [t, _r, v, i] <- xs
     -> do  v'      <- convertX v
            i'      <- convertX i
            t'      <- convertX t
            return $ mk (T.PrimStore T.PrimStorePeek)
                     [ xRTop anno, t', v', i' ]

    -- vlength# [t] vec
    -- becomes a projection
    F.NameOpVector F.OpVectorLength
     | [xt, v] <- xs
     , Just t               <- takeXType xt
     -> do  t'      <- convertType t
            v'      <- convertX    v
            return $ xVecLen t' v'

    -- vwrite# [t] vec ix val
    F.NameOpStore (F.OpStoreWriteVector 1)
     | [xt, vec, ix, val] <- xs
     , Just t             <- takeXType xt
     -> do  t'      <- convertType t
            vec'    <- convertX    vec
            ix'     <- convertX    ix
            val'    <- convertX    val

            return
             $ XLet anno
                    (LLet (BNone T.tVoid)
                    $  mk (T.PrimStore T.PrimStorePoke)
                       [ xRTop anno
                       , XType anno t'
                       , xVecPtr t' vec'
                       , ix'
                       , val' ])
               (XCon anno DaConUnit)


    -- vnew# [t] len
    F.NameOpStore F.OpStoreNewVector
     | [xt, sz] <- xs
     , Just t   <- takeXType xt
     -> do  t'      <- convertType t
            sz'     <- convertX    sz

            let lenR = allocRef anno T.tNat sz'
                datR = allocPtr anno t'     sz'
                tup  = allocTuple2 anno (T.tPtr rTop T.tNat) (T.tPtr rTop t') lenR datR
            return tup

    _
     -> case takeXApps xx of
         Just (f,args) -> convertApp f args
         Nothing       -> error "Impossible"


 | Just (f, args@(_:_)) <- takeXApps xx
 = convertApp f args

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

  mk = prim anno

  true = XVar anno $ UName $ T.NameLitBool True

prim anno n args
 = let t = T.typeOfPrim n
   in      xApps anno (XVar anno (UPrim (T.NamePrimOp n) t)) args


convertApp :: Exp a F.Name -> [Exp a F.Name] -> ConvertM (Exp a T.Name)
convertApp f args
 = do   f'      <- convertX f
        -- filter out any type args that reference deleted XLAMs
        let checkT arg
             | XType _ (TVar (UName n)) <- arg
             = not <$> isRateXLAM n
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
        then return $ xApps anno f' args'
        else return $ xApps anno f' args'
 where
  anno = annotOfExp f

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
xVecLen _t x
 = prim anno (T.PrimStore T.PrimStorePeek)
 [ xRTop anno, XType anno T.tNat
 , castPtr anno T.tNat T.tObj
 $ xApps anno (XVar anno $ UName $ T.NameVar "getFieldOfBoxed")
   [ xRTop anno, XType anno $ T.tPtr rTop T.tObj, x, T.xNat anno 0 ]
 , T.xNat anno 0 ]
 where
  anno = annotOfExp x

-- | Get the pointer to the data from a Vector
xVecPtr :: Type T.Name -> Exp a T.Name -> Exp a T.Name
xVecPtr t x
 = castPtr anno t T.tObj
 $ xApps anno (XVar anno $ UName $ T.NameVar "getFieldOfBoxed")
   [ xRTop anno, XType anno $ T.tPtr rTop T.tObj, x, T.xNat anno 1 ]
 where
  anno = annotOfExp x

allocRef :: a -> Type T.Name -> Exp a T.Name -> Exp a T.Name
allocRef anno tY val
 = let ty  = XType anno tY

       sz   = prim anno (T.PrimStore T.PrimStoreSize)  [ty]
       addr = prim anno (T.PrimStore T.PrimStoreAlloc) [sz]
       ptr  = prim anno (T.PrimStore T.PrimStoreMakePtr)
                 [ xRTop anno, ty, addr ]
                
       ll   = LLet (BAnon $ T.tPtr rTop tY)
                   ptr

       ptr' = XVar anno $ UIx 0
       poke = prim anno (T.PrimStore T.PrimStorePoke)
               [ xRTop anno, ty, ptr', T.xNat anno 0, val ]
   in  XLet anno ll
     $ XLet anno (LLet (BNone T.tVoid) poke) 
       ptr'

allocPtr :: a -> Type T.Name -> Exp a T.Name -> Exp a T.Name
allocPtr anno tY elts
 = let ty  = XType anno tY

       sz   = prim anno (T.PrimStore T.PrimStoreSize)  [ty]
       addr = prim anno (T.PrimStore T.PrimStoreAlloc)
                 [ prim anno (T.PrimArith T.PrimArithMul)
                    [ XType anno T.tNat, elts, sz] ]
       ptr  = prim anno (T.PrimStore T.PrimStoreMakePtr)
                 [ xRTop anno, ty, addr ]
                
   in  ptr

allocTuple2 :: a -> Type T.Name -> Type T.Name -> Exp a T.Name -> Exp a T.Name -> Exp a T.Name
allocTuple2 anno ta tb a b
 = let tup  = xApps anno (XVar anno $ UName $ T.NameVar "allocBoxed")
              [ xRTop anno, T.xTag anno 0, T.xNat anno 2 ]

       tup' = XVar anno $ UIx 0
    
       setA = xApps anno (XVar anno $ UName $ T.NameVar "setFieldOfBoxed")
              [ xRTop anno, XType anno T.tObj, tup', T.xNat anno 0
              , castPtr anno T.tObj ta a ]

       setB = xApps anno (XVar anno $ UName $ T.NameVar "setFieldOfBoxed")
              [ xRTop anno, XType anno T.tObj, tup', T.xNat anno 1
              , castPtr anno T.tObj tb b ]
                
   in  XLet anno (LLet (BAnon $ T.tPtr rTop T.tObj) tup)
     $ XLet anno (LLet (BNone T.tVoid) setA)
     $ XLet anno (LLet (BNone T.tVoid) setB)
       tup'


castPtr :: a -> Type T.Name -> Type T.Name -> Exp a T.Name -> Exp a T.Name
castPtr anno to from x
 = prim anno (T.PrimStore T.PrimStoreCastPtr)
    [ xRTop anno, XType anno to, XType anno from, x ]

