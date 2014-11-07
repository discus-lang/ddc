
-- | Conversion of Flow expressions to Tetra expressions
-- This only handles the subset of flow that occurs after lowering.
module DDC.Core.Flow.Convert.Exp
        ( convertX )
where

import DDC.Core.Flow.Convert.Base
import DDC.Core.Flow.Convert.Type
import DDC.Core.Compounds
import DDC.Core.Exp
-- import DDC.Control.Monad.Check                  (throw)
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
               (XCon anno $ DaConUnit)


    -- natOfRateNat becomes a noop, as RateNats become Nats.
    F.NameOpConcrete F.OpConcreteNatOfRateNat
     | [ _r, n ] <- xs
     -> convertX n

    F.NameOpConcrete (F.OpConcreteNext 1)
     | [t, _r, v, i] <- xs
     -> do  v'      <- convertX v
            i'      <- convertX i
            t'      <- convertX t
            return $ mk (T.PrimStore T.PrimStorePeek)
                     [ xRTop anno, t', v'
                     , mk (T.PrimArith T.PrimArithMul)
                       [ XType anno T.tNat, i'
                       , mk (T.PrimStore T.PrimStoreSize) [t'] ] ]

    -- vlength# [t] vec
    -- becomes a projection
    F.NameOpVector F.OpVectorLength
     | [xt, v] <- xs
     , Just t               <- takeXType xt
     -> do  t'      <- convertType t
            v'      <- convertX    v
            return $ xVecLen t' v'

    -- vwrite# [t] buf ix val
    F.NameOpStore (F.OpStoreWriteVector 1)
     | [xt, buf, ix, val] <- xs
     , Just t             <- takeXType xt
     -> do  t'      <- convertType t
            buf'    <- convertX    buf
            ix'     <- convertX    ix
            val'    <- convertX    val

            return
             $ XLet anno
                    (LLet (BNone T.tVoid)
                    $  mk (T.PrimStore T.PrimStorePoke)
                       [ xRTop anno
                       , XType anno t'
                       , buf'
                       , mk (T.PrimArith T.PrimArithMul)
                         [ XType anno T.tNat, ix'
                         , mk (T.PrimStore T.PrimStoreSize) [XType anno t'] ]
                       , val' ])
               (XCon anno DaConUnit)

    -- vbuf# [t] vec
    F.NameOpStore F.OpStoreBufOfVector
     | [xt, vec]          <- xs
     , Just t             <- takeXType xt
     -> do  t'      <- convertType t
            vec'    <- convertX    vec

            return
             $ xVecPtr t' vec'


    -- vnew# [t] len
    F.NameOpStore F.OpStoreNewVector
     | [xt, sz] <- xs
     , Just t   <- takeXType xt
     -> do  t'      <- convertType t
            sz'     <- convertX    sz

            let lenR = allocRef    anno T.tNat sz'
                datR = allocPtr    anno t'     sz'
                tup  = allocTupleN anno [(tRef rTop T.tNat, lenR), (T.tPtr rTop t', datR)]
            return tup

    -- vtrunc# [t] len vec
    F.NameOpStore F.OpStoreTruncVector
     | [xt, sz, v]  <- xs
     , Just t       <- takeXType xt
     -> do  _t'     <- convertType t
            sz'     <- convertX    sz
            v'      <- convertX    v

            return
             $ XLet anno
                (LLet (BNone T.tVoid)
                    $  mk (T.PrimStore T.PrimStorePoke)
                       [ xRTop anno
                       , XType anno T.tNat
                       , projTuple anno v' 0 (T.tPtr rTop T.tNat)
                       , T.xNat anno 0
                       , sz' ])
               (XCon anno $ DaConUnit)

    F.NameOpSeries F.OpSeriesRunProcess
     | [proc]               <- xs
     -> do  proc'   <- convertX proc
            return
               $ XApp anno proc' $ XCon anno $ DaConUnit

{-
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
               $ XLet anno (LLet (BNone tUnit)
                           ( xApps anno proc'
                             (xVecLen t' v' : zipWith xVecPtr ts' vs')))
                 true
             (_, _)
              -> throw $ ErrorNotSupported op
-}

    _
     -> case takeXApps xx of
         Just (f,args) -> convertApp f args
         Nothing       -> error "Impossible"

 | Just
    (DaConPrim (F.NameDaConFlow (F.DaConFlowTuple n)) _
    , args)                                             <- takeXConApps xx
 , length args == n * 2
 , (xts, xs)            <- splitAt n args
 , Just ts              <- mapM takeXType xts
 = do   ts' <- mapM convertType ts
        xs' <- mapM convertX    xs
        return
         $ allocTupleN anno (ts' `zip` xs')

 | Just (f, args@(_:_)) <- takeXApps xx
 = convertApp f args

 | XCase _ x
    [AAlt (PData (DaConPrim (F.NameDaConFlow (F.DaConFlowTuple n)) _) bs) x1]
                                                        <- xx
 , length bs == n 
 = do   x'  <- convertX x
        bs' <- mapM convertBind bs
        x1' <- convertX x1
        return
         $ xLets anno
            [ LLet b (projTuple anno x' i $ typeOfBind b)
             | (b,i) <- bs' `zip` [0..] ]
           x1'

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

  -- true = T.xBool anno True -- T.xNat anno 1

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
        if    isSusp
         then return $ xApps anno f' args'
         else return $ xApps anno f' args'
 where
  anno = annotOfExp f

convertDaCon :: DaCon F.Name -> ConvertM (DaCon T.Name)
convertDaCon dd
 = case dd of
   DaConUnit
    -> return DaConUnit
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
 , projTuple anno x 0 (T.tPtr rTop T.tNat)
 , T.xNat anno 0 ]
 where
  anno = annotOfExp x

-- | Get the pointer to the data from a Vector
xVecPtr :: Type T.Name -> Exp a T.Name -> Exp a T.Name
xVecPtr t x
 = projTuple anno x 1 (T.tPtr rTop t)
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


unptr :: Type T.Name -> Type T.Name
unptr t
 | Just (_,t') <- T.takeTPtr t
 = t'
 | otherwise
 = t

trybox :: a -> Type T.Name -> Exp a T.Name -> Exp a T.Name
trybox anno t x
 -- Already a pointer, don't bother
 | Just (_,_) <- T.takeTPtr t
 = x
 | otherwise
 = allocRef anno t x

tryunbox :: a -> Type T.Name -> Exp a T.Name -> Exp a T.Name
tryunbox anno t x
 -- Already a pointer, don't bother
 | Just (_,_) <- T.takeTPtr t
 = x
 | otherwise
 = prim anno (T.PrimStore T.PrimStorePeek)
 [ xRTop anno, XType anno t
 , x, T.xNat anno 0 ]

projTuple :: a -> Exp a T.Name -> Integer -> Type T.Name -> Exp a T.Name
projTuple anno x i t
 = let t' = unptr t
 in tryunbox anno t
  $ castPtr  anno t' T.tObj
  $ xApps    anno (XVar anno $ UName $ T.NameVar "getFieldOfBoxed")
    [ xRTop anno, XType anno $ T.tPtr rTop T.tObj, x, T.xNat anno i ]


allocTupleN :: a -> [(Type T.Name, Exp a T.Name)] -> Exp a T.Name
allocTupleN anno txs
 = let tup  = xApps anno (XVar anno $ UName $ T.NameVar "allocBoxed")
              [ xRTop anno, T.xTag anno 0, T.xNat anno (fromIntegral $ length txs) ]

       tup' = XVar anno $ UIx 0
    
       set i t x
        = let t' = unptr t
              x' = trybox anno t x
          in xApps anno (XVar anno $ UName $ T.NameVar "setFieldOfBoxed")
              [ xRTop anno, XType anno (T.tPtr rTop T.tObj), tup', T.xNat anno i
              , castPtr anno T.tObj t' x' ]
                
   in  XLet anno (LLet (BAnon $ T.tPtr rTop T.tObj) tup)
     $ xLets anno
        [LLet (BNone T.tVoid) (set i t x) | ((t,x), i) <- txs `zip` [0..]]
       tup'


castPtr :: a -> Type T.Name -> Type T.Name -> Exp a T.Name -> Exp a T.Name
castPtr anno to from x
 = prim anno (T.PrimStore T.PrimStoreCastPtr)
    [ xRTop anno, XType anno to, XType anno from, x ]

