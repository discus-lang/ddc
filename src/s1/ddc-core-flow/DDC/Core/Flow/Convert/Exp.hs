
-- | Conversion of Flow expressions to Tetra expressions
-- This only handles the subset of flow that occurs after lowering.
module DDC.Core.Flow.Convert.Exp
        ( convertX )
where

import DDC.Core.Flow.Convert.Base
import DDC.Core.Flow.Convert.Type
import DDC.Core.Exp.Annot
import DDC.Type.Transform.BoundT

import qualified DDC.Core.Flow.Prim             as F
import qualified DDC.Core.Flow.Compounds        as F

import qualified DDC.Core.Salt.Name            as T
import qualified DDC.Core.Salt.Compounds       as T

import Control.Monad


---------------------------------------------------------------------------------------------------
convertX :: Exp a F.Name -> ConvertM (Exp a T.Name)
convertX xx
 -- Remove any /\(k : Rate). They are not needed any more.
 | XLAM _ b x <- xx
 , typeOfBind b == F.kRate
 = withRateXLAM b $ removeXLAM b <$> convertX x

 -- Operators that just need a region added as first argument
 | Just (op, xs@(_:_)) <- takeXNameApps xx
 = case op of

    F.NameOpStore F.OpStoreNew
     | [ RType tY, RTerm val ] <- xs
     -> do  tY'    <- convertType tY
            val'   <- convertX    val
            return $ allocRef anno tY' val'


    F.NameOpStore F.OpStoreRead
     | [ RType ty, RTerm ref ] <- xs
     -> do  ty'  <- convertType ty
            ref' <- convertX    ref

            return $ mk (T.PrimStore T.PrimStorePeek)
                [ RType rTop, RType ty'
                , RTerm ref', RTerm $ T.xNat anno 0 ]


    F.NameOpStore F.OpStoreWrite
     | [ RType ty, RTerm ref, RTerm val ] <- xs
     -> do  ty'  <- convertType ty
            ref' <- convertX    ref
            val' <- convertX    val

            return
             $ XLet anno
                    (LLet (BNone T.tVoid)
                    $ mk (T.PrimStore T.PrimStorePoke)
                       [ RType rTop, RType ty'
                       , RTerm ref', RTerm $ T.xNat anno 0, RTerm val' ])
               (XCon anno $ DaConUnit)


    -- natOfRateNat becomes a noop, as RateNats become Nats.
    F.NameOpConcrete F.OpConcreteNatOfRateNat
     | [RType _r, RTerm n ] <- xs
     -> convertX n


    F.NameOpConcrete (F.OpConcreteNext 1)
     | [RType t, RType _r, RTerm v, RTerm i] <- xs
     -> do  v'      <- convertX    v
            i'      <- convertX    i
            t'      <- convertType t
            return $ mk (T.PrimStore T.PrimStorePeek)
                     [ RType rTop
                     , RType t'
                     , RTerm v'
                     , RTerm $ mk (T.PrimArith T.PrimArithMul)
                                [ RType T.tNat
                                , RTerm i'
                                , RTerm $ mk (T.PrimStore T.PrimStoreSize) [RType t'] ] ]


    -- vlength# [t] vec
    -- becomes a projection
    F.NameOpVector F.OpVectorLength
     | [RType t, RTerm v] <- xs
     -> do  t'      <- convertType t
            v'      <- convertX    v
            return $ xVecLen t' v'


    -- vwrite# [t] buf ix val
    F.NameOpStore (F.OpStoreWriteVector 1)
     | [RType t, RTerm buf, RTerm ix, RTerm val] <- xs
     -> do  t'      <- convertType t
            buf'    <- convertX    buf
            ix'     <- convertX    ix
            val'    <- convertX    val

            return
             $ XLet anno
                    (LLet (BNone T.tVoid)
                    $  mk (T.PrimStore T.PrimStorePoke)
                       [ RType rTop
                       , RType t'
                       , RTerm buf'
                       , RTerm $ mk (T.PrimArith T.PrimArithMul)
                                  [ RType T.tNat
                                  , RTerm ix'
                                  , RTerm $ mk (T.PrimStore T.PrimStoreSize) [RType t'] ]
                       , RTerm val' ])
               (XCon anno DaConUnit)


    -- vbuf# [t] vec
    F.NameOpStore F.OpStoreBufOfVector
     | [RType t, RTerm vec] <- xs
     -> do  t'      <- convertType t
            vec'    <- convertX    vec
            return
             $ xVecPtr t' vec'


    -- vnew# [t] len
    F.NameOpStore F.OpStoreNewVector
     | [RType t, RTerm sz] <- xs
     -> do  t'      <- convertType t
            sz'     <- convertX    sz

            let lenR = allocRef    anno T.tNat sz'
                datR = allocPtr    anno t'     sz'
                tup  = allocTupleN anno [(tRef rTop T.tNat, lenR), (T.tPtr rTop t', datR)]
            return tup


    -- vtrunc# [t] len vec
    F.NameOpStore F.OpStoreTruncVector
     | [RType t, RTerm sz, RTerm v]  <- xs
     -> do  _t'     <- convertType t
            sz'     <- convertX    sz
            v'      <- convertX    v

            return
             $ XLet anno
                (LLet (BNone T.tVoid)
                    $  mk (T.PrimStore T.PrimStorePoke)
                       [ RType rTop
                       , RType T.tNat
                       , RTerm $ projTuple anno v' 0 (T.tPtr rTop T.tNat)
                       , RTerm $ T.xNat anno 0
                       , RTerm $ sz' ])
               (XCon anno $ DaConUnit)

    F.NameOpSeries F.OpSeriesRunProcess
     | [RTerm proc] <- xs
     -> do  proc'   <- convertX proc
            return  $  XApp anno proc' (RTerm (XCon anno $ DaConUnit))

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
         Nothing       -> error "ddc-core-flow.convertX: impossible!"

 | Just
    (DaConPrim (F.NameDaConFlow (F.DaConFlowTuple n))
    , args)     <- takeXConApps xx
 , length args == n * 2
 , (xts, as)            <- splitAt n args
 , Just ts              <- mapM takeRType xts
 = do   ts' <- mapM convertType ts
        as' <- mapM convertArg  as
        return
         $ allocTupleN anno (ts' `zip` [x | RTerm x <- as'])

 | Just (f, args@(_:_)) <- takeXApps xx
 = convertApp f args

 | XCase _ x
    [AAlt (PData (DaConPrim (F.NameDaConFlow (F.DaConFlowTuple n))) bs) x1]
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
   XVar a b     -> XVar a  <$> convertBound b
   XCon a c     -> XCon a  <$> convertDaCon c
   XAtom{}      -> error "ddc-core-flow.convertX: impossible"

   XAbs a (MType b) x
    -> XAbs a  <$> (fmap MType     $ convertBind  b) <*> convertX x

   XAbs a (MTerm b) x
    -> XAbs a  <$> (fmap MTerm     $ convertBind  b) <*> convertX x

   XAbs a (MImplicit b) x
    -> XAbs a  <$> (fmap MImplicit $ convertBind  b) <*> convertX x

   XApp a p q
    -> XApp a  <$> convertX     p <*> convertArg q

   XLet a ls x
    -> let bs = valwitBindsOfLets ls
       in  withSuspFns bs $ XLet a <$> convertLets ls <*> convertX x

   XCase a x as
    -> XCase a <$> convertX     x <*> mapM convertAlt as

   XCast a c x
    -> XCast a <$> convertCast  c <*> convertX x

 where
  anno = annotOfExp xx

  mk = prim anno


prim anno n args
 = xApps anno (XVar anno (UName (T.NamePrimOp n))) args


---------------------------------------------------------------------------------------------------
convertArg :: Arg a F.Name -> ConvertM (Arg a T.Name)
convertArg arg
 = case arg of
        RType t         -> RType     <$> convertType t
        RWitness w      -> RWitness  <$> convertWit  w
        RTerm x         -> RTerm     <$> convertX    x
        RImplicit arg'  -> RImplicit <$> convertArg  arg'


---------------------------------------------------------------------------------------------------
convertApp :: Exp a F.Name -> [Arg a F.Name] -> ConvertM (Exp a T.Name)
convertApp f args
 = do   f'      <- convertX f
        -- filter out any type args that reference deleted XLAMs
        let checkT arg
             | RType (TVar (UName n)) <- arg
             = not <$> isRateXLAM n
             | otherwise
             = return True

        args'   <-  filterM checkT args
                >>= mapM convertArg

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


---------------------------------------------------------------------------------------------------
convertDaCon
        :: DaCon F.Name (Type F.Name)
        -> ConvertM (DaCon T.Name (Type T.Name))
convertDaCon dd
 = case dd of
   DaConUnit      -> return   DaConUnit
   DaConRecord ns -> return $ DaConRecord ns

   DaConPrim n
    -> DaConPrim  <$> convertName n

   DaConBound (DaConBoundName _ _ n)
    -> DaConBound <$> (DaConBoundName <$> pure Nothing <*> pure Nothing <*> convertName n)


---------------------------------------------------------------------------------------------------
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

 where
  liftMaybe f m
   = case m of
     Just a  -> Just <$> f a
     Nothing -> return Nothing

  both f g
   = \(a,b) -> (,) <$> f a <*> g b


---------------------------------------------------------------------------------------------------
convertAlt  :: Alt a F.Name -> ConvertM (Alt a T.Name)
convertAlt aa
 = case aa of
   AAlt p x
    -> AAlt <$> convertPat p <*> convertX x


---------------------------------------------------------------------------------------------------
convertPat :: Pat F.Name -> ConvertM (Pat T.Name)
convertPat pp
 = case pp of
   PDefault
    -> return $ PDefault
   PData dc bs
    -> PData <$> convertDaCon dc <*> mapM convertBind bs


---------------------------------------------------------------------------------------------------
convertCast :: Cast a F.Name -> ConvertM (Cast a T.Name)
convertCast cc
 = case cc of
   CastWeakenEffect et
    -> CastWeakenEffect  <$> convertType et

   CastPurify w
    -> CastPurify        <$> convertWit w

   CastBox
    -> return $ CastBox

   CastRun
    -> return $ CastRun



---------------------------------------------------------------------------------------------------
convertWit :: Witness a F.Name -> ConvertM (Witness a T.Name)
convertWit = error "ddc-core-flow.convertWit: cannot convert witness from core flow program"



---------------------------------------------------------------------------------------------------
-- | When replacing @/\(b : Rate). x@ with @x@, if @b@ is a de bruijn index
--   then any type vars in @x@ must be lowered.
-- @b@ must not be mentioned in @x@.
removeXLAM :: Bind F.Name -> Exp a T.Name -> Exp a T.Name
removeXLAM b t
 = case b of
   BAnon _
    -> lowerT 1 t
   _
    ->          t


-- | Get the Nat# of length from a Vector
xVecLen :: Type T.Name -> Exp a T.Name -> Exp a T.Name
xVecLen _t x
 = prim anno (T.PrimStore T.PrimStorePeek)
        [ RType rTop, RType T.tNat
        , RTerm $ projTuple anno x 0 (T.tPtr rTop T.tNat)
        , RTerm $ T.xNat anno 0 ]
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
 = let
       sz   = prim anno (T.PrimStore T.PrimStoreSize)
                [RType tY]

       addr = prim anno (T.PrimStore T.PrimStoreAlloc)
                [RTerm sz]

       ptr  = prim anno (T.PrimStore T.PrimStoreMakePtr)
                [RType rTop, RType tY, RTerm addr]

       ll   = LLet (BAnon $ T.tPtr rTop tY)
                   ptr

       ptr' = XVar anno $ UIx 0

       poke = prim anno (T.PrimStore T.PrimStorePoke)
               [ RType rTop, RType tY
               , RTerm ptr', RTerm $ T.xNat anno 0, RTerm val ]

   in  XLet anno ll
     $ XLet anno (LLet (BNone T.tVoid) poke)
       ptr'


allocPtr :: a -> Type T.Name -> Exp a T.Name -> Exp a T.Name
allocPtr anno tY elts
 = let
       sz   = prim anno (T.PrimStore T.PrimStoreSize)
                [ RType tY ]

       addr = prim anno (T.PrimStore T.PrimStoreAlloc)
                [ RTerm $ prim anno (T.PrimArith T.PrimArithMul)
                    [ RType T.tNat, RTerm elts, RTerm sz] ]

       ptr  = prim anno (T.PrimStore T.PrimStoreMakePtr)
                [ RType rTop, RType tY, RTerm addr ]

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
        [ RType rTop, RType t
        , RTerm x,    RTerm $ T.xNat anno 0 ]

projTuple :: a -> Exp a T.Name -> Integer -> Type T.Name -> Exp a T.Name
projTuple anno x i t
 = let t' = unptr t
 in tryunbox anno t
  $ castPtr  anno t' T.tObj
  $ xApps    anno (XVar anno $ UName $ T.NameVar "getFieldOfBoxed")
        [ RType rTop, RType $ T.tPtr rTop T.tObj
        , RTerm x,    RTerm $ T.xNat anno i ]


allocTupleN :: a -> [(Type T.Name, Exp a T.Name)] -> Exp a T.Name
allocTupleN anno txs
 = let tup  = xApps anno (XVar anno $ UName $ T.NameVar "allocBoxed")
                [ RType rTop
                , RTerm $ T.xTag anno 0
                , RTerm $ T.xNat anno (fromIntegral $ length txs) ]

       tup' = XVar anno $ UIx 0

       set i t x
        = let t' = unptr t
              x' = trybox anno t x
          in xApps anno (XVar anno $ UName $ T.NameVar "setFieldOfBoxed")
              [ RType rTop, RType (T.tPtr rTop T.tObj)
              , RTerm tup', RTerm (T.xNat anno i)
              , RTerm (castPtr anno T.tObj t' x') ]

   in  XLet anno (LLet (BAnon $ T.tPtr rTop T.tObj) tup)
     $ xLets anno
        [LLet (BNone T.tVoid) (set i t x) | ((t,x), i) <- txs `zip` [0..]]
       tup'


castPtr :: a -> Type T.Name -> Type T.Name -> Exp a T.Name -> Exp a T.Name
castPtr anno to from x
 = prim anno (T.PrimStore T.PrimStoreCastPtr)
        [ RType rTop, RType to, RType from, RTerm x ]


