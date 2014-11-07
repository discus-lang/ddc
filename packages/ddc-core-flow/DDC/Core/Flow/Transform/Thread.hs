
-- | Definition for the thread transform.
module DDC.Core.Flow.Transform.Thread
        ( threadConfig
        , wrapResultType
        , wrapResultExp
        , unwrapResult
        , threadType)
where
import DDC.Core.Flow.Compounds
import DDC.Core.Flow.Profile
import DDC.Core.Flow.Prim
import DDC.Core.Compounds       as C
import DDC.Core.Exp
import DDC.Core.Transform.Thread
import DDC.Core.Transform.Reannotate
import DDC.Core.Check           (AnTEC (..))
import qualified DDC.Core.Check         as Check


-- | Thread config defines what state token to use,
--   and what functions need to have it threaded though them.
threadConfig :: Config () Name
threadConfig
        = Config
        { configCheckConfig      = Check.configOfProfile profile
        , configTokenType        = tWorld
        , configVoidType         = tUnit
        , configWrapResultType   = wrapResultType
        , configWrapResultExp    = wrapResultExp
        , configThreadMe         = threadType
        , configThreadPat        = unwrapResult }


-- | Wrap the result type of a stateful computation with the state type.
wrapResultType :: Type Name -> Type Name
wrapResultType tt
 | Just (TyConBound u _, tsArgs)        <- takeTyConApps tt
 , UPrim n _                            <- u
 , NameTyConFlow (TyConFlowTuple _)     <- n
 = tTupleN (tWorld : tsArgs)

 | otherwise
 = tTuple2 tWorld tt


-- | Wrap the result of a stateful computation with the state token.
wrapResultExp
        :: Exp (AnTEC () Name) Name     -- ^ World expression
        -> Exp (AnTEC () Name) Name     -- ^ Result expression
        -> Exp () Name

wrapResultExp xWorld xResult
 -- Rewrite Unit => World
 | aResult      <- annotOfExp xResult
 , annotType aResult == tUnit
 = reannotate annotTail xWorld

 -- Rewrite (TupleN        a1 a2 ..       x1 x2 ..)
 --      => (TupleN World# a1 a2 .. world x1 x2 ..)
 | aWorld   <- annotOfExp xWorld
 , aResult  <- annotOfExp xResult
 = let  tWorld'  = annotType aWorld
        tResult  = annotType aResult
        xWorld'  = reannotate annotTail xWorld
        xResult' = reannotate annotTail xResult
   in   case C.takeXConApps xResult' of
         Just (dc, xa)
          | DaConPrim (NameDaConFlow (DaConFlowTuple n)) _ <- dc
          , x <- length xa
          , x >= 2
          -> let (b, a) = splitAt (x `quot` 2) xa
             in C.xApps () (XCon () (dcTupleN $ n + 1))
                 $  XType (annotTail aWorld) tWorld' : b   -- World# : a1 a2 ..
                 ++ xWorld'                          : a   -- world  : x1 x2 ..

         _ -> C.xApps () (XCon () (dcTupleN 2))
                         [ XType (annotTail aWorld) tWorld'
                         , XType (annotTail aResult) tResult
                         , xWorld'
                         , xResult' ]

 | otherwise
 = error "ddc-core-flow: wrapResultExp can't get type annotations"


-- | Make a pattern to unwrap the result of a stateful computation.
unwrapResult   :: Name -> Maybe (Bind Name -> [Bind Name] -> Pat Name)
unwrapResult _
 = Just unwrap

 where  unwrap bWorld bsResult
         | [bResult]    <- bsResult
         , typeOfBind bResult == tUnit
         = PData dcTuple1 [bWorld]

         | otherwise
         = PData (dcTupleN (length (bWorld : bsResult)))
                 (bWorld : bsResult)


-- | Get the new type for a stateful primop.
--   The new types have a World# token threaded though them, which make them
--   suitable for applying the Thread transform when converting a Core Flow
--   program to a language that needs such state threading (like GHC Core).
threadType :: Name -> Type Name -> Maybe (Type Name)
threadType n _
 = case n of
        -- Assignables --------------------------
        -- new#  :: [a : Data]. a -> World# -> T2# (World#, Ref# a)
        NameOpStore OpStoreNew
         -> Just $ tForall kData
                 $ \tA -> tA
                        `tFun` tWorld `tFun` (tTuple2 tWorld (tRef tA))

        -- read# :: [a : Data]. Ref# a -> World# -> T2# (World#, a)
        NameOpStore OpStoreRead
         -> Just $ tForall kData
                 $ \tA -> tRef tA
                        `tFun` tWorld `tFun` (tTuple2 tWorld (tRef tA))

        -- write# :: [a : Data]. Ref# -> a -> World# -> World#
        NameOpStore OpStoreWrite
         -> Just $ tForall kData
                 $ \tA  -> tRef tA `tFun` tA
                        `tFun` tWorld `tFun` tWorld

        -- Vectors -------------------------------
        -- vnew#   :: [a : Data]. Nat# -> World# -> T2# World# (Vector# a)
        NameOpStore OpStoreNewVector
         -> Just $ tForall kData
                 $ \tA -> tNat
                        `tFun` tWorld `tFun` (tTuple2 tWorld (tVector tA))

        -- vnew#  :: [a : Data]. [k : Rate]. RateNat# k
        --        -> World# -> T2# (World#, Vector# a)
        NameOpStore OpStoreNewVectorN
         -> Just $ tForalls [kData, kRate]
                 $ \[tA, tK]
                     -> tRateNat tK
                        `tFun` tWorld `tFun` (tTuple2 tWorld (tVector tA))

        -- vread#  :: [a : Data]. Vector# a -> Nat# -> World# -> T2# World# a
        NameOpStore (OpStoreReadVector _)
         -> Just $ tForall kData
                 $ \tA -> tA `tFun` tBuffer tA `tFun` tNat
                        `tFun` tWorld `tFun` (tTuple2 tWorld tA)

        -- vwrite# :: [a : Data]. Vector# a -> Nat# -> a -> World# -> World#
        NameOpStore (OpStoreWriteVector _)
         -> Just $ tForall kData
                 $ \tA -> tA `tFun` tBuffer tA `tFun` tNat `tFun` tA
                        `tFun` tWorld `tFun` tWorld

        -- vtrunc# :: [a : Data]. Nat# -> Vector# a -> World# -> World#
        NameOpStore OpStoreTruncVector
         -> Just $ tForall kData
                 $ \tA -> tNat `tFun` tVector tA
                        `tFun` tWorld `tFun` tWorld

        -- Series ------------------------------
        -- next#  :: [k : Rate]. [a : Data]
        --        .  Series# k a -> Int# -> World# -> (World#, a)
        NameOpConcrete (OpConcreteNext 1)
         -> Just $ tForalls [kProc, kRate, kRate, kData]
                 $ \[tP, tK, tL, tA]
                        ->     tSeries tP tK tL tA `tFun` tInt
                        `tFun` tWorld `tFun` (tTuple2 tWorld tA)

        -- nextN# :: [k : Rate]. [a : Data]
        --        .  Series# k a -> Int# -> World# -> (World#, a)
        NameOpConcrete (OpConcreteNext c)
         | c >= 2
         -> Just $ tForalls [kProc, kRate, kRate, kData]
                 $ \[tP, tK, tL, tA]
                        ->     tSeries tP (tDown c tK) tL tA `tFun` tInt 
                        `tFun` tWorld `tFun` (tTuple2 tWorld (tVec c tA))

        -- Control -----------------------------
        -- loopn#  :: [k : Rate]. RateNat# k
        --         -> (Nat#  -> World# -> World#)
        --         -> World# -> World#
        NameOpControl OpControlLoopN
         -> Just $ tForalls [kRate]
                 $ \[tK] -> tRateNat tK
                        `tFun`  (tNat `tFun` tWorld `tFun` tWorld)
                        `tFun` tWorld `tFun` tWorld

        -- guard#
        NameOpControl OpControlGuard
         -> Just $ tRef tNat
                        `tFun` tBool
                        `tFun` (tNat  `tFun` tWorld `tFun` tWorld)
                        `tFun` tWorld `tFun` tWorld

        -- split#  :: [k : Rate]. RateNat# k
        --         -> (RateNat# (Down8# k) -> World# -> World#)
        --         -> (RateNat# (Tail8# k) -> World# -> World#)
        --         -> World# -> World#
        NameOpControl (OpControlSplit c)
         -> Just $ tForall kRate
          $ \tK -> tRateNat tK
                `tFun` (tRateNat (tDown c tK) `tFun` tWorld `tFun` tWorld)
                `tFun` (tRateNat (tTail c tK) `tFun` tWorld `tFun` tWorld)
                `tFun` tWorld `tFun` tWorld

        _ -> Nothing

