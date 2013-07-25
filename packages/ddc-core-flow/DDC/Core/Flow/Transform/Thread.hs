
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
 | Just aResult         <- takeAnnotOfExp xResult
 , annotType aResult == tUnit     
 = reannotate annotTail xWorld

 -- Rewrite (TupleN        a1 a2 ..       x1 x2 ..) 
 --      => (TupleN World# a1 a2 .. world x1 x2 ..)
 | Just aWorld   <- takeAnnotOfExp xWorld
 , Just aResult  <- takeAnnotOfExp xResult
 = let  tWorld'  = annotType aWorld
        tResult  = annotType aResult
        xWorld'  = reannotate annotTail xWorld
        xResult' = reannotate annotTail xResult
   in   
        -- ISSUE #308: Handle Tuple arities generically in thread transform.
        case C.takeXConApps xResult' of
         Just (dc, [xT1, xT2
                   , x1, x2])
          | dc == dcTupleN 2
          -> C.xApps () (XCon () (dcTupleN 3))
                [ XType tWorld', xT1, xT2
                , xWorld',       x1,  x2]

         Just (dc, [xT1, xT2, xT3
                   , x1,  x2,  x3])
          | dc == dcTupleN 3
          -> C.xApps () (XCon () (dcTupleN 4))
                [ XType tWorld', xT1, xT2, xT3
                , xWorld',       x1,  x2,  x3]

         Just (dc, [xT1, xT2, xT3, xT4
                   , x1,  x2,  x3,  x4])
          | dc == dcTupleN 4
          -> C.xApps () (XCon () (dcTupleN 5))
                [ XType tWorld', xT1, xT2, xT3, xT4
                , xWorld',       x1,  x2,  x3,  x4]


         _ -> C.xApps () (XCon () (dcTupleN 2))
                         [ XType tWorld'
                         , XType tResult
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
                 $ \tA -> tA `tFunPE` tWorld 
                        `tFunPE` (tTuple2 tWorld (tRef tA))

        -- read# :: [a : Data]. Ref# a -> World# -> T2# (World#, a)
        NameOpStore OpStoreRead
         -> Just $ tForall kData
                 $ \tA -> tRef tA `tFunPE` tWorld 
                        `tFunPE` (tTuple2 tWorld (tRef tA))

        -- write# :: [a : Data]. Ref# -> a -> World# -> World#
        NameOpStore OpStoreWrite 
         -> Just $ tForall kData
                 $ \tA  -> tRef tA `tFunPE` tA 
                        `tFunPE` tWorld `tFunPE` tWorld

        -- Vectors -------------------------------
        -- newVector#   :: [a : Data]. Nat# -> World# -> T2# World# (Vector# a)
        NameOpStore OpStoreNewVector
         -> Just $ tForall kData
                 $ \tA -> tNat `tFunPE` tWorld 
                        `tFunPE` (tTuple2 tWorld (tVector tA))

        -- newVectorN#  :: [a : Data]. [k : Rate]. RateNat# k 
        --              -> World# -> T2# (World#, Vector# a)
        NameOpStore OpStoreNewVectorN
         -> Just $ tForalls [kData, kRate]
                 $ \[tA, tK] 
                     -> tRateNat tK 
                        `tFunPE` tWorld 
                        `tFunPE` (tTuple2 tWorld (tVector tA))

        -- readVector#  :: [a : Data]. Vector# a -> Nat# -> World# -> T2# World# a
        NameOpStore OpStoreReadVector
         -> Just $ tForall kData
                 $ \tA -> tA `tFunPE` tVector tA `tFunPE` tNat `tFunPE` tWorld
                        `tFunPE` (tTuple2 tWorld tA)

        -- writeVector# :: [a : Data]. Vector# a -> Nat# -> a -> World# -> World#
        NameOpStore OpStoreWriteVector
         -> Just $ tForall kData
                 $ \tA -> tA `tFunPE` tVector tA `tFunPE` tNat `tFunPE` tA 
                        `tFunPE` tWorld `tFunPE` tWorld

        -- sliceVector#   :: [a : Data]. Nat# -> Vector# a -> World# -> T2# World# (Vector# a)
        NameOpStore OpStoreSliceVector
         -> Just $ tForall kData
                 $ \tA -> tNat `tFunPE` tVector tA `tFunPE` tWorld 
                        `tFunPE` (tTuple2 tWorld (tVector tA))


        -- Streams ------------------------------
        -- next#  :: [k : Rate]. [a : Data]
        --        .  Series# k a -> Int# -> World# -> (World#, a)
        NameOpStore OpStoreNext
         -> Just $ tForalls [kRate, kData]
                 $ \[tK, tA] -> tSeries tK tA `tFunPE` tInt 
                                `tFunPE` tWorld `tFunPE` (tTuple2 tWorld tA)

        -- Contexts -----------------------------
        -- loopn#  :: [k : Rate]. RateNat# k 
        --         -> (Nat#  -> World# -> World#) 
        --         -> World# -> World#
        NameOpLoop  OpLoopLoopN
         -> Just $ tForalls [kRate]
                 $ \[tK] -> tRateNat tK
                                `tFunPE`  (tNat `tFunPE` tWorld `tFunPE` tWorld)
                                `tFunPE` tWorld `tFunPE` tWorld
        
        -- guard#
        NameOpLoop  OpLoopGuard
         -> Just $ tRef tNat
                        `tFunPE` tBool
                        `tFunPE` (tNat `tFunPE` tWorld `tFunPE` tWorld)
                        `tFunPE` tWorld `tFunPE` tWorld

        _ -> Nothing
