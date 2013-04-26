
-- | Definition for the thread transform.
module DDC.Core.Flow.PrimState.Thread
        ( threadConfig
        , wrapResultType
        , wrapResultExp
        , unwrapResult
        , threadType)
where
import DDC.Core.Flow.Env
import DDC.Core.Flow.Prim
import DDC.Core.Flow.Compounds
import DDC.Core.Transform.Thread
import DDC.Core.Exp


-- | Thread config defines what state token to use,
--   and what functions need to have it threaded though them.
threadConfig :: Config () Name
threadConfig
        = Config
        { configDataDefs         = primDataDefs
        , configTokenType        = tWorld
        , configVoidType         = tVoid
        , configWrapResultType   = wrapResultType
        , configWrapResultExp    = wrapResultExp
        , configThreadMe         = threadType 
        , configThreadPat        = unwrapResult }


-- | Wrap the result type of a stateful computation with the state type.
wrapResultType :: Type Name -> Type Name
wrapResultType tt
 = tTuple2 tWorld tt


-- | Wrap the result of a stateful computation with the state token.
wrapResultExp  :: Exp () Name -> Exp () Name -> Exp () Name
wrapResultExp xWorld xResult
 = xTuple2 () xWorld xResult


-- | Make a pattern to unwrap the result of a stateful computation.
unwrapResult   :: Name -> Maybe (Bind Name -> Bind Name -> Pat Name)
unwrapResult _
 = Just unwrap
 where  unwrap bWorld bResult 
         = PData dcTuple2 [bWorld, bResult]


-- | Get the new type for a stateful primop.
--   The new types have a World# token threaded though them, which make them
--   suitable for applying the Thread transform when converting a Core Flow
--   program to a language that needs such state threading (like GHC Core).
threadType :: Name -> Maybe (Type Name)
threadType n
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

        -- write# :: [a : Data]. Ref# -> a -> T2# (World#, Unit)
        NameOpStore OpStoreWrite 
         -> Just $ tForall kData
                 $ \tA -> tRef tA `tFunPE` tA `tFunPE` tWorld
                        `tFunPE` (tTuple2 tWorld tUnit)

        -- Arrays -------------------------------
        -- newArray#   :: [a : Data]. a -> Nat# -> World# -> World#
        NameOpStore OpStoreNewArray
         -> Just $ tForall kData
                 $ \tA -> tA `tFunPE` tNat `tFunPE` tWorld 
                        `tFunPE` (tTuple2 tWorld (tArray tA))

        -- readArray#  :: [a : Data]. Array# a -> Nat# -> World# -> T2# (World#, a)
        NameOpStore OpStoreReadArray
         -> Just $ tForall kData
                 $ \tA -> tA `tFunPE` tArray tA `tFunPE` tNat `tFunPE` tWorld
                        `tFunPE` (tTuple2 tWorld tA)

        -- writeArray# :: [a : Data]. Array# a -> Nat# -> a -> World# -> T2 (World#, Void#)
        NameOpStore OpStoreWriteArray
         -> Just $ tForall kData
                 $ \tA -> tA `tFunPE` tArray tA `tFunPE` tNat `tFunPE` tA `tFunPE` tWorld
                        `tFunPE` (tTuple2 tWorld tVoid)

        -- Streams ------------------------------
        -- next#  :: [k : Rate]. [a : Data]
        --        .  Stream# k a -> Int# -> World# -> (World#, a)
        NameOpStore OpStoreNext
         -> Just $ tForalls [kRate, kData]
                 $ \[tK, tA] -> tStream tK tA `tFunPE` tInt `tFunPE` tWorld
                             `tFunPE` (tTuple2 tWorld tA)

        -- loop#  :: Nat# -> (Nat# -> World# -> (World#, Unit)) 
        --        -> World# -> T2# (World#, Unit)
        NameOpLoop  OpLoopLoop
         -> Just $ tNat 
                `tFunPE` (tNat `tFunPE` tWorld `tFunPE` tTuple2 tWorld tNat) 
                `tFunPE` tWorld
                `tFunPE` (tTuple2 tWorld tUnit)

        _ -> Nothing
