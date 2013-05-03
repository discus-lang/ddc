
module DDC.Core.Flow.Prim.OpStore
        ( OpStore (..)
        , readOpStore
        , typeOpStore
        , xNew,      xRead,      xWrite
        , xNewArray, xReadArray, xWriteArray
        , xNext)
where
import DDC.Core.Flow.Prim.KiConFlow
import DDC.Core.Flow.Prim.TyConFlow
import DDC.Core.Flow.Prim.TyConPrim
import DDC.Core.Flow.Prim.Base
import DDC.Core.Compounds
import DDC.Core.Exp
import DDC.Base.Pretty
import Control.DeepSeq


instance NFData OpStore


instance Pretty OpStore where
 ppr so
  = case so of
        -- Assignables.
        OpStoreNew              -> text "new#"
        OpStoreRead             -> text "read#"
        OpStoreWrite            -> text "write#"

        -- Arrays.
        OpStoreNewArray         -> text "newArray#"
        OpStoreReadArray        -> text "readArray#"
        OpStoreWriteArray       -> text "writeArray#"

        -- Streams.
        OpStoreNext             -> text "next#"


-- | Read a baked-in store operator.
readOpStore :: String -> Maybe OpStore
readOpStore str
 = case str of
        "new#"          -> Just OpStoreNew
        "read#"         -> Just OpStoreRead
        "write#"        -> Just OpStoreWrite

        "newArray#"     -> Just OpStoreNewArray
        "readArray#"    -> Just OpStoreReadArray
        "writeArray#"   -> Just OpStoreWriteArray

        "next#"         -> Just OpStoreNext
        _               -> Nothing


-- Types ----------------------------------------------------------------------
typeOpStore :: OpStore -> Type Name
typeOpStore op
 = case op of
        -- Assignables ----------------
        -- new#        :: [a : Data]. a -> Array# a
        OpStoreNew
         -> tForall kData $ \tA -> tA `tFunPE` tRef tA

        -- read#       :: [a : Data]. Ref# a -> a
        OpStoreRead
         -> tForall kData $ \tA -> tRef tA `tFunPE` tA

        -- write#      :: [a : Data]. Ref# a -> a -> Unit
        OpStoreWrite
         -> tForall kData $ \tA -> tRef tA `tFunPE` tA `tFunPE` tVoid

        -- Arrays ---------------------
        -- newArray#   :: [a : Data]. Nat# -> Array# a
        OpStoreNewArray
         -> tForall kData $ \tA -> tNat `tFunPE` tArray tA

        -- readArray#  :: [a : Data]. Array# a -> NAt# -> a
        OpStoreReadArray
         -> tForall kData 
         $  \tA -> tArray tA `tFunPE` tNat `tFunPE` tA

        -- writeArray# :: [a : Data]. Array# a -> Nat# -> a -> Void#
        OpStoreWriteArray 
         -> tForall kData 
         $  \tA -> tArray tA `tFunPE` tNat `tFunPE` tA `tFunPE` tVoid

        -- Streams --------------------
        -- next#  :: [a : Data]. [k : Rate]. Stream# k a -> Nat# -> a
        OpStoreNext
         -> tForalls [kData, kRate]
         $  \[tA, tK] -> tStream tK tA `tFunPE` tNat `tFunPE` tA


-- Compounds ------------------------------------------------------------------
xNew :: Type Name -> Exp () Name -> Exp () Name
xNew t xV
 = xApps () (xVarOpStore OpStoreNew)
            [XType t, xV ]


xRead :: Type Name -> Exp () Name -> Exp () Name
xRead t xRef
 = xApps () (xVarOpStore OpStoreRead)
            [XType t, xRef ]


xWrite :: Type Name -> Exp () Name -> Exp () Name -> Exp () Name
xWrite t xRef xVal
 = xApps () (xVarOpStore OpStoreWrite)
            [XType t, xRef, xVal ]


xNewArray :: Type Name -> Exp () Name -> Exp () Name
xNewArray t xLen
 = xApps () (xVarOpStore OpStoreNewArray)
            [XType t, xLen]


xReadArray :: Type Name -> Exp () Name -> Exp () Name -> Exp () Name
xReadArray t xArr xIx
 = xApps () (xVarOpStore OpStoreReadArray)
            [XType t, xArr, xIx]


xWriteArray :: Type Name -> Exp () Name -> Exp () Name -> Exp () Name -> Exp () Name
xWriteArray t xArr xIx xElem
 = xApps () (xVarOpStore OpStoreWriteArray)
            [XType t, xArr, xIx, xElem]


xNext  :: Type Name -> Type Name -> Exp () Name -> Exp () Name -> Exp () Name
xNext tRate tElem xStream xIndex
 = xApps () (xVarOpStore OpStoreNext)
            [XType tRate, XType tElem, xStream, xIndex]


-- Utils ----------------------------------------------------------------------
xVarOpStore :: OpStore -> Exp () Name
xVarOpStore op
        = XVar () (UPrim (NameOpStore op) (typeOpStore op))





