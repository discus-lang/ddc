module DDC.Core.Discus.Codec.Shimmer.Decode
        (takeName)
where
import qualified DDC.Core.Discus.Prim   as D
import qualified SMR.Core.Exp           as S
import qualified SMR.Prim.Name          as S

import Data.Maybe
import Data.Text                        (Text)
import qualified Data.Text              as T
import qualified Data.Char              as Char

---------------------------------------------------------------------------------------------------
type SExp = S.Exp Text S.Prim

---------------------------------------------------------------------------------------------------
-- | Extract a `Name` from a Shimmer expression,
--   or `error` if it is unrecognised.
fromName :: SExp -> D.Name
fromName ss
        = fromMaybe (error "fromName failed")
        $ takeName ss


-- | Take the Shimmer encoding of a `Name`.
takeName :: SExp -> Maybe D.Name
takeName ss
 = case ss of
        XTxt  tx
         |  not $ T.null tx
         -> if Char.isUpper $ T.head tx
                then Just $ D.NameCon tx
                else Just $ D.NameVar tx

        XAps "dv" [XTxt tx]             -> Just $ D.NameVar tx
        XAps "dc" [XTxt tx]             -> Just $ D.NameCon tx
        XAps "de" [ssName, XTxt tx]     -> Just $ D.NameExt (fromName ssName) tx

        -- TyConDiscus
        XAps "dc-tuple" [XNat n]        -> Just $ D.NameTyConDiscus $ D.TyConDiscusTuple (fromI n)
        XSym "dc-vector"                -> Just $ D.NameTyConDiscus $ D.TyConDiscusVector
        XSym "dc-u"                     -> Just $ D.NameTyConDiscus $ D.TyConDiscusU
        XSym "dc-f"                     -> Just $ D.NameTyConDiscus $ D.TyConDiscusF

        -- DaConDiscus
        XAps "dc-tuple" [XNat n]        -> Just $ D.NameDaConDiscus $ D.DaConDiscusTuple $ fromI n

        -- OpError
        XSym "op-error-case"            -> Just $ D.NameOpError     D.OpErrorDefault   True
        XSym "op-error-case-b"          -> Just $ D.NameOpError     D.OpErrorDefault   False

        -- OpFun
        XAps "op-curry"   [XNat n]      -> Just $ D.NameOpFun     $ D.OpFunCurry     $ fromI n
        XAps "op-apply"   [XNat n]      -> Just $ D.NameOpFun     $ D.OpFunApply     $ fromI n
        XSym "op-reify"                 -> Just $ D.NameOpFun       D.OpFunReify

        -- OpVector
        XSym "op-alloc"                 -> Just $ D.NameOpVector    D.OpVectorAlloc    True
        XSym "op-length"                -> Just $ D.NameOpVector    D.OpVectorAlloc    True
        XSym "op-read"                  -> Just $ D.NameOpVector    D.OpVectorAlloc    True
        XSym "op-write"                 -> Just $ D.NameOpVector    D.OpVectorAlloc    True

        XSym "op-alloc-b"               -> Just $ D.NameOpVector    D.OpVectorAlloc    False
        XSym "op-length-b"              -> Just $ D.NameOpVector    D.OpVectorAlloc    False
        XSym "op-read-b"                -> Just $ D.NameOpVector    D.OpVectorAlloc    False
        XSym "op-write-b"               -> Just $ D.NameOpVector    D.OpVectorAlloc    False

        -- OpInfo
        XSym "op-frame-new"             -> Just $ D.NameOpInfo      D.OpInfoFrameNew      True
        XSym "op-frame-push"            -> Just $ D.NameOpInfo      D.OpInfoFramePush     True
        XSym "op-frame-add-data"        -> Just $ D.NameOpInfo      D.OpInfoFrameAddData  True
        XSym "op-frame-add-super"       -> Just $ D.NameOpInfo      D.OpInfoFrameAddSuper True

        XSym "op-frame-new-b"           -> Just $ D.NameOpInfo      D.OpInfoFrameNew      False
        XSym "op-frame-push-b"          -> Just $ D.NameOpInfo      D.OpInfoFramePush     False
        XSym "op-frame-add-data-b"      -> Just $ D.NameOpInfo      D.OpInfoFrameAddData  False
        XSym "op-frame-add-super-b"     -> Just $ D.NameOpInfo      D.OpInfoFrameAddSuper False

        -- PrimTyCon
        XSym "tc-void"                  -> Just $ D.NamePrimTyCon   D.PrimTyConVoid
        XSym "tc-bool"                  -> Just $ D.NamePrimTyCon   D.PrimTyConBool
        XSym "tc-nat"                   -> Just $ D.NamePrimTyCon   D.PrimTyConNat
        XSym "tc-int"                   -> Just $ D.NamePrimTyCon   D.PrimTyConInt
        XSym "tc-size"                  -> Just $ D.NamePrimTyCon   D.PrimTyConSize
        XAps "tc-word"  [XNat n]        -> Just $ D.NamePrimTyCon $ D.PrimTyConWord  $ fromI n
        XAps "tc-float" [XNat n]        -> Just $ D.NamePrimTyCon $ D.PrimTyConFloat $ fromI n
        XAps "tc-vec"   [XNat n]        -> Just $ D.NamePrimTyCon $ D.PrimTyConVec   $ fromI n
        XSym "tc-addr"                  -> Just $ D.NamePrimTyCon   D.PrimTyConAddr
        XSym "tc-ptr"                   -> Just $ D.NamePrimTyCon   D.PrimTyConPtr
        XSym "tc-textlit"               -> Just $ D.NamePrimTyCon   D.PrimTyConTextLit
        XSym "tc-tag"                   -> Just $ D.NamePrimTyCon   D.PrimTyConTag

        -- PrimArith
        XSym "op-neg"                   -> Just $ D.NamePrimArith   D.PrimArithNeg     True
        XSym "op-add"                   -> Just $ D.NamePrimArith   D.PrimArithAdd     True
        XSym "op-sub"                   -> Just $ D.NamePrimArith   D.PrimArithSub     True
        XSym "op-mul"                   -> Just $ D.NamePrimArith   D.PrimArithMul     True
        XSym "op-div"                   -> Just $ D.NamePrimArith   D.PrimArithDiv     True
        XSym "op-mod"                   -> Just $ D.NamePrimArith   D.PrimArithMod     True
        XSym "op-rem"                   -> Just $ D.NamePrimArith   D.PrimArithRem     True
        XSym "op-eq"                    -> Just $ D.NamePrimArith   D.PrimArithEq      True
        XSym "op-neq"                   -> Just $ D.NamePrimArith   D.PrimArithNeq     True
        XSym "op-gt"                    -> Just $ D.NamePrimArith   D.PrimArithGt      True
        XSym "op-ge"                    -> Just $ D.NamePrimArith   D.PrimArithGe      True
        XSym "op-lt"                    -> Just $ D.NamePrimArith   D.PrimArithLt      True
        XSym "op-le"                    -> Just $ D.NamePrimArith   D.PrimArithLe      True
        XSym "op-and"                   -> Just $ D.NamePrimArith   D.PrimArithAnd     True
        XSym "op-or"                    -> Just $ D.NamePrimArith   D.PrimArithOr      True
        XSym "op-shl"                   -> Just $ D.NamePrimArith   D.PrimArithShl     True
        XSym "op-shr"                   -> Just $ D.NamePrimArith   D.PrimArithShr     True
        XSym "op-band"                  -> Just $ D.NamePrimArith   D.PrimArithBAnd    True
        XSym "op-bor"                   -> Just $ D.NamePrimArith   D.PrimArithBOr     True
        XSym "op-bxor"                  -> Just $ D.NamePrimArith   D.PrimArithBXOr    True

        XSym "op-neg-b"                 -> Just $ D.NamePrimArith   D.PrimArithNeg     False
        XSym "op-add-b"                 -> Just $ D.NamePrimArith   D.PrimArithAdd     False
        XSym "op-sub-b"                 -> Just $ D.NamePrimArith   D.PrimArithSub     False
        XSym "op-mul-b"                 -> Just $ D.NamePrimArith   D.PrimArithMul     False
        XSym "op-div-b"                 -> Just $ D.NamePrimArith   D.PrimArithDiv     False
        XSym "op-mod-b"                 -> Just $ D.NamePrimArith   D.PrimArithMod     False
        XSym "op-rem-b"                 -> Just $ D.NamePrimArith   D.PrimArithRem     False
        XSym "op-eq-b"                  -> Just $ D.NamePrimArith   D.PrimArithEq      False
        XSym "op-neq-b"                 -> Just $ D.NamePrimArith   D.PrimArithNeq     False
        XSym "op-gt-b"                  -> Just $ D.NamePrimArith   D.PrimArithGt      False
        XSym "op-ge-b"                  -> Just $ D.NamePrimArith   D.PrimArithGe      False
        XSym "op-lt-b"                  -> Just $ D.NamePrimArith   D.PrimArithLt      False
        XSym "op-le-b"                  -> Just $ D.NamePrimArith   D.PrimArithLe      False
        XSym "op-and-b"                 -> Just $ D.NamePrimArith   D.PrimArithAnd     False
        XSym "op-or-b"                  -> Just $ D.NamePrimArith   D.PrimArithOr      False
        XSym "op-shl-b"                 -> Just $ D.NamePrimArith   D.PrimArithShl     False
        XSym "op-shr-b"                 -> Just $ D.NamePrimArith   D.PrimArithShr     False
        XSym "op-band-b"                -> Just $ D.NamePrimArith   D.PrimArithBAnd    False
        XSym "op-bor-b"                 -> Just $ D.NamePrimArith   D.PrimArithBOr     False
        XSym "op-bxor-b"                -> Just $ D.NamePrimArith   D.PrimArithBXOr    False

        -- PrimCast
        XSym "op-convert"               -> Just $ D.NamePrimCast    D.PrimCastConvert  True
        XSym "op-promote"               -> Just $ D.NamePrimCast    D.PrimCastPromote  True
        XSym "op-truncate"              -> Just $ D.NamePrimCast    D.PrimCastTruncate True

        XSym "op-convert-b"             -> Just $ D.NamePrimCast    D.PrimCastConvert  False
        XSym "op-promote-b"             -> Just $ D.NamePrimCast    D.PrimCastPromote  False
        XSym "op-truncate-b"            -> Just $ D.NamePrimCast    D.PrimCastTruncate False

        -- PrimLit
        XPrm (S.PrimLitBool b)          -> Just $ D.NameLitBool b
        XPrm (S.PrimLitNat  n)          -> Just $ D.NameLitNat  n
        XPrm (S.PrimLitInt  i)          -> Just $ D.NameLitInt  i

        XPrm (S.PrimLitWord8  n)        -> Just $ D.NameLitWord (fromI n) 8
        XPrm (S.PrimLitWord16 n)        -> Just $ D.NameLitWord (fromI n) 16
        XPrm (S.PrimLitWord32 n)        -> Just $ D.NameLitWord (fromI n) 32
        XPrm (S.PrimLitWord64 n)        -> Just $ D.NameLitWord (fromI n) 64

        XPrm (S.PrimLitFloat32 d)       -> Just $ D.NameLitFloat (realToFrac d) 32
        XPrm (S.PrimLitFloat64 d)       -> Just $ D.NameLitFloat d 64

        XAps "lt-size" [XPrm (S.PrimLitNat n)]
                                        -> Just $ D.NameLitSize n

        XAps "lt-char" [XTxt tx]
         | not $ T.null tx              -> Just $ D.NameLitChar (T.head tx)

        XAps "lt-text" [XTxt tx]        -> Just $ D.NameLitTextLit tx

        XAps "lt-unboxed" [ssName]
         | Just n <- takeName ssName    -> Just $ D.NameLitUnboxed n

        -- Hole
        XSym "hole"                     -> Just $ D.NameHole

        _ -> error $ unlines
                [ "ddc-core-discus.Codec.Shimmer.takeName failed."
                , " shimmer expression was:"
                , show ss ]


pattern XTxt tx    = S.XRef (S.RTxt tx)
pattern XSym tx    = S.XRef (S.RSym tx)
pattern XPrm p     = S.XRef (S.RPrm p)
pattern XAps tx xs = S.XApp (S.XRef (S.RSym tx)) xs
pattern XNat n     = S.XRef (S.RPrm (S.PrimLitNat n))

fromI = fromIntegral
