{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
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
        XAps "dt-Tuple" [XNat n]        -> Just $ D.NameTyConDiscus $ D.TyConDiscusTuple (fromI n)
        XSym "dt-Vector"                -> Just $ D.NameTyConDiscus $ D.TyConDiscusVector
        XSym "dt-U"                     -> Just $ D.NameTyConDiscus $ D.TyConDiscusU
        XSym "dt-F"                     -> Just $ D.NameTyConDiscus $ D.TyConDiscusF
        XSym "dt-C"                     -> Just $ D.NameTyConDiscus $ D.TyConDiscusC

        -- DaConDiscus
        XAps "dd-Tuple" [XNat n]        -> Just $ D.NameDaConDiscus $ D.DaConDiscusTuple $ fromI n

        -- OpError
        XSym "oe-error-u"               -> Just $ D.NameOpError     D.OpErrorDefault   True
        XSym "oe-error-b"               -> Just $ D.NameOpError     D.OpErrorDefault   False

        -- OpFun
        XAps "of-curry"   [XNat n]      -> Just $ D.NameOpFun     $ D.OpFunCurry     $ fromI n
        XAps "of-apply"   [XNat n]      -> Just $ D.NameOpFun     $ D.OpFunApply     $ fromI n
        XSym "of-creify"                -> Just $ D.NameOpFun       D.OpFunCReify
        XAps "of-ccurry"  [XNat n]      -> Just $ D.NameOpFun     $ D.OpFunCCurry    $ fromI n
        XAps "of-cextend" [XNat n]      -> Just $ D.NameOpFun     $ D.OpFunCExtend   $ fromI n
        XAps "of-capply"  [XNat n]      -> Just $ D.NameOpFun     $ D.OpFunCApply    $ fromI n

        -- OpVector
        XSym "ov-alloc-u"               -> Just $ D.NameOpVector    D.OpVectorAlloc    True
        XSym "ov-length-u"              -> Just $ D.NameOpVector    D.OpVectorAlloc    True
        XSym "ov-read-u"                -> Just $ D.NameOpVector    D.OpVectorAlloc    True
        XSym "ov-write-u"               -> Just $ D.NameOpVector    D.OpVectorAlloc    True

        XSym "ov-alloc-b"               -> Just $ D.NameOpVector    D.OpVectorAlloc    False
        XSym "ov-length-b"              -> Just $ D.NameOpVector    D.OpVectorAlloc    False
        XSym "ov-read-b"                -> Just $ D.NameOpVector    D.OpVectorAlloc    False
        XSym "ov-write-b"               -> Just $ D.NameOpVector    D.OpVectorAlloc    False

        -- PrimTyCon
        XSym "pt-void"                  -> Just $ D.NamePrimTyCon   D.PrimTyConVoid
        XSym "pt-bool"                  -> Just $ D.NamePrimTyCon   D.PrimTyConBool
        XSym "pt-nat"                   -> Just $ D.NamePrimTyCon   D.PrimTyConNat
        XSym "pt-int"                   -> Just $ D.NamePrimTyCon   D.PrimTyConInt
        XSym "pt-size"                  -> Just $ D.NamePrimTyCon   D.PrimTyConSize
        XAps "pt-word"  [XNat n]        -> Just $ D.NamePrimTyCon $ D.PrimTyConWord  $ fromI n
        XAps "pt-float" [XNat n]        -> Just $ D.NamePrimTyCon $ D.PrimTyConFloat $ fromI n
        XAps "pt-vec"   [XNat n]        -> Just $ D.NamePrimTyCon $ D.PrimTyConVec   $ fromI n
        XSym "pt-addr"                  -> Just $ D.NamePrimTyCon   D.PrimTyConAddr
        XSym "pt-ptr"                   -> Just $ D.NamePrimTyCon   D.PrimTyConPtr
        XSym "pt-textlit"               -> Just $ D.NamePrimTyCon   D.PrimTyConTextLit
        XSym "pt-tag"                   -> Just $ D.NamePrimTyCon   D.PrimTyConTag

        -- PrimArith
        XSym "pa-neg-u"                 -> Just $ D.NamePrimArith   D.PrimArithNeg     True
        XSym "pa-add-u"                 -> Just $ D.NamePrimArith   D.PrimArithAdd     True
        XSym "pa-sub-u"                 -> Just $ D.NamePrimArith   D.PrimArithSub     True
        XSym "pa-mul-u"                 -> Just $ D.NamePrimArith   D.PrimArithMul     True
        XSym "pa-div-u"                 -> Just $ D.NamePrimArith   D.PrimArithDiv     True
        XSym "pa-mod-u"                 -> Just $ D.NamePrimArith   D.PrimArithMod     True
        XSym "pa-rem-u"                 -> Just $ D.NamePrimArith   D.PrimArithRem     True
        XSym "pa-eq-u"                  -> Just $ D.NamePrimArith   D.PrimArithEq      True
        XSym "pa-neq-u"                 -> Just $ D.NamePrimArith   D.PrimArithNeq     True
        XSym "pa-gt-u"                  -> Just $ D.NamePrimArith   D.PrimArithGt      True
        XSym "pa-ge-u"                  -> Just $ D.NamePrimArith   D.PrimArithGe      True
        XSym "pa-lt-u"                  -> Just $ D.NamePrimArith   D.PrimArithLt      True
        XSym "pa-le-u"                  -> Just $ D.NamePrimArith   D.PrimArithLe      True
        XSym "pa-and-u"                 -> Just $ D.NamePrimArith   D.PrimArithAnd     True
        XSym "pa-or-u"                  -> Just $ D.NamePrimArith   D.PrimArithOr      True
        XSym "pa-shl-u"                 -> Just $ D.NamePrimArith   D.PrimArithShl     True
        XSym "pa-shr-u"                 -> Just $ D.NamePrimArith   D.PrimArithShr     True
        XSym "pa-band-u"                -> Just $ D.NamePrimArith   D.PrimArithBAnd    True
        XSym "pa-bor-u"                 -> Just $ D.NamePrimArith   D.PrimArithBOr     True
        XSym "pa-bxor-u"                -> Just $ D.NamePrimArith   D.PrimArithBXOr    True

        XSym "pa-neg-b"                 -> Just $ D.NamePrimArith   D.PrimArithNeg     False
        XSym "pa-add-b"                 -> Just $ D.NamePrimArith   D.PrimArithAdd     False
        XSym "pa-sub-b"                 -> Just $ D.NamePrimArith   D.PrimArithSub     False
        XSym "pa-mul-b"                 -> Just $ D.NamePrimArith   D.PrimArithMul     False
        XSym "pa-div-b"                 -> Just $ D.NamePrimArith   D.PrimArithDiv     False
        XSym "pa-mod-b"                 -> Just $ D.NamePrimArith   D.PrimArithMod     False
        XSym "pa-rem-b"                 -> Just $ D.NamePrimArith   D.PrimArithRem     False
        XSym "pa-eq-b"                  -> Just $ D.NamePrimArith   D.PrimArithEq      False
        XSym "pa-neq-b"                 -> Just $ D.NamePrimArith   D.PrimArithNeq     False
        XSym "pa-gt-b"                  -> Just $ D.NamePrimArith   D.PrimArithGt      False
        XSym "pa-ge-b"                  -> Just $ D.NamePrimArith   D.PrimArithGe      False
        XSym "pa-lt-b"                  -> Just $ D.NamePrimArith   D.PrimArithLt      False
        XSym "pa-le-b"                  -> Just $ D.NamePrimArith   D.PrimArithLe      False
        XSym "pa-and-b"                 -> Just $ D.NamePrimArith   D.PrimArithAnd     False
        XSym "pa-or-b"                  -> Just $ D.NamePrimArith   D.PrimArithOr      False
        XSym "pa-shl-b"                 -> Just $ D.NamePrimArith   D.PrimArithShl     False
        XSym "pa-shr-b"                 -> Just $ D.NamePrimArith   D.PrimArithShr     False
        XSym "pa-band-b"                -> Just $ D.NamePrimArith   D.PrimArithBAnd    False
        XSym "pa-bor-b"                 -> Just $ D.NamePrimArith   D.PrimArithBOr     False
        XSym "pa-bxor-b"                -> Just $ D.NamePrimArith   D.PrimArithBXOr    False

        -- PrimCast
        XSym "pc-convert-u"             -> Just $ D.NamePrimCast    D.PrimCastConvert  True
        XSym "pc-promote-u"             -> Just $ D.NamePrimCast    D.PrimCastPromote  True
        XSym "pc-truncate-u"            -> Just $ D.NamePrimCast    D.PrimCastTruncate True

        XSym "pc-convert-b"             -> Just $ D.NamePrimCast    D.PrimCastConvert  False
        XSym "pc-promote-b"             -> Just $ D.NamePrimCast    D.PrimCastPromote  False
        XSym "pc-truncate-b"            -> Just $ D.NamePrimCast    D.PrimCastTruncate False

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

        XAps "l-s" [XPrm (S.PrimLitNat n)]
                                        -> Just $ D.NameLitSize n

        XAps "l-c" [XTxt tx]
         | not $ T.null tx              -> Just $ D.NameLitChar (T.head tx)

        XAps "l-t" [XTxt tx]            -> Just $ D.NameLitTextLit tx

        XAps "l-u" [ssName]
         | Just n <- takeName ssName    -> Just $ D.NameLitUnboxed n

        -- Hole
        XSym "dh"                       -> Just $ D.NameHole

        _ -> error "ddc-core-discus.Codec.Shimmer.takeName failed"


pattern XTxt tx    = S.XRef (S.RTxt tx)
pattern XSym tx    = S.XRef (S.RSym tx)
pattern XPrm p     = S.XRef (S.RPrm p)
pattern XAps tx xs = S.XApp (S.XRef (S.RSym tx)) xs
pattern XNat n     = S.XRef (S.RPrm (S.PrimLitNat n))

fromI = fromIntegral
