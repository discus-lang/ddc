module DDC.Core.Discus.Codec.Shimmer.Encode
        ( takeName
        , takeVarName
        , takeConName)
where
import qualified DDC.Core.Discus.Prim   as D
import qualified SMR.Core.Exp           as S
import qualified SMR.Prim.Name          as S
import qualified Data.Text              as T
import Data.Text                        (Text)
import DDC.Data.Textual


---------------------------------------------------------------------------------------------------
type SExp = S.Exp Text S.Prim


---------------------------------------------------------------------------------------------------
-- | Take the Shimmer encoding of a `Name`.
takeName :: D.Name -> SExp
takeName nn
 = case nn of
        D.NameVar tx            -> xAps "dv" [xText tx]
        D.NameCon tx            -> xAps "dc" [xText tx]
        D.NameExt n tx          -> xAps "de" [takeName n, xText tx]
        D.NameTyConDiscus tc    -> takeTyConDiscus tc
        D.NameDaConDiscus dc    -> takeDaConDiscus dc

        -- Op
        D.NameOpError op b      -> takeOpError op b
        D.NameOpFun op          -> takeOpFun op
        D.NameOpVector op b     -> takeOpVector op b
        D.NameOpInfo op b       -> takeOpInfo op b

        -- Prim
        D.NamePrimTyCon tc      -> takePrimTyCon tc
        D.NamePrimArith pa b    -> takePrimArith pa b
        D.NamePrimCast  pc b    -> takePrimCast  pc b

        -- Lit
        D.NameLitBool  b        -> xBool b
        D.NameLitNat   n        -> xNat n
        D.NameLitInt   i        -> xInt i

        D.NameLitWord  n b
         -> case b of
                8               -> S.XRef $ S.RPrm $ S.PrimLitWord8  (fromIntegral n)
                16              -> S.XRef $ S.RPrm $ S.PrimLitWord16 (fromIntegral n)
                32              -> S.XRef $ S.RPrm $ S.PrimLitWord32 (fromIntegral n)
                64              -> S.XRef $ S.RPrm $ S.PrimLitWord64 (fromIntegral n)
                _               -> error "ddc-core-discus.takeName: shimmer invalid word size"

        D.NameLitFloat d b
         -> case b of
                32              -> S.XRef $ S.RPrm $ S.PrimLitFloat32 (realToFrac d)
                64              -> S.XRef $ S.RPrm $ S.PrimLitFloat64 d
                _               -> error "ddc-core-discus.takeName: shimmer invalid float size"

        D.NameLitSize  n        -> xAps "l-s" [xNat n]
        D.NameLitChar  c        -> xAps "l-c" [xText $ T.pack [c]]
        D.NameLitTextLit tx     -> xAps "l-t" [xText tx]
        D.NameLitUnboxed n      -> xAps "l-u" [takeName n]

        -- Hole
        D.NameHole              -> xSym "dh"


-- | Take the Shimmer encoding of a variable name or `Nothing` if this isn't one.
takeVarName :: D.Name -> Maybe Text
takeVarName nn
 = case nn of
        D.NameVar tx            -> Just tx
        _                       -> Nothing


-- | Take the Shimmer encoding of a constructor name or `Nothing` if this isn't one.
takeConName :: D.Name -> Maybe Text
takeConName nn
 = case nn of
        D.NameCon tx            -> Just tx
        _                       -> Nothing


-- TyConDiscus ------------------------------------------------------------------------------------
-- | Take the Shimmer encoding of a Discus type constructor.
takeTyConDiscus :: D.TyConDiscus -> SExp
takeTyConDiscus tc
 = case tc of
        D.TyConDiscusTuple n    -> xAps "dt-Tuple" [xNat' n]
        D.TyConDiscusVector     -> xSym "dt-Vector"
        D.TyConDiscusU          -> xSym "dt-U"
        D.TyConDiscusF          -> xSym "dt-F"


-- DaConDiscus ------------------------------------------------------------------------------------
-- | Take the Shimmer encoding of a Discus data constructor.
takeDaConDiscus :: D.DaConDiscus -> SExp
takeDaConDiscus dc
 = case dc of
        D.DaConDiscusTuple n    -> xAps "dd-Tuple" [xNat' n]


-- Ops --------------------------------------------------------------------------------------------
-- | Take the Shimmer encoding of a error operator.
takeOpError :: D.OpError -> Bool -> SExp
takeOpError op b
 = case op of
        D.OpErrorDefault
         -> case b of
                False           -> xSym "oe-error-u"
                True            -> xSym "oe-error-b"


-- | Take the Shimmer encoding of a function operator.
takeOpFun :: D.OpFun -> SExp
takeOpFun op
 = case op of
        D.OpFunCurry n          -> xAps "of-curry"   [xNat' n]
        D.OpFunApply n          -> xAps "of-apply"   [xNat' n]
        D.OpFunReify            -> xSym "of-reify"


-- | Take the Shimmer encoding of a vector operator.
takeOpVector :: D.OpVector -> Bool -> SExp
takeOpVector op b
 = xSym (nop % nbx)
 where
  nop
   = case op of
        D.OpVectorAlloc         -> "ov-alloc"
        D.OpVectorLength        -> "ov-length"
        D.OpVectorRead          -> "ov-read"
        D.OpVectorWrite         -> "ov-write"

  nbx
   = case b of
        True                    -> "-u"
        False                   -> "-b"


-- | Take the Shimmer encoding of an info table operator.
takeOpInfo :: D.OpInfo -> Bool -> SExp
takeOpInfo pm bx
 = xSym (npm % nbx)
 where
   npm
    = case pm of
        D.OpInfoFrameNew        -> "oi-frame-new"
        D.OpInfoFramePush       -> "oi-frame-push"
        D.OpInfoFrameAddData    -> "oi-frame-add-data"

   nbx
    = case bx of
        True                    -> "-u"
        False                   -> "-b"


-- Prims ------------------------------------------------------------------------------------------
-- | Take the Shimmer encoding of a primitive type constructor.
takePrimTyCon :: D.PrimTyCon -> SExp
takePrimTyCon tc
 = case tc of
        D.PrimTyConVoid         -> xSym "pt-void"
        D.PrimTyConBool         -> xSym "pt-bool"
        D.PrimTyConNat          -> xSym "pt-nat"
        D.PrimTyConInt          -> xSym "pt-int"
        D.PrimTyConSize         -> xSym "pt-size"
        D.PrimTyConWord n       -> xAps "pt-word"  [xNat' n]
        D.PrimTyConFloat n      -> xAps "pt-float" [xNat' n]
        D.PrimTyConVec n        -> xAps "pt-vec"   [xNat' n]
        D.PrimTyConAddr         -> xSym "pt-addr"
        D.PrimTyConPtr          -> xSym "pt-ptr"
        D.PrimTyConTextLit      -> xSym "pt-textlit"
        D.PrimTyConTag          -> xSym "pt-tag"


-- | Take the Shimmer encoding of a primitive arithmetic operator.
takePrimArith :: D.PrimArith -> Bool -> SExp
takePrimArith pm bx
 = xSym (npm % nbx)
 where
   npm
    = case pm of
        D.PrimArithNeg          -> "pa-neg"
        D.PrimArithAdd          -> "pa-add"
        D.PrimArithSub          -> "pa-sub"
        D.PrimArithMul          -> "pa-mul"
        D.PrimArithDiv          -> "pa-div"
        D.PrimArithMod          -> "pa-mod"
        D.PrimArithRem          -> "pa-rem"
        D.PrimArithEq           -> "pa-eq"
        D.PrimArithNeq          -> "pa-neq"
        D.PrimArithGt           -> "pa-gt"
        D.PrimArithGe           -> "pa-ge"
        D.PrimArithLt           -> "pa-lt"
        D.PrimArithLe           -> "pa-le"
        D.PrimArithAnd          -> "pa-and"
        D.PrimArithOr           -> "pa-or"
        D.PrimArithShl          -> "pa-shl"
        D.PrimArithShr          -> "pa-shr"
        D.PrimArithBAnd         -> "pa-band"
        D.PrimArithBOr          -> "pa-bor"
        D.PrimArithBXOr         -> "pa-bxor"

   nbx
    = case bx of
        True                    -> "-u"
        False                   -> "-b"


-- | Take the Shimmer encoding of a primitive cast operator.
takePrimCast :: D.PrimCast -> Bool -> SExp
takePrimCast pc bx
 = xSym (npm % nbx)
 where
  npm
   = case pc of
        D.PrimCastConvert       -> "pc-convert"
        D.PrimCastPromote       -> "pc-promote"
        D.PrimCastTruncate      -> "pc-truncate"

  nbx
   = case bx of
       True                     -> "-u"
       False                    -> "-b"


-- Utils ------------------------------------------------------------------------------------------
xSym :: Text -> SExp
xSym tx = S.XRef (S.RSym tx)

xNat :: Integer -> SExp
xNat i  = S.XRef $ S.RPrm $ S.PrimLitNat i

xInt :: Integer -> SExp
xInt i  = S.XRef $ S.RPrm $ S.PrimLitInt i

xNat' :: Int -> SExp
xNat' i = S.XRef $ S.RPrm $ S.PrimLitNat $ fromIntegral i

xText :: Text -> SExp
xText tx = S.XRef (S.RTxt tx)

xBool :: Bool -> SExp
xBool b  = S.XRef $ S.RPrm $ S.PrimLitBool b

xAps :: Text -> [SExp] -> SExp
xAps t1 xs = S.XApp (S.XRef (S.RSym t1)) xs
