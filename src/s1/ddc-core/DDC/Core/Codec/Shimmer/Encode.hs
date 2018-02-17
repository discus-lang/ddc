{-# LANGUAGE OverloadedStrings #-}
module DDC.Core.Codec.Shimmer.Encode
        (fromType)
where
import qualified DDC.Core.Exp           as C
import qualified SMR.Core.Exp           as S
import qualified SMR.Prim.Op.Base       as S
import qualified DDC.Type.Sum           as Sum
import Data.Text                        (Text)


---------------------------------------------------------------------------------------------------
type SRef = S.Ref Text S.Prim
type SExp = S.Exp Text S.Prim


-- Type -------------------------------------------------------------------------------------------
fromType :: (n -> SRef) -> C.Type n -> SExp
fromType fromRef tt
 = case tt of
        C.TCon tc
         -> xApp (xSym "t-con") [fromTyCon fromRef tc]

        C.TVar u
         -> xApp (xSym "t-var") [fromBound fromRef u]

        C.TAbs b t
         -> xApp (xSym "t-abs") [fromBind  fromRef b, fromType fromRef t]

        C.TApp t1 t2
         -> xApp (xSym "t-app") [fromType fromRef t1, fromType fromRef t2]

        C.TForall b t
         -> xApp (xSym "t-all") [fromBind  fromRef b, fromType fromRef t]

        C.TSum ts
         -> xApp (xSym "t-sum") (map (fromType fromRef) $ Sum.toList ts)


-- Bind -------------------------------------------------------------------------------------------
fromBind  :: (n -> SRef) -> C.Bind n -> SExp
fromBind fromRef bb
 = case bb of
        C.BNone t               -> xApp (xSym "b-o") [fromType fromRef t]
        C.BAnon t               -> xApp (xSym "b-a") [fromType fromRef t]
        C.BName n t             -> xApp (xSym "b-n") [xRef $ fromRef n, fromType fromRef t]


-- Bound ------------------------------------------------------------------------------------------
fromBound :: (n -> SRef) -> C.Bound n -> SExp
fromBound fromRef uu
 = case uu of
        C.UIx i                 -> xApp (xSym "u-i") [xNat i]
        C.UName n               -> xApp (xSym "u-n") [xRef $ fromRef n]
        C.UPrim n _t            -> xApp (xSym "u-p") [xRef $ fromRef n]


-- TyCon ------------------------------------------------------------------------------------------
fromTyCon :: (n -> SRef) -> C.TyCon n -> SExp
fromTyCon fromRef tc
 = case tc of
        C.TyConSort    c        -> fromSoCon c
        C.TyConKind    c        -> fromKiCon c
        C.TyConWitness c        -> fromTwCon c
        C.TyConSpec    c        -> fromTcCon c

        C.TyConBound   u k
         -> xApp (xSym "tc-bnd") [fromBound fromRef u, fromType fromRef k]

        C.TyConExists    i k
         -> xApp (xSym "tc-ext") [xNat i, fromType fromRef k]


fromSoCon :: C.SoCon -> SExp
fromSoCon c
 = case c of
        C.SoConProp             -> xSym "tc-prop"
        C.SoConComp             -> xSym "tc-comp"


fromKiCon :: C.KiCon -> SExp
fromKiCon c
 = case c of
        C.KiConFun              -> xSym "tc-funn"
        C.KiConWitness          -> xSym "tc-witn"
        C.KiConData             -> xSym "tc-data"
        C.KiConRegion           -> xSym "tc-rgnn"
        C.KiConEffect           -> xSym "tc-efft"
        C.KiConClosure          -> xSym "tc-clos"


fromTwCon :: C.TwCon -> SExp
fromTwCon c
 = case c of
        C.TwConImpl             -> xSym "tc-impl"
        C.TwConPure             -> xSym "tc-pure"
        C.TwConConst            -> xSym "tc-cnst"
        C.TwConDeepConst        -> xSym "tc-dcns"
        C.TwConMutable          -> xSym "tc-mutb"
        C.TwConDeepMutable      -> xSym "tc-dmut"
        C.TwConDistinct n       -> xApp (xSym "tc-dist") [xNat n]
        C.TwConDisjoint         -> xSym "tc-disj"


fromTcCon :: C.TcCon -> SExp
fromTcCon c
 = case c of
        C.TcConUnit             -> xSym "tc-unit"
        C.TcConFunExplicit      -> xSym "tc-fune"
        C.TcConFunImplicit      -> xSym "tc-funi"
        C.TcConSusp             -> xSym "tc-susp"
        C.TcConRecord ts        -> xApp (xSym "tc-recd") (map xSym ts)
        C.TcConRead             -> xSym "tc-read"
        C.TcConHeadRead         -> xSym "tc-hred"
        C.TcConDeepRead         -> xSym "tc-dred"
        C.TcConWrite            -> xSym "tc-writ"
        C.TcConDeepWrite        -> xSym "tc-dwri"
        C.TcConAlloc            -> xSym "tc-aloc"
        C.TcConDeepAlloc        -> xSym "tc-dalo"


-- Base -------------------------------------------------------------------------------------------
xRef :: SRef -> SExp
xRef r = S.XRef r

xSym :: Text -> SExp
xSym tx    = S.XRef (S.RSym tx)

xApp :: SExp -> [SExp] -> SExp
xApp x1 xs = S.XApp x1 xs

xNat :: Int -> SExp
xNat i = S.XRef (S.RPrm (S.PrimLitNat $ fromIntegral i))

