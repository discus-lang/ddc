{-# LANGUAGE OverloadedStrings #-}
module DDC.Core.Codec.Shimmer.Encode
        ( takeModule
        , takeModuleName
        , takeExportSource
        , takeExp,   takeParam,   takeArg,  takePrim
        , takeLets
        , takeAlt,   takePat,     takeDaCon
        , takeCast,  takeWitness, takeWiCon
        , takeType
        , takeBind,  takeBound
        , takeTyCon, takeSoCon,   takeKiCon, takeTwCon, takeTcCon)
where
import qualified DDC.Core.Module        as C
import qualified DDC.Core.Exp           as C
import qualified SMR.Core.Exp           as S
import qualified SMR.Prim.Op.Base       as S
import qualified DDC.Type.Sum           as Sum
import qualified Data.Text              as T
import Data.Text                        (Text)


---------------------------------------------------------------------------------------------------
type SRef = S.Ref Text S.Prim
type SExp = S.Exp Text S.Prim


-- Module -----------------------------------------------------------------------------------------
takeModule :: (n -> SRef) -> C.Module a n -> SExp
takeModule takeRef mm@C.ModuleCore{}
 = xAps "module"
        [ xAps  "m-name"
                [takeModuleName (C.moduleName mm)]

        , xAps  "m-exp-typs"
                (map (takeExportSource takeRef) $ map snd $ C.moduleExportTypes mm)

        , xAps  "m-exp-vals"
                (map (takeExportSource takeRef) $ map snd $ C.moduleExportValues mm)

        -- TODO: other stuff to fill in

        , xAps  "m-body"
                [takeExp takeRef $ C.moduleBody mm] ]


-- ModuleName -------------------------------------------------------------------------------------
takeModuleName :: C.ModuleName -> SExp
takeModuleName (C.ModuleName parts)
 = xAps "module-name" (map (xSym . T.pack) parts)


-- ExportSource------------------------------------------------------------------------------------
takeExportSource :: (n -> SRef) -> C.ExportSource n (C.Type n) -> SExp
takeExportSource takeRef es
 = case es of
        C.ExportSourceLocal n t
         -> xAps "exsrc-local" [xRef $ takeRef n, takeType takeRef t]

        C.ExportSourceLocalNoType n
         -> xAps "exsrc-notyp" [xRef $ takeRef n]


-- Exp --------------------------------------------------------------------------------------------
takeExp :: (n -> SRef) -> C.Exp a n -> SExp
takeExp takeRef mm
 = case mm of
        C.XPrim _ p     -> xAps "x-prm" [takePrim p]
        C.XCon  _ dc    -> xAps "x-con" [takeDaCon takeRef dc]
        C.XVar  _ u     -> xAps "x-var" [takeBound takeRef u]
        C.XAbs  _ p x   -> xAps "x-abs" [takeParam takeRef p,   takeExp takeRef x]
        C.XApp  _ x a   -> xAps "x-app" [takeExp   takeRef x,   takeArg takeRef a]
        C.XLet  _ lts x -> xAps "x-let" [takeLets  takeRef lts, takeExp takeRef x]
        C.XCase _ x as  -> xAps "x-cse" [takeExp   takeRef x,   xList (map (takeAlt takeRef) as) ]
        C.XCast _ c x   -> xAps "x-cst" [takeCast  takeRef c,   takeExp takeRef x]


-- Param ------------------------------------------------------------------------------------------
takeParam :: (n -> SRef) -> C.Param n -> SExp
takeParam takeRef mm
 = case mm of
        C.MType b       -> xAps "xm-typ" [takeBind takeRef b]
        C.MTerm b       -> xAps "xm-trm" [takeBind takeRef b]
        C.MImplicit b   -> xAps "xm-imp" [takeBind takeRef b]


-- Arg --------------------------------------------------------------------------------------------
takeArg :: (n -> SRef) -> C.Arg a n -> SExp
takeArg takeRef rr
 = case rr of
        C.RType t       -> xAps "xr-typ" [takeType    takeRef t]
        C.RTerm x       -> xAps "xr-trm" [takeExp     takeRef x]
        C.RWitness w    -> xAps "xr-wit" [takeWitness takeRef w]
        C.RImplicit r'  -> xAps "xr-imp" [takeArg     takeRef r']


-- Prim -------------------------------------------------------------------------------------------
takePrim :: C.Prim -> SExp
takePrim pp
 = case pp of
        C.PElaborate    -> xSym "xp-elab"
        C.PProject tx   -> xAps "xp-proj" [xSym tx]
        C.PShuffle      -> xSym "xp-shuf"
        C.PCombine      -> xSym "xp-comb"


-- Lets -------------------------------------------------------------------------------------------
takeLets :: (n -> SRef) -> C.Lets a n -> SExp
takeLets takeRef lts
 = case lts of
        C.LLet b x
         -> xAps "xl-let" [takeBind takeRef b, takeExp takeRef x]

        C.LRec bxs
         -> xAps "xl-rec" [ xPair (takeBind takeRef n) (takeExp takeRef x) | (n, x) <- bxs]

        C.LPrivate bsRgn mt bsWit
         -> xAps "xl-prv"
                 [ xList (map (takeBind takeRef) bsRgn)
                 , case mt of
                        Nothing -> xSym "none"
                        Just t  -> xAps "some" [takeType takeRef t]
                 , xList (map (takeBind takeRef) bsWit) ]


-- Alt --------------------------------------------------------------------------------------------
takeAlt :: (n -> SRef) -> C.Alt a n -> SExp
takeAlt takeRef aa
 = case aa of
        C.AAlt p x      -> xAps "xa-alt" [takePat takeRef p, takeExp takeRef x]


-- Pat --------------------------------------------------------------------------------------------
takePat :: (n -> SRef) -> C.Pat n -> SExp
takePat takeRef pp
 = case pp of
        C.PDefault       -> xSym "xt-def"
        C.PData dc bs    -> xAps "xt-dat" (takeDaCon takeRef dc : map (takeBind takeRef) bs)


-- DaCon ------------------------------------------------------------------------------------------
takeDaCon :: (n -> SRef) -> C.DaCon n (C.Type n) -> SExp
takeDaCon takeRef dc
 = case dc of
        C.DaConUnit      -> xSym "xc-unit"
        C.DaConRecord fs -> xAps "xc-rec" (map xSym fs)
        C.DaConPrim n t  -> xAps "xc-prm" [xRef $ takeRef n, takeType takeRef t]
        C.DaConBound n   -> xAps "xc-bnd" [xRef $ takeRef n]


-- Cast -------------------------------------------------------------------------------------------
takeCast :: (n -> SRef) -> C.Cast a n -> SExp
takeCast takeRef cc
 = case cc of
        C.CastWeakenEffect eff  -> xAps "xc-wea" [takeType takeRef eff]
        C.CastPurify wit        -> xAps "xc-pur" [takeWitness takeRef wit]
        C.CastBox               -> xSym "xc-box"
        C.CastRun               -> xSym "xc-run"


-- Witness ----------------------------------------------------------------------------------------
takeWitness :: (n -> SRef) -> C.Witness a n -> SExp
takeWitness takeRef ww
 = case ww of
        C.WVar _ u      -> xAps "w-var" [takeBound takeRef u]
        C.WCon _ wc     -> xAps "w-con" [takeWiCon takeRef wc]
        C.WApp _ w1 w2  -> xAps "w-app" [takeWitness takeRef w1, takeWitness takeRef w2]
        C.WType _ t     -> xAps "w-typ" [takeType  takeRef t]


-- WiCon ------------------------------------------------------------------------------------------
takeWiCon :: (n -> SRef) -> C.WiCon n -> SExp
takeWiCon takeRef wc
 = case wc of
        C.WiConBound b _ -> xApp (xSym "wc-bnd") [takeBound takeRef b]


-- Type -------------------------------------------------------------------------------------------
takeType :: (n -> SRef) -> C.Type n -> SExp
takeType takeRef tt
 = case tt of
        C.TCon tc       -> xAps "t-con" [takeTyCon takeRef tc]
        C.TVar u        -> xAps "t-var" [takeBound takeRef u]
        C.TAbs b t      -> xAps "t-abs" [takeBind  takeRef b, takeType takeRef t]
        C.TApp t1 t2    -> xAps "t-app" [takeType  takeRef t1, takeType takeRef t2]
        C.TForall b t   -> xAps "t-all" [takeBind  takeRef b, takeType takeRef t]
        C.TSum ts       -> xAps "t-sum" (map (takeType takeRef) $ Sum.toList ts)


-- Bind -------------------------------------------------------------------------------------------
takeBind  :: (n -> SRef) -> C.Bind n -> SExp
takeBind takeRef bb
 = case bb of
        C.BNone t       -> xAps "b-o" [takeType takeRef t]
        C.BAnon t       -> xAps "b-a" [takeType takeRef t]
        C.BName n t     -> xAps "b-n" [xRef $ takeRef n, takeType takeRef t]


-- Bound ------------------------------------------------------------------------------------------
takeBound :: (n -> SRef) -> C.Bound n -> SExp
takeBound takeRef uu
 = case uu of
        C.UIx i         -> xAps "u-i" [xNat i]
        C.UName n       -> xAps "u-n" [xRef $ takeRef n]
        C.UPrim n _t    -> xAps "u-p" [xRef $ takeRef n]


-- TyCon ------------------------------------------------------------------------------------------
takeTyCon :: (n -> SRef) -> C.TyCon n -> SExp
takeTyCon takeRef tc
 = case tc of
        C.TyConSort    c        -> takeSoCon c
        C.TyConKind    c        -> takeKiCon c
        C.TyConWitness c        -> takeTwCon c
        C.TyConSpec    c        -> takeTcCon c

        C.TyConBound   u k
         -> xApp (xSym "tc-bnd") [takeBound takeRef u, takeType takeRef k]

        C.TyConExists    i k
         -> xApp (xSym "tc-ext") [xNat i, takeType takeRef k]


takeSoCon :: C.SoCon -> SExp
takeSoCon c
 = case c of
        C.SoConProp             -> xSym "tc-prop"
        C.SoConComp             -> xSym "tc-comp"


takeKiCon :: C.KiCon -> SExp
takeKiCon c
 = case c of
        C.KiConFun              -> xSym "tc-funn"
        C.KiConWitness          -> xSym "tc-witn"
        C.KiConData             -> xSym "tc-data"
        C.KiConRegion           -> xSym "tc-rgnn"
        C.KiConEffect           -> xSym "tc-efft"
        C.KiConClosure          -> xSym "tc-clos"


takeTwCon :: C.TwCon -> SExp
takeTwCon c
 = case c of
        C.TwConImpl             -> xSym "tc-impl"
        C.TwConPure             -> xSym "tc-pure"
        C.TwConConst            -> xSym "tc-cnst"
        C.TwConDeepConst        -> xSym "tc-dcns"
        C.TwConMutable          -> xSym "tc-mutb"
        C.TwConDeepMutable      -> xSym "tc-dmut"
        C.TwConDistinct n       -> xAps "tc-dist" [xNat n]
        C.TwConDisjoint         -> xSym "tc-disj"


takeTcCon :: C.TcCon -> SExp
takeTcCon c
 = case c of
        C.TcConUnit             -> xSym "tc-unit"
        C.TcConFunExplicit      -> xSym "tc-fune"
        C.TcConFunImplicit      -> xSym "tc-funi"
        C.TcConSusp             -> xSym "tc-susp"
        C.TcConRecord ts        -> xAps "tc-recd" (map xSym ts)
        C.TcConRead             -> xSym "tc-read"
        C.TcConHeadRead         -> xSym "tc-hred"
        C.TcConDeepRead         -> xSym "tc-dred"
        C.TcConWrite            -> xSym "tc-writ"
        C.TcConDeepWrite        -> xSym "tc-dwri"
        C.TcConAlloc            -> xSym "tc-aloc"
        C.TcConDeepAlloc        -> xSym "tc-dalo"


-- Base -------------------------------------------------------------------------------------------
xRef :: SRef -> SExp
xRef r  = S.XRef r

xSym :: Text -> SExp
xSym tx = S.XRef (S.RSym tx)

xApp :: SExp -> [SExp] -> SExp
xApp x1 xs = S.XApp x1 xs

xAps :: Text -> [SExp] -> SExp
xAps t1 xs = S.XApp (S.XRef (S.RSym t1)) xs

xNat :: Int -> SExp
xNat i  = S.XRef (S.RPrm (S.PrimLitNat $ fromIntegral i))

xPair :: SExp -> SExp -> SExp
xPair x1 x2 = xAps "pair" [x1, x2]

xList :: [SExp] -> SExp
xList xs    = xAps "list" xs

