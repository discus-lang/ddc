{-# LANGUAGE OverloadedStrings #-}
module DDC.Core.Codec.Shimmer.Encode
        ( takeModuleDecls
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
import qualified DDC.Core.Module                as C
import qualified DDC.Core.Exp                   as C
import qualified DDC.Core.Exp.Annot.Compounds   as C
import qualified SMR.Core.Exp                   as S
import qualified SMR.Prim.Op.Base               as S
import qualified DDC.Type.Sum                   as Sum
import qualified Data.Text                      as T
import Data.Text                                (Text)


---------------------------------------------------------------------------------------------------
type SExp  = S.Exp  Text S.Prim
type SDecl = S.Decl Text S.Prim

-- TODO: pack vars into text as may use special chars.
-- the permitted Core chars are not the same as permitted Shimmer chars.

-- Module -----------------------------------------------------------------------------------------
takeModuleDecls :: (n -> SExp) -> C.Module a n -> [SDecl]
takeModuleDecls takeRef mm@C.ModuleCore{}
 = declsTop
 where
        declsTop
         | C.XLet _ (C.LRec bxs) _ <-  C.moduleBody mm
         = map takeDeclTop bxs

         | otherwise
         = error "takeModuleDecls: ill-formed module"

        takeDeclTop (C.BName n _, x)    -- TODO: pass through type.
         = case takeRef n of
                S.XApp _ [S.XRef (S.RSym tx)]
                  -> S.DeclMac tx (takeExp takeRef x)

                s -> error $ "takeDeclTop: unexpected takeRef " ++ show s

        takeDeclTop  _
         = error "takeModuleDecls: takeRef unexpected"

{-
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
-}

-- ModuleName -------------------------------------------------------------------------------------
takeModuleName :: C.ModuleName -> SExp
takeModuleName (C.ModuleName parts)
 = xAps "module-name" (map (xSym . T.pack) parts)


-- ExportSource------------------------------------------------------------------------------------
takeExportSource :: (n -> SExp) -> C.ExportSource n (C.Type n) -> SExp
takeExportSource takeRef es
 = case es of
        C.ExportSourceLocal n t
         -> xAps "exsrc-local" [takeRef n, takeType takeRef t]

        C.ExportSourceLocalNoType n
         -> xAps "exsrc-notyp" [takeRef n]


-- Exp --------------------------------------------------------------------------------------------
takeExp :: (n -> SExp) -> C.Exp a n -> SExp
takeExp takeRef xx
 = case xx of
        C.XPrim _ p     -> takePrim p
        C.XCon  _ dc    -> takeDaCon takeRef dc
        C.XVar  _ u     -> takeBound takeRef u

        C.XAbs{}
         -> let go acc (C.XAbs _ m     x)
                 = go (m : acc) x
                go acc x
                 = xAps "xb" [ xList (map (takeParam takeRef) (reverse acc))
                             , takeExp takeRef x ]
            in  go [] xx

        C.XApp{}
         -> let Just (x1, as) = C.takeXApps xx
            in  xAps "xa" (takeExp   takeRef x1 : map (takeArg takeRef) as)

        -- TODO: pack lets into here
        C.XLet  _ lts x -> xAps "xl" [takeLets  takeRef lts, takeExp takeRef x]

        C.XCase _ x as  -> xAps "xc" (takeExp   takeRef x : map (takeAlt takeRef) as)

        -- TODO: unpack cast into this.
        C.XCast _ c x   -> xAps "xt" [takeCast  takeRef c,  takeExp takeRef x]


-- Param ------------------------------------------------------------------------------------------
takeParam :: (n -> SExp) -> C.Param n -> SExp
takeParam takeRef mm
 = case mm of
        C.MType b       -> xAps "xmt" [takeBind takeRef b]
        C.MTerm b       -> xAps "xmm" [takeBind takeRef b]
        C.MImplicit b   -> xAps "xmi" [takeBind takeRef b]


-- Arg --------------------------------------------------------------------------------------------
takeArg :: (n -> SExp) -> C.Arg a n -> SExp
takeArg takeRef rr
 = case rr of
        C.RType t       -> takeType    takeRef t
        C.RTerm x       -> takeExp     takeRef x
        C.RWitness w    -> takeWitness takeRef w
        C.RImplicit r'  -> xAps "xri" [takeArg     takeRef r']


-- Prim -------------------------------------------------------------------------------------------
takePrim :: C.Prim -> SExp
takePrim pp
 = case pp of
        C.PElaborate    -> xSym "xpe"
        C.PProject tx   -> xAps "xpp" [xSym tx]
        C.PShuffle      -> xSym "xps"
        C.PCombine      -> xSym "xpc"


-- Lets -------------------------------------------------------------------------------------------
takeLets :: (n -> SExp) -> C.Lets a n -> SExp
takeLets takeRef lts
 = case lts of
        C.LLet b x
         -> xAps "xll" [takeBind takeRef b, takeExp takeRef x]

        C.LRec bxs
         -> xAps "xlr" [ xPair (takeBind takeRef n) (takeExp takeRef x) | (n, x) <- bxs]

        C.LPrivate bsRgn mt bsWit
         -> xAps "xlp"
                 [ xList (map (takeBind takeRef) bsRgn)
                 , case mt of
                        Nothing -> xSym "none"
                        Just t  -> xAps "some" [takeType takeRef t]
                 , xList (map (takeBind takeRef) bsWit) ]


-- Alt --------------------------------------------------------------------------------------------
takeAlt :: (n -> SExp) -> C.Alt a n -> SExp
takeAlt takeRef aa
 = case aa of
        C.AAlt p x      -> xAps "xaa" [takePat takeRef p, takeExp takeRef x]


-- Pat --------------------------------------------------------------------------------------------
takePat :: (n -> SExp) -> C.Pat n -> SExp
takePat takeRef pp
 = case pp of
        C.PDefault       -> xSym "xae"

        -- TODO: replicate takeDaCon into this.
        C.PData dc bs    -> xAps "xad" (takeDaCon takeRef dc : map (takeBind takeRef) bs)


-- DaCon ------------------------------------------------------------------------------------------
takeDaCon :: (n -> SExp) -> C.DaCon n (C.Type n) -> SExp
takeDaCon takeRef dc
 = case dc of
        C.DaConUnit      -> xSym "xcu"
        C.DaConRecord fs -> xAps "xcr" (map xSym fs)
        C.DaConPrim n t  -> xAps "xcp" [takeRef n, takeType takeRef t]
        C.DaConBound n   -> xAps "xcb" [takeRef n]


-- Cast -------------------------------------------------------------------------------------------
takeCast :: (n -> SExp) -> C.Cast a n -> SExp
takeCast takeRef cc
 = case cc of
        C.CastWeakenEffect eff
                        -> xAps "xtw" [takeType takeRef eff]
        C.CastPurify w  -> xAps "xtp" [takeWitness takeRef w]
        C.CastBox       -> xSym "xtb"
        C.CastRun       -> xSym "xtr"


-- Witness ----------------------------------------------------------------------------------------
takeWitness :: (n -> SExp) -> C.Witness a n -> SExp
takeWitness takeRef ww
 = case ww of
        C.WVar _ u      -> xAps "wv" [takeBound takeRef u]
        C.WCon _ wc     -> xAps "wc" [takeWiCon takeRef wc]
        C.WApp _ w1 w2  -> xAps "wa" [takeWitness takeRef w1, takeWitness takeRef w2]
        C.WType _ t     -> xAps "wt" [takeType  takeRef t]


-- WiCon ------------------------------------------------------------------------------------------
takeWiCon :: (n -> SExp) -> C.WiCon n -> SExp
takeWiCon takeRef wc
 = case wc of
        C.WiConBound b _ -> xApp (xSym "wb") [takeBound takeRef b]


-- Type -------------------------------------------------------------------------------------------
takeType :: (n -> SExp) -> C.Type n -> SExp
takeType takeRef tt
 = case tt of
        C.TCon tc       -> takeTyCon takeRef tc
        C.TVar u        -> takeBound takeRef u
        C.TAbs b t      -> xAps "tb" [takeBind  takeRef b, takeType takeRef t]

        C.TApp {}
         | ts <- C.takeTApps tt
         -> xAps "ta" (map (takeType takeRef) ts)

        C.TForall b t   -> xAps "tl" [takeBind  takeRef b, takeType takeRef t]

        C.TSum ts
         -> case Sum.toList ts of
                []      -> xSym "ts"
                ts'     -> xAps "ts" (map (takeType takeRef) ts')


-- Bind -------------------------------------------------------------------------------------------
takeBind  :: (n -> SExp) -> C.Bind n -> SExp
takeBind takeRef bb
 = case bb of
        C.BNone t       -> xAps "bo" [takeType takeRef t]
        C.BAnon t       -> xAps "ba" [takeType takeRef t]
        C.BName n t     -> xAps "bn" [takeRef n, takeType takeRef t]


-- Bound ------------------------------------------------------------------------------------------
takeBound :: (n -> SExp) -> C.Bound n -> SExp
takeBound takeRef uu
 = case uu of
        C.UIx i         -> xNat i
        C.UName n       -> takeRef n
        C.UPrim n _t    -> xAps "up" [takeRef n]


-- TyCon ------------------------------------------------------------------------------------------
takeTyCon :: (n -> SExp) -> C.TyCon n -> SExp
takeTyCon takeRef tc
 = case tc of
        C.TyConSort    c        -> takeSoCon c
        C.TyConKind    c        -> takeKiCon c
        C.TyConWitness c        -> takeTwCon c
        C.TyConSpec    c        -> takeTcCon c
        C.TyConBound   u _k     -> xApp (xSym "tc") [takeBound takeRef u]
        C.TyConExists  i k      -> xApp (xSym "ty") [xNat i, takeType takeRef k]


takeSoCon :: C.SoCon -> SExp
takeSoCon c
 = case c of
        C.SoConProp             -> xSym "TP"
        C.SoConComp             -> xSym "TO"


takeKiCon :: C.KiCon -> SExp
takeKiCon c
 = case c of
        C.KiConFun              -> xSym "TF"
        C.KiConWitness          -> xSym "TW"
        C.KiConData             -> xSym "TD"
        C.KiConRegion           -> xSym "TR"
        C.KiConEffect           -> xSym "TE"
        C.KiConClosure          -> xSym "TC"


takeTwCon :: C.TwCon -> SExp
takeTwCon c
 = case c of
        C.TwConImpl             -> xSym "Tl"
        C.TwConPure             -> xSym "Tp"
        C.TwConConst            -> xSym "Tc"
        C.TwConDeepConst        -> xSym "Td"
        C.TwConMutable          -> xSym "Tm"
        C.TwConDeepMutable      -> xSym "Tv"
        C.TwConDistinct n       -> xAps "Tt" [xNat n]
        C.TwConDisjoint         -> xSym "Tj"


takeTcCon :: C.TcCon -> SExp
takeTcCon c
 = case c of
        C.TcConUnit             -> xSym "Tu"
        C.TcConFunExplicit      -> xSym "Tx"
        C.TcConFunImplicit      -> xSym "Ti"
        C.TcConSusp             -> xSym "Ts"
        C.TcConRecord ts        -> xAps "To" (map xSym ts)
        C.TcConRead             -> xSym "Tr"
        C.TcConHeadRead         -> xSym "Th"
        C.TcConDeepRead         -> xSym "Te"
        C.TcConWrite            -> xSym "Tw"
        C.TcConDeepWrite        -> xSym "Tq"
        C.TcConAlloc            -> xSym "Ta"
        C.TcConDeepAlloc        -> xSym "Tb"


-- Base -------------------------------------------------------------------------------------------
xSym :: Text -> SExp
xSym tx = S.XRef (S.RSym tx)

xApp :: SExp -> [SExp] -> SExp
xApp x1 xs = S.XApp x1 xs

xAps :: Text -> [SExp] -> SExp
xAps t1 xs = S.XApp (S.XRef (S.RSym t1)) xs

xNat :: Int -> SExp
xNat i  = S.XRef (S.RPrm (S.PrimLitNat $ fromIntegral i))

xPair :: SExp -> SExp -> SExp
xPair x1 x2 = xAps "p" [x1, x2]

xList :: [SExp] -> SExp
xList xs    = xAps "l" xs

