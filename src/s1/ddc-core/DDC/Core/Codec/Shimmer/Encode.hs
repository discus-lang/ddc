{-# LANGUAGE OverloadedStrings #-}
module DDC.Core.Codec.Shimmer.Encode
        ( Config (..)
        , takeModuleDecls
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
import Data.Monoid

---------------------------------------------------------------------------------------------------
type SExp  = S.Exp  Text S.Prim
type SDecl = S.Decl Text S.Prim


data Config n
        = Config
        { configTakeRef         :: n -> SExp
        , configTakeVarName     :: n -> Maybe Text
        , configTakeConName     :: n -> Maybe Text }


-- Module -----------------------------------------------------------------------------------------
takeModuleDecls :: Config n -> C.Module a n -> [SDecl]
takeModuleDecls c mm@C.ModuleCore{}
 = declsTop
 where
        declsTop
         | C.XLet _ (C.LRec bxs) _ <-  C.moduleBody mm
         = concatMap takeDeclTop bxs

         | otherwise
         = error "takeModuleDecls: ill-formed module"

        takeDeclTop (C.BName n t, x)
         = case configTakeRef c n of
                S.XApp _ [S.XRef (S.RTxt tx)]
                  -> [ S.DeclMac (T.pack "t-" <> tx) (takeType c t)
                     , S.DeclMac (T.pack "x-" <> tx) (takeExp c x)  ]

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
takeExportSource :: Config n -> C.ExportSource n (C.Type n) -> SExp
takeExportSource c es
 = case es of
        C.ExportSourceLocal n t
         -> let Just tx = configTakeVarName c n
            in  xAps "exsrc-local" [S.XRef (S.RTxt tx), takeType c t]

        C.ExportSourceLocalNoType n
         -> let Just tx = configTakeVarName c n
            in  xAps "exsrc-notyp" [S.XRef (S.RTxt tx)]


-- Exp --------------------------------------------------------------------------------------------
takeExp :: Config n -> C.Exp a n -> SExp
takeExp c xx
 = case xx of
        C.XPrim _ p     -> takePrim p
        C.XCon  _ dc    -> takeDaCon c dc
        C.XVar  _ u     -> takeBound c u

        C.XAbs{}
         -> let go acc (C.XAbs _ m     x)
                 = go (m : acc) x
                go acc x
                 = xAps "xb" [ xList (map (takeParam c) (reverse acc))
                             , takeExp c x ]
            in  go [] xx

        C.XApp{}
         -> let Just (x1, as) = C.takeXApps xx
            in  xAps "xa" (takeExp   c x1 : map (takeArg c) as)

        -- TODO: pack lets into here
        C.XLet  _ lts x -> xAps "xl" [takeLets c lts, takeExp c x]

        C.XCase _ x as  -> xAps "xc" (takeExp  c x : map (takeAlt c) as)

        -- TODO: unpack cast into this.
        C.XCast _ ca x  -> xAps "xt" [takeCast c ca,  takeExp c x]


-- Param ------------------------------------------------------------------------------------------
takeParam :: Config n -> C.Param n -> SExp
takeParam c mm
 = case mm of
        C.MType b       -> xAps "xmt" [takeBind c b]
        C.MTerm b       -> xAps "xmm" [takeBind c b]
        C.MImplicit b   -> xAps "xmi" [takeBind c b]


-- Arg --------------------------------------------------------------------------------------------
takeArg :: Config n -> C.Arg a n -> SExp
takeArg c rr
 = case rr of
        C.RType t       -> takeType    c t
        C.RTerm x       -> takeExp     c x
        C.RWitness w    -> takeWitness c w
        C.RImplicit r'  -> xAps "xri" [takeArg c r']


-- Prim -------------------------------------------------------------------------------------------
takePrim :: C.Prim -> SExp
takePrim pp
 = case pp of
        C.PElaborate    -> xSym "xpe"
        C.PProject tx   -> xAps "xpp" [xSym tx]
        C.PShuffle      -> xSym "xps"
        C.PCombine      -> xSym "xpc"


-- Lets -------------------------------------------------------------------------------------------
takeLets :: Config n -> C.Lets a n -> SExp
takeLets c lts
 = case lts of
        C.LLet b x
         -> xAps "xll" [takeBind c b, takeExp c x]

        C.LRec bxs
         -> xAps "xlr" [ xPair (takeBind c n) (takeExp c x) | (n, x) <- bxs]

        C.LPrivate bsRgn mt bsWit
         -> xAps "xlp"
                 [ xList (map (takeBind c) bsRgn)
                 , case mt of
                        Nothing -> xSym "n"
                        Just t  -> xAps "s" [takeType c t]
                 , xList (map (takeBind c) bsWit) ]


-- Alt --------------------------------------------------------------------------------------------
takeAlt :: Config n -> C.Alt a n -> SExp
takeAlt c aa
 = case aa of
        C.AAlt p x      -> xAps "xaa" [takePat c p, takeExp c x]


-- Pat --------------------------------------------------------------------------------------------
takePat :: Config n -> C.Pat n -> SExp
takePat c pp
 = case pp of
        C.PDefault      -> xSym "xae"

        -- TODO: replicate takeDaCon into this.
        C.PData dc bs   -> xAps "xad" (takeDaCon c dc : map (takeBind c) bs)


-- DaCon ------------------------------------------------------------------------------------------
takeDaCon :: Config n -> C.DaCon n (C.Type n) -> SExp
takeDaCon c dc
 = case dc of
        C.DaConUnit      -> xSym "xcu"
        C.DaConRecord fs -> xAps "xcr" (map xSym fs)
        C.DaConPrim n t  -> xAps "xcp" [configTakeRef c n, takeType c t]
        C.DaConBound n   -> xAps "xcb" [configTakeRef c n]


-- Cast -------------------------------------------------------------------------------------------
takeCast :: Config n -> C.Cast a n -> SExp
takeCast c cc
 = case cc of
        C.CastWeakenEffect eff
                        -> xAps "xtw" [takeType c eff]
        C.CastPurify w  -> xAps "xtp" [takeWitness c w]
        C.CastBox       -> xSym "xtb"
        C.CastRun       -> xSym "xtr"


-- Witness ----------------------------------------------------------------------------------------
takeWitness :: Config n -> C.Witness a n -> SExp
takeWitness c ww
 = case ww of
        C.WVar _ u      -> xAps "wv" [takeBound c u]
        C.WCon _ wc     -> xAps "wc" [takeWiCon c wc]
        C.WApp _ w1 w2  -> xAps "wa" [takeWitness c w1, takeWitness c w2]
        C.WType _ t     -> xAps "wt" [takeType  c t]


-- WiCon ------------------------------------------------------------------------------------------
takeWiCon :: Config n -> C.WiCon n -> SExp
takeWiCon c wc
 = case wc of
        C.WiConBound b _ -> xApp (xSym "wb") [takeBound c b]


-- Type -------------------------------------------------------------------------------------------
takeType :: Config n -> C.Type n -> SExp
takeType c tt
 = case tt of
        C.TCon tc       -> takeTyCon c tc
        C.TVar u        -> takeBound c u
        C.TAbs b t      -> xAps "tb" [takeBind  c b, takeType c t]

        C.TApp {}
         | ts <- C.takeTApps tt
         -> xAps "ta" (map (takeType c) ts)

        C.TForall b t   -> xAps "tl" [takeBind c b, takeType c t]

        C.TSum ts
         -> case Sum.toList ts of
                []      -> xSym "ts"
                ts'     -> xAps "ts" (map (takeType c) ts')


-- Bind -------------------------------------------------------------------------------------------
takeBind  :: Config n -> C.Bind n -> SExp
takeBind c bb
 = case bb of
        C.BNone t       -> xAps "bo" [takeType c t]
        C.BAnon t       -> xAps "ba" [takeType c t]

        C.BName n t
         -> let Just tx = configTakeVarName c n
            in  xAps "bn" [xTxt tx, takeType c t]


-- Bound ------------------------------------------------------------------------------------------
takeBound :: Config n -> C.Bound n -> SExp
takeBound c uu
 = case uu of
        C.UIx i         -> xNat i

        C.UName n
         | Just tx   <- configTakeVarName c n -> xTxt tx
         | Just tx   <- configTakeConName c n -> xTxt tx
         | otherwise -> configTakeRef c n

        C.UPrim n _t    -> xAps "up" [configTakeRef c n]


-- TyCon ------------------------------------------------------------------------------------------
takeTyCon :: Config n -> C.TyCon n -> SExp
takeTyCon c tc
 = case tc of
        C.TyConSort    cn       -> takeSoCon cn
        C.TyConKind    cn       -> takeKiCon cn
        C.TyConWitness cn       -> takeTwCon cn
        C.TyConSpec    cn       -> takeTcCon cn
        C.TyConBound   u _k     -> xApp (xSym "tc") [takeBound c u]
        C.TyConExists  i k      -> xApp (xSym "ty") [xNat i, takeType c k]


takeSoCon :: C.SoCon -> SExp
takeSoCon c
 = case c of
        C.SoConProp             -> xSym "tsp"
        C.SoConComp             -> xSym "tsc"


takeKiCon :: C.KiCon -> SExp
takeKiCon c
 = case c of
        C.KiConFun              -> xSym "tkf"
        C.KiConWitness          -> xSym "tkw"
        C.KiConData             -> xSym "tkd"
        C.KiConRegion           -> xSym "tkr"
        C.KiConEffect           -> xSym "tke"
        C.KiConClosure          -> xSym "tkc"


takeTwCon :: C.TwCon -> SExp
takeTwCon c
 = case c of
        C.TwConImpl             -> xSym "twl"
        C.TwConPure             -> xSym "twp"
        C.TwConConst            -> xSym "twc"
        C.TwConDeepConst        -> xSym "twd"
        C.TwConMutable          -> xSym "twm"
        C.TwConDeepMutable      -> xSym "twv"
        C.TwConDistinct n       -> xAps "twt" [xNat n]
        C.TwConDisjoint         -> xSym "twj"


takeTcCon :: C.TcCon -> SExp
takeTcCon c
 = case c of
        C.TcConUnit             -> xSym "tcu"
        C.TcConFunExplicit      -> xSym "tcf"
        C.TcConFunImplicit      -> xSym "tci"
        C.TcConSusp             -> xSym "tcs"
        C.TcConRecord ts        -> xAps "tco" (map xSym ts)
        C.TcConRead             -> xSym "tcr"
        C.TcConHeadRead         -> xSym "tch"
        C.TcConDeepRead         -> xSym "tce"
        C.TcConWrite            -> xSym "tcw"
        C.TcConDeepWrite        -> xSym "tcq"
        C.TcConAlloc            -> xSym "tca"
        C.TcConDeepAlloc        -> xSym "tcb"


-- Base -------------------------------------------------------------------------------------------
xSym :: Text -> SExp
xSym tx = S.XRef (S.RSym tx)

xTxt :: Text -> SExp
xTxt tx = S.XRef (S.RTxt tx)

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

