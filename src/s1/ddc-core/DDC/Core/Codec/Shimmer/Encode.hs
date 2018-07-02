
module DDC.Core.Codec.Shimmer.Encode
        ( Config (..)
        , storeInterface
        , takeModuleDecls
        , takeModuleName
        , takeExp,   takeParam,   takeArg
        , takeWitness, takeWiCon
        , takeType
        , takeBind,  takeBound
        , takeTyCon, takeSoCon,   takeKiCon, takeTwCon, takeTcCon)
where
import qualified DDC.Core.Interface.Base        as C
import qualified DDC.Core.Module                as C
import qualified DDC.Core.Exp                   as C
import qualified DDC.Core.Exp.Annot.Compounds   as C
import qualified DDC.Type.DataDef               as C
import qualified DDC.Type.Sum                   as Sum
import qualified SMR.Core.Codec                 as S
import qualified SMR.Core.Exp                   as S
import qualified SMR.Prim.Name                  as S
import qualified Data.Text                      as T
import Data.Text                                (Text)
import DDC.Data.Pretty

import qualified Foreign.Marshal.Alloc                  as Foreign
import qualified System.IO                              as System

---------------------------------------------------------------------------------------------------
type SExp  = S.Exp  Text S.Prim
type SDecl = S.Decl Text S.Prim


-- | Config holding functions to extract various sorts of names from the tree.
data Config n
        = Config
        { configTakeRef         :: n -> SExp
        , configTakeVarName     :: n -> Maybe Text
        , configTakeConName     :: n -> Maybe Text }


-- Interface --------------------------------------------------------------------------------------
-- | Store an interface at the given file path.
storeInterface :: Config n -> FilePath -> C.Interface n -> IO ()
storeInterface config pathDst ii
 = do
        let dsDecls = takeModuleDecls config $ C.interfaceModule ii
        let len     = S.sizeOfFileDecls dsDecls

        Foreign.allocaBytes len $ \pBuf
         -> do  _ <- S.pokeFileDecls dsDecls pBuf
                h <- System.openBinaryFile pathDst System.WriteMode
                System.hPutBuf h pBuf len
                System.hClose h
                return ()


-- Module -----------------------------------------------------------------------------------------
-- | Take shimmer declarations from a module.
--
--   The declarations are ordered so we get the sets of various symbols
--   up front, followed by separate bindings for their attached information.
--
takeModuleDecls :: Config n -> C.Module a n -> [SDecl]
takeModuleDecls c mm@C.ModuleCore{}
 =  [ dName,   dExTyp,  dExTrm
    , dImMod,  dImTyp,  dImDat, dImSyn, dImCap, dImTrm
    , dLcDat,  dLcSyn ]
 ++ concat
    [ dsImDat, dsImSyn, dsImTrm
    , dsLcDat, dsLcSyn, dsLcTrm ]
 where
        -- Module Name.
        dName
         = S.DeclSet "m-name" $ takeModuleName $ C.moduleName mm

        -- Exported Types.
        dExTyp
         = S.DeclSet "m-ex-typ"
         $ xList ([ xAps "ex-typ" [ configTakeRef c n
                                  , takeType c t]
                  | (n, C.ExportTypeLocal _ t) <- C.moduleExportTypes mm])

        -- Exported Values.
        dExTrm
         = S.DeclSet "m-ex-val"
         $ xList $ map (takeExportValue c) $ map snd $ C.moduleExportValues mm

        -- Imported Modules.
        dImMod
         = S.DeclSet "m-im-mod" $ xList xsImport
         where xsImport  = map takeModuleName  $ C.moduleImportModules mm

        -- Imported Types.
        dImTyp
         = S.DeclSet "m-im-typ" $ xList xsImport
         where xsImport  = map (takeImportType c) $ C.moduleImportTypes mm

        -- Imported Data Defs.
        (dImDat, dsImDat)
         = (S.DeclSet "m-im-dat" $ xList xsImport, dsImport)
         where  xdsImport = map (takeDataDef c) $ map snd $ C.moduleImportDataDefs mm
                xsImport  = map fst xdsImport
                dsImport  = concatMap snd xdsImport

        -- Imported Type Synonyms.
        (dImSyn, dsImSyn)
         = (S.DeclSet "m-im-syn"  $ xList xsImport, dsImport)
         where  xdsImport = map (takeTypeSyn c) $ C.moduleImportTypeDefs mm
                xsImport  = map fst xdsImport
                dsImport  = concatMap snd xdsImport

        -- Imported Capabilities.
        dImCap
         = (S.DeclSet "m-im-cap" $ xList xsImport)
         where  xsImport  = map (takeImportCap c) $ C.moduleImportCaps mm

        -- Imported Values.
        (dImTrm, dsImTrm)
         = (S.DeclSet "m-im-val" $ xList xsImport, dsImport)
         where  xdsImport = map (takeImportValue c) $ map snd $ C.moduleImportValues mm
                xsImport  = map fst xdsImport
                dsImport  = concatMap snd xdsImport

        -- Local Data Defs
        (dLcDat, dsLcDat)
         = (S.DeclSet "m-lc-dat" $ xList xsImport, dsImport)
         where  xdsImport = map (takeDataDef c) $ map snd $ C.moduleLocalDataDefs mm
                xsImport  = map fst xdsImport
                dsImport  = concatMap snd xdsImport

        -- Local Type Synonyms
        (dLcSyn, dsLcSyn)
         = (S.DeclSet "m-lc-syn" $ xList xsImport, dsImport)
         where  xdsImport = map (takeTypeSyn c) $ C.moduleLocalTypeDefs mm
                xsImport  = map fst xdsImport
                dsImport  = concatMap snd xdsImport

        -- Local Term Declarations.
        dsLcTrm
         | C.XLet _ (C.LRec bxs) _ <-  C.moduleBody mm
         = concatMap takeDeclTop bxs

         | otherwise
         = error "ddc-core.takeModuleDecls: ill-formed module"

        takeDeclTop (C.BName n t, x)
         = case configTakeRef c n of
                S.XApp _ [S.XRef (S.RTxt tx)]
                  -> [ S.DeclMac (text "t-" % tx) (takeType c t)
                     , S.DeclMac (text "x-" % tx) (takeExp  c x)  ]

                s -> error $ "ddc-core.takeDeclTop: unexpected takeRef " ++ show s

        takeDeclTop  _
         = error "ddc-core.takeModuleDecls: takeRef unexpected"


-- ModuleName -------------------------------------------------------------------------------------
takeModuleName :: C.ModuleName -> SExp
takeModuleName (C.ModuleName parts)
 = xAps "module-name" (map (xTxt . T.pack) parts)


-- ExportValue------------------------------------------------------------------------------------
takeExportValue :: Config n -> C.ExportValue n (C.Type n) -> SExp
takeExportValue c es
 = case es of
        C.ExportValueLocal mn n _ Nothing
         -> let Just tx = configTakeVarName c n
            in  xAps "ex-val-loc"
                        [ takeModuleName mn, xTxt tx, xMac ("t-" <> tx), xMac ("x-" <> tx)]

        C.ExportValueLocal mn n _ (Just (aT, aX, aB))
         -> let Just tx = configTakeVarName c n
            in  xAps "ex-val-loc"
                        [ takeModuleName mn, xTxt tx, xMac ("t-" <> tx), xMac ("x-" <> tx)
                        , xNat aT, xNat aX, xNat aB]

        C.ExportValueLocalNoType n
         -> let Just tx = configTakeVarName c n
            in  xAps "ex-val-loc"
                        [ xTxt tx, xMac ("t-" <> tx), xMac ("x-" <> tx)]

        -- TODO: split type into own decl so we can import/export via the same data.
        C.ExportValueSea mn n x t
         -> let Just tx = configTakeVarName c n
            in  xAps "ex-val-sea"
                        [ takeModuleName mn, xTxt tx, xTxt x, takeType c t ]


-- TypeSyn -----------------------------------------------------------------------------------------
takeTypeSyn :: Config n -> (n, (C.Kind n, C.Type n)) -> (SExp, [SDecl])
takeTypeSyn c (n, (k, t))
        = let Just tx = configTakeConName c n
          in  ( xAps "typ-syn" [xTxt tx, xMac ("s-" <> tx)]
              , [ S.DeclMac ("s-" <> tx) $ xAps "s-syn" [ takeType c k, takeType c t] ])


-- DataDef ----------------------------------------------------------------------------------------
takeDataDef :: Config n -> C.DataDef n -> (SExp, [SDecl])
takeDataDef c dd
 = case dd of
        C.DataDef modName n ps mCtors True
         -> let Just tx = configTakeConName c n
            in  ( xAps "typ-dat" [takeModuleName modName, xTxt tx, xMac ("d-" <> tx)]
                , [ S.DeclMac ("d-" <> tx)
                     $ xAps "d-alg"
                            [ takeModuleName modName, xTxt tx
                            , xList $ map (takeBind c) ps
                            , case mCtors of
                                Nothing    -> xNone
                                Just ctors -> xSome (xList $ map (takeDataCtor c) ctors) ]])

        C.DataDef modName n ps mCtors False
         -> let Just tx = configTakeConName c n
            in  ( xAps "typ-dat" [takeModuleName modName, xTxt tx, xMac ("d-" <> tx)]
                , [ S.DeclMac ("d-" <> tx)
                     $ xAps "d-nlg"
                            [ takeModuleName modName, xTxt tx
                            , xList $ map (takeBind c) ps
                            , case mCtors of
                                Nothing     -> xNone
                                Just ctors  -> xSome (xList $ map (takeDataCtor c) ctors) ]])

takeDataCtor :: Config n -> C.DataCtor n -> SExp
takeDataCtor c ctor
 = case ctor of
        C.DataCtor modName n tag tsParam tResult _ _
         -> let Just tx = configTakeConName c n
            in  xAps "ctor"
                 $  [xNat (fromIntegral tag), takeModuleName modName, xTxt tx]
                 ++ (map (takeType c) tsParam)
                 ++ [takeType c tResult]


-- ImportType -------------------------------------------------------------------------------------
takeImportType :: Config n -> (n, C.ImportType n (C.Type n)) -> SExp
takeImportType c (n, it)
 = case it of
        C.ImportTypeAbstract k
         -> let Just tx = configTakeConName c n
            in  xAps "im-typ-abs" [xTxt tx, takeType c k]

        C.ImportTypeBoxed k
         -> let Just tx = configTakeConName c n
            in  xAps "im-typ-box" [xTxt tx, takeType c k]


-- ImportCap --------------------------------------------------------------------------------------
takeImportCap   :: Config n -> (n, C.ImportCap n (C.Type n)) -> SExp
takeImportCap c (n, ic)
 = case ic of
        C.ImportCapAbstract k
         -> let Just tx = configTakeVarName c n
            in  xAps "im-cap-abs"  [xTxt tx, takeType c k]


-- ImportValue ------------------------------------------------------------------------------------
takeImportValue :: Config n -> C.ImportValue n (C.Type n) -> (SExp, [SDecl])
takeImportValue c iv
 = case iv of
        C.ImportValueModule{}
         -> let Just tx = configTakeVarName c (C.importValueModuleVar iv)
                mn      = C.importValueModuleName iv
            in  ( xAps "im-val-mod"
                        $ [ takeModuleName mn
                          , xTxt tx
                          , xMac ("t-" <> tx)]
                       ++ case C.importValueModuleArity iv of
                              Nothing           -> []
                              Just (nt, nx, nb) -> [xNat nt, xNat nx, xNat nb]
                 , [S.DeclMac ("t-" <> tx) (takeType c $ C.importValueModuleType iv)])

        C.ImportValueSea{}
         -> let Just tx = configTakeVarName c (C.importValueSeaNameInternal iv)
            in  ( xAps "im-val-sea"
                        [ takeModuleName $ C.importValueSeaModuleName iv
                        , xTxt tx
                        , xTxt (C.importValueSeaNameExternal iv)
                        , xMac ("t-" <> tx)]
                , [S.DeclMac ("t-" <> tx) (takeType c $ C.importValueSeaType iv)])


-- Exp --------------------------------------------------------------------------------------------
takeExp :: Config n -> C.Exp a n -> SExp
takeExp c xx
 = case xx of
        -- Var ------
        C.XVar  _ u
         -> takeBound c u

        -- Prim -----
        C.XPrim _ p
         -> case p of
                C.PElaborate     -> xSym "xpe"
                C.PProject tx    -> xAps "xpp" [xSym tx]
                C.PShuffle       -> xSym "xps"
                C.PCombine       -> xSym "xpc"

        -- Con ----
        C.XCon  _ dc
         -> case dc of
                C.DaConUnit      -> xSym "xdu"
                C.DaConRecord fs -> xAps "xdr" (map xSym fs)
                C.DaConPrim n    -> configTakeRef c n
                C.DaConBound n   -> takeDaConBoundName c n

        -- Abs -----
        C.XAbs{}
         -> let go acc (C.XAbs _ m     x)
                 = go (m : acc) x

                go acc x
                 = xAps "xb" (map (takeParam c) (reverse acc) ++ [takeExp c x])
            in  go [] xx

        -- App -----
        C.XApp{}
         -> let Just (x1, as) = C.takeXApps xx
            in  xAps "xa" (takeExp   c x1 : map (takeArg c) as)

        -- Let -----
        C.XLet _ (C.LLet b x1) x2
         -> xAps "xll"  [takeBind c b, takeExp c x1, takeExp c x2]

        C.XLet _ (C.LRec bxs)  x2
         -> xAps "xlr"  (  [xPair (takeBind c n) (takeExp c x) | (n, x) <- bxs]
                        ++ [takeExp c x2])

        C.XLet _ (C.LPrivate [bRgn] mt bsWit) x2
         -> xAps "xlp"
                 ( [ takeBind c bRgn
                   , case mt of
                        Nothing -> xNone
                        Just t  -> xSome (takeType c t)]
                 ++ (map (takeBind c) bsWit)
                 ++ [takeExp c x2])

        C.XLet _ (C.LPrivate bsRgn mt bsWit) x2
         -> xAps "xlp"
                 ( [ xList (map (takeBind c) bsRgn)
                   , case mt of
                        Nothing -> xNone
                        Just t  -> xSome (takeType c t)]
                 ++ (map (takeBind c) bsWit)
                 ++ [takeExp c x2])

        -- Case -----
        C.XCase _ x as
         -> xAps "xc" (takeExp  c x : map (takeAlt c) as)

        -- Cast -----
        C.XCast _ cc x
         -> case cc of
                C.CastWeakenEffect eff
                                -> xAps "xcw" [takeType c eff,  takeExp c x]
                C.CastPurify w  -> xAps "xcp" [takeWitness c w, takeExp c x]
                C.CastBox       -> xAps "xcb" [takeExp c x]
                C.CastRun       -> xAps "xcr" [takeExp c x]


-- Param ------------------------------------------------------------------------------------------
takeParam :: Config n -> C.Param n -> SExp
takeParam c mm
 = case mm of
        -- Type
        C.MType (C.BNone t)     -> xAps "mto" [takeType c t]
        C.MType (C.BAnon t)     -> xAps "mta" [takeType c t]
        C.MType (C.BName n t)
         -> let Just tx = configTakeVarName c n
            in  xAps "mtn" [xTxt tx, takeType c t]

        -- Term
        C.MTerm (C.BNone t)     -> xAps "mxo" [takeType c t]
        C.MTerm (C.BAnon t)     -> xAps "mxa" [takeType c t]
        C.MTerm (C.BName n t)
         -> let Just tx = configTakeVarName c n
            in  xAps "mxn" [xTxt tx, takeType c t]

        -- Implicit
        C.MImplicit (C.BNone t) -> xAps "mio" [takeType c t]
        C.MImplicit (C.BAnon t) -> xAps "mia" [takeType c t]
        C.MImplicit (C.BName n t)
         -> let Just tx = configTakeVarName c n
            in  xAps "min" [xTxt tx, takeType c t]


-- Arg --------------------------------------------------------------------------------------------
takeArg :: Config n -> C.Arg a n -> SExp
takeArg c rr
 = case rr of
        C.RTerm x       -> takeExp     c x
        C.RType t       -> xAps "rt" [takeType    c t]
        C.RWitness w    -> xAps "rw" [takeWitness c w]
        C.RImplicit r'  -> xAps "ri" [takeArg c r']


-- Alt --------------------------------------------------------------------------------------------
takeAlt :: Config n -> C.Alt a n -> SExp
takeAlt c aa
 = case aa of
        C.AAlt C.PDefault x
         -> xAps "ae" [takeExp c x]

        -- Unit
        C.AAlt (C.PData C.DaConUnit []) x
         -> xAps "au" [takeExp c x]

        C.AAlt (C.PData C.DaConUnit bs) x
         -> xAps "au" (takeExp c x : map (takeBind c) bs)

        -- Record
        C.AAlt (C.PData (C.DaConRecord fs) []) x
         -> xAps "ar" (map xSym fs ++ [takeExp c x])

        C.AAlt (C.PData (C.DaConRecord fs) bs) x
         -> xAps "ar" ((xList (map xSym fs) : map (takeBind c) bs) ++ [takeExp c x])

        -- Prim
        C.AAlt (C.PData (C.DaConPrim n) []) x
         -> xAps "ap" [configTakeRef c n, takeExp c x]

        C.AAlt (C.PData (C.DaConPrim n) bs) x
         -> xAps "ap" ((configTakeRef c n  : map (takeBind c) bs) ++ [takeExp c x])

        -- Bound
        C.AAlt (C.PData (C.DaConBound n) []) x
         -> xAps "ab"   [ takeDaConBoundName c n
                        , takeExp c x]

        C.AAlt (C.PData (C.DaConBound n) bs) x
         -> xAps "ab"   ((takeDaConBoundName c n : map (takeBind c) bs) ++ [takeExp c x])


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
 -- Flatten out applications of the function type constructor.
 | (tcsParam, tResult) <- C.takeTFunCons tt
 , not $ null tcsParam
 = let  takeFunParam (C.TcConFunExplicit, t) = takeType c t
        takeFunParam (C.TcConFunImplicit, t) = xAps "ni" [takeType c t]
        takeFunParam (tc,                 t) = xAps "nn" [takeTcCon tc, takeType c t]
   in   xAps "tf" (map takeFunParam tcsParam ++ [takeType c tResult])

 -- Flatten out applications of data type constructor.
 | Just (tc, tsArg)    <- C.takeTyConApps tt
 , C.TyConBound u _ <- tc
 = xAps "tu" (takeBound c u : map (takeType c) tsArg)

 -- Some other type.
 | otherwise
 = case tt of
        C.TAbs b t      -> xAps "tb" [takeBind  c b, takeType c t]

        C.TApp {}
         | ts <- C.takeTApps tt
         -> xAps "ta" (map (takeType c) ts)

        C.TForall b t   -> xAps "tl" [takeBind c b, takeType c t]

        C.TSum ts       -> xAps "ts" ( takeType c (Sum.kindOfSum ts)
                                     : (map (takeType c) $ Sum.toList ts))

        C.TCon tc       -> takeTyCon c tc
        C.TVar u        -> takeBound c u


-- DaConBoundName ---------------------------------------------------------------------------------
takeDaConBoundName :: Config n -> C.DaConBoundName n -> SExp
takeDaConBoundName c (C.DaConBoundName mnModule mnType nCtor)
 = xAps "dcbn"
        [ xMaybe xModuleName mnModule
        , xMaybe (configTakeRef c) mnType
        , configTakeRef c nCtor ]


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


-- TyCon ------------------------------------------------------------------------------------------
takeTyCon :: Config n -> C.TyCon n -> SExp
takeTyCon c tc
 = case tc of
        C.TyConSort    cn       -> takeSoCon cn
        C.TyConKind    cn       -> takeKiCon cn
        C.TyConWitness cn       -> takeTwCon cn
        C.TyConSpec    cn       -> takeTcCon cn
        C.TyConBound   u _k     -> xApp (xSym "tcb") [takeBound c u]
        C.TyConExists  i k      -> xApp (xSym "tcy") [xNat i, takeType c k]


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


-- Utils ------------------------------------------------------------------------------------------
xSym :: Text -> SExp
xSym tx = S.XRef (S.RSym tx)

xMac :: Text -> SExp
xMac tx = S.XRef (S.RMac tx)

xTxt :: Text -> SExp
xTxt tx = S.XRef (S.RTxt tx)

xApp :: SExp -> [SExp] -> SExp
xApp x1 xs = S.XApp x1 xs

xAps :: Text -> [SExp] -> SExp
xAps t1 xs = S.XApp (S.XRef (S.RSym t1)) xs

xNat :: Int -> SExp
xNat i  = S.XRef (S.RPrm (S.PrimLitNat $ fromIntegral i))

xNone :: SExp
xNone   = xSym "n"

xSome :: SExp -> SExp
xSome x = xAps "s" [x]

xMaybe :: (a -> SExp) -> Maybe a -> SExp
xMaybe _ Nothing  = xNone
xMaybe f (Just x) = xSome (f x)

xPair :: SExp -> SExp -> SExp
xPair x1 x2 = xAps "p" [x1, x2]

xList :: [SExp] -> SExp
xList [] = xSym "o"
xList xs = xAps "l" xs

xModuleName :: C.ModuleName -> SExp
xModuleName (C.ModuleName sParts)
        = xList $ map (xTxt . T.pack) sParts


