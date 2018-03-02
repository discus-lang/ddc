{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms   #-}
module DDC.Core.Codec.Shimmer.Decode
        ( Config (..)
        , takeModuleDecls
        , takeTyCon)
where
import qualified DDC.Core.Module                as C
import qualified DDC.Core.Exp                   as C
import qualified DDC.Type.Exp.Simple.Compounds  as C
import qualified DDC.Core.Exp.Annot.Compounds   as C
import qualified DDC.Type.DataDef               as C
import qualified DDC.Type.Sum                   as Sum

import qualified SMR.Core.Exp                   as S
import qualified SMR.Prim.Name                  as S

import qualified Data.Text                      as T
import qualified System.IO.Unsafe               as System
import Data.IORef
import Data.Text                                (Text)
import Data.Maybe
-- import Data.Monoid
import qualified Data.Map.Strict                as Map
import Data.Map                                 (Map)
import Prelude hiding (read)

---------------------------------------------------------------------------------------------------
type SExp  = S.Exp  Text S.Prim
type SDecl = S.Decl Text S.Prim


-- | Config holding functions to extract various sorts of names from the tree.
data Config n
        = Config
        { configTakeRef         :: SExp -> Maybe n }

fromRef c ss
 = case configTakeRef c ss of
    Just r  -> r
    Nothing -> error "fromRef failed"

-- Module -----------------------------------------------------------------------------------------
takeModuleDecls :: (Ord n, Show n) => Config n -> [SDecl] -> Maybe (C.Module () n)
takeModuleDecls c decls
 = let  col = collectModuleDecls decls

        Just modName
         = case colName col of
                [S.DeclSet _ (XAps "l" ssParts)]
                  |  Just sParts <- sequence $ map takeXTxt ssParts
                  -> Just $ C.ModuleName $ map T.unpack sParts
                _ -> Nothing

        mpT     = Map.fromList [(tx, fromType c ss) | S.DeclMac tx ss <- colDsT col]
        mpD     = Map.fromList [(tx, ss)            | S.DeclMac tx ss <- colDsD col]

   in   Just $ C.ModuleCore
         { C.moduleName            = modName
         , C.moduleIsHeader        = False
         , C.moduleExportTypes     = []
         , C.moduleExportValues    = []
         , C.moduleImportTypes     = []
         , C.moduleImportDataDefs  = concatMap (takeDeclDat       c mpD)         $ colImDat col
         , C.moduleImportTypeDefs  = []
         , C.moduleImportCaps      = []
         , C.moduleImportValues    = concatMap (takeLocalTermDecl c modName mpT) $ colImTrm col
         , C.moduleDataDefsLocal   = concatMap (takeDeclDat       c mpD)         $ colLcDat col
         , C.moduleTypeDefsLocal   = []
         , C.moduleBody            = C.xUnit () }


-- Collect ----------------------------------------------------------------------------------------
-- | Collect the different top level declarations into separate bins
--   for each type of module.
---
--   We do this via IORefs because we're a bit worried about the cost of
--   reallocating the Collect wrapper for every declaration.
--   The resulting code is still clunky, and we probably want a better module representation.
--
collectModuleDecls :: [SDecl] -> Collect
collectModuleDecls decls
 = System.unsafePerformIO
 $ do   let new  = newIORef []
        let read = readIORef
        let rev  = reverse

        refName  <- new;
        refExTyp <- new; refExTrm <- new;
        refImTyp <- new; refImDat <- new; refImSyn <- new; refImCap <- new; refImTrm <- new
        refLcDat <- new; refLcSyn <- new
        refD     <- new; refS     <- new; refT     <- new; refX     <- new

        let eat (d@(S.DeclSet tx _ss) : ds)
             | T.isPrefixOf "m-name"   tx = do { modifyIORef' refName  (d :); eat ds }
             | T.isPrefixOf "m-ex-typ" tx = do { modifyIORef' refExTyp (d :); eat ds }
             | T.isPrefixOf "m-ex-trm" tx = do { modifyIORef' refExTrm (d :); eat ds }
             | T.isPrefixOf "m-im-typ" tx = do { modifyIORef' refImTyp (d :); eat ds }
             | T.isPrefixOf "m-im-dat" tx = do { modifyIORef' refImDat (d :); eat ds }
             | T.isPrefixOf "m-im-syn" tx = do { modifyIORef' refImSyn (d :); eat ds }
             | T.isPrefixOf "m-im-cap" tx = do { modifyIORef' refImCap (d :); eat ds }
             | T.isPrefixOf "m-im-trm" tx = do { modifyIORef' refImTrm (d :); eat ds }
             | T.isPrefixOf "m-lc-dat" tx = do { modifyIORef' refLcDat (d :); eat ds }
             | T.isPrefixOf "m-lc-syn" tx = do { modifyIORef' refLcSyn (d :); eat ds }
             | otherwise                  = error "ddc-core.collectModuleDecls: unexpected decl"

            eat (d@(S.DeclMac tx _ss) : ds)
             | T.isPrefixOf "d-"       tx = do { modifyIORef' refD     (d :); eat ds }
             | T.isPrefixOf "s-"       tx = do { modifyIORef' refS     (d :); eat ds }
             | T.isPrefixOf "t-"       tx = do { modifyIORef' refT     (d :); eat ds }
             | T.isPrefixOf "x-"       tx = do { modifyIORef' refX     (d :); eat ds }
             | otherwise                  = error "ddc-core.collectModuleDecls: unexpected decl"

            eat []                        = return ()

        eat decls

        dsName  <- read refName
        dsExTyp <- read refExTyp; dsExTrm <- read refExTrm
        dsImTyp <- read refImTyp; dsImDat <- read refImDat
        dsImSyn <- read refImSyn; dsImCap <- read refImCap; dsImTrm <- read refImTrm
        dsLcDat <- read refLcDat; dsLcSym <- read refLcSyn
        dsD     <- read refD;     dsS     <- read refT;     dsT     <- read refT
        dsX     <- read refX

        return  $ Collect
                { colName  = rev dsName
                , colExTyp = rev dsExTyp, colExTrm = rev dsExTrm
                , colImTyp = rev dsImTyp, colImDat = rev dsImDat
                , colImSyn = rev dsImSyn, colImCap = rev dsImCap, colImTrm = rev dsImTrm
                , colLcDat = rev dsLcDat, colLcSyn = rev dsLcSym
                , colDsD   = rev dsD,     colDsS   = rev dsS,     colDsT   = rev dsT
                , colDsX   = rev dsX }

data Collect
        = Collect
        { colName  :: [SDecl]
        , colExTyp :: [SDecl], colExTrm :: [SDecl]
        , colImTyp :: [SDecl], colImDat :: [SDecl]
        , colImSyn :: [SDecl], colImCap :: [SDecl], colImTrm :: [SDecl]
        , colLcDat :: [SDecl], colLcSyn :: [SDecl]
        , colDsD   :: [SDecl], colDsS   :: [SDecl], colDsT   :: [SDecl], colDsX :: [SDecl] }


-- DataDefs ---------------------------------------------------------------------------------------
takeDeclDat :: Ord n => Config n -> Map Text SExp -> SDecl -> [C.DataDef n]
takeDeclDat c mpD dd
 = case dd of
        S.DeclSet "m-lc-dat" (XList ssTypDat)
          -> map takeTypDat ssTypDat

        S.DeclSet "m-im-dat" (XList ssTypDat)
          -> map takeTypDat ssTypDat

        _ -> failDecode "takeDeclDat failed"

 where  takeTypDat  (XAps "typ-dat" [XTxt _nCon, XMac txMacDat])
         | Just ssDataDef <- Map.lookup txMacDat mpD
         = takeDataDef ssDataDef

        takeTypDat _ = failDecode "takeTypDat failed"

        takeDataDef (XAps "d-alg"   [ssCon, ssListParam, XNone])
         = C.DataDef
            { C.dataDefTypeName     = fromRef c ssCon
            , C.dataDefParams       = map (fromBind c)  $ fromList ssListParam
            , C.dataDefCtors        = Nothing
            , C.dataDefIsAlgebraic  = True }

        takeDataDef (XAps "d-alg"   [ssType, ssListParam, XSome ssCtors])
         | nType    <- fromRef c ssType
         , bsParam  <- map (fromBind c)  $ fromList ssListParam
         = C.DataDef
            { C.dataDefTypeName     = nType
            , C.dataDefParams       = bsParam
            , C.dataDefCtors        = Just $ map (takeDataCtor nType bsParam) $ fromList ssCtors
            , C.dataDefIsAlgebraic  = True }

        takeDataDef ss = failDecode $ "takeDataDef failed " ++ show ss

        takeDataCtor nType bsParam (XAps "ctor"  (XNat nTag : ssCtorName : ssRest))
         | (ssField, ssResult)  <- splitLast ssRest
         = C.DataCtor
            { C.dataCtorName        = fromRef c ssCtorName
            , C.dataCtorTag         = nTag
            , C.dataCtorFieldTypes  = map (fromType c) ssField
            , C.dataCtorResultType  = fromType c ssResult
            , C.dataCtorTypeName    = nType
            , C.dataCtorTypeParams  = bsParam }

        takeDataCtor _ _ _ = failDecode "takeDataCtor failed"


-- ImportValue ------------------------------------------------------------------------------------
takeLocalTermDecl
        :: Config n
        -> C.ModuleName -> Map Text (C.Type n)
        -> SDecl -> [(n, C.ImportValue n (C.Type n))]

takeLocalTermDecl c mn mpT dd
 = case dd of
        S.DeclSet "m-im-trm" (XList ssImTrm)
          -> map takeImTrm ssImTrm
        _ -> failDecode "takeImportValue failed"

 where  takeImTrm (XAps "im-trm-mod" [ssVar, XMac txMacType, XNat nT, XNat nX, XNat nB])
         | Just t <- Map.lookup txMacType mpT
         , Just n <- configTakeRef c (ssVar :: SExp)
         = (n, C.ImportValueModule
                { C.importValueModuleName  = mn
                , C.importValueModuleVar   = n
                , C.importValueModuleType  = t
                , C.importValueModuleArity = Just (fromI nT, fromI nX, fromI nB) })

        takeImTrm (XAps "im-trm-sea" [ssVar@(XTxt txVar), XMac txMacType])
         | Just t <- Map.lookup txMacType mpT
         , Just n <- configTakeRef c (ssVar :: SExp)
         = (n, C.ImportValueSea
                { C.importValueSeaVar      = T.unpack txVar
                , C.importValueSeaType     = t })

        takeImTrm _
         = failDecode "loadImTrm failed"


-- Type -------------------------------------------------------------------------------------------
fromType :: Ord n => Config n -> SExp -> C.Type n
fromType c ss
 = case ss of
        -- Applications of function type constructors.
        XAps "tf" ssParamResult
         -> let (ssParam, ssResult) = splitLast ssParamResult
                Just tf = takeTypeFun c ssParam (fromType c ssResult)
            in  tf

        -- Applications of a data type constructor.
        XAps "tu" (ssBound : ssArgs)
         -> C.tApps (C.TCon $ C.TyConBound (fromBound c ssBound) (C.tBot C.sComp))
                    (map (fromType c) ssArgs)

        -- Abstraction.
        XAps "tb" [ssBind, ssBody]
         -> C.TAbs (fromBind c ssBind) (fromType c ssBody)

        -- Application
        XAps "ta" ssArgs
         -> let (t1 : ts) = map (fromType c) ssArgs
            in  C.tApps t1 ts

        -- Forall
        XAps "tl" [ssBind, ssBody]
         -> C.TForall (fromBind c ssBind) (fromType c ssBody)

        -- Sum
        XAps "ts" (ssKind : ssArgs)
         -> C.TSum $ Sum.fromList (fromType c ssKind)
                   $ map (fromType c) ssArgs

          -- Con
        _ |  Just tc    <- takeTyCon c ss
          -> C.TCon tc

          -- Bound
          |  Just u     <- takeBound c ss
          -> C.TVar u

          | otherwise   -> failDecode "fromType failed"


takeTypeFun :: Ord n => Config n -> [SExp] -> C.Type n -> Maybe (C.Type n)
takeTypeFun c ssParam tResult
 = case ssParam of
        []      -> Just tResult

        -- Implicit function parameter.
        XAps "ni" [ssType] : ssParamRest
         |  Just tRest  <- takeTypeFun c ssParamRest tResult
         -> Just $ C.tApps (C.TCon $ C.TyConSpec C.TcConFunImplicit)
                        [fromType c ssType, tRest]

        -- Some other function constructor.
        XAps "nn" [ssTyCon, ssType] : ssParamRest
         |  Just tcFun  <- takeTyCon   c ssTyCon
         ,  Just tRest  <- takeTypeFun c ssParamRest tResult
         -> Just $ C.tApps (C.TCon tcFun) [fromType c ssType, tRest]

        -- Explicit function parameter.
        ssType : ssParamRest
         |  Just tRest  <- takeTypeFun c ssParamRest tResult
         -> Just $ C.tApps (C.TCon $ C.TyConSpec C.TcConFunExplicit)
                        [fromType c ssType, tRest]

        _ -> Nothing


-- Bind -------------------------------------------------------------------------------------------
fromBind :: Ord n => Config n -> SExp -> C.Bind n
fromBind c ss
 = fromMaybe (failDecode "fromBind failed")
 $ takeBind c ss

takeBind :: Ord n => Config n -> SExp -> Maybe (C.Bind n)
takeBind c ss
 = case ss of
        XAps "bo" [ssType]
         -> Just $ C.BNone $ fromType c ssType

        XAps "ba" [ssType]
         -> Just $ C.BAnon $ fromType c ssType

        XAps "bn" [ssRef, ssType]
         |  Just n      <- configTakeRef c ssRef
         -> Just $ C.BName n $ fromType c ssType

        _ -> Nothing


-- Bound ------------------------------------------------------------------------------------------
fromBound :: Ord n => Config n -> SExp -> C.Bound n
fromBound c ss
 = fromMaybe (failDecode "fromBound failed")
 $ takeBound c ss

takeBound :: Ord n => Config n -> SExp -> Maybe (C.Bound n)
takeBound c ss
 = case ss of
        XNat n
          -> Just $ C.UIx $ fromIntegral n

        XAps "up" [ssRef]
          | Just n       <- configTakeRef c ssRef
          -> Just $ C.UPrim n (C.tBot C.sComp)

        _ -> fmap C.UName $ configTakeRef c ss


-- TyCon ------------------------------------------------------------------------------------------
takeTyCon :: Ord n => Config n -> SExp -> Maybe (C.TyCon n)
takeTyCon c ss
 = case ss of

        -- TyConBound
        XApp (XSym "tcb") [ssBound]
         | Just u       <- takeBound c ssBound
         -> Just $ C.TyConBound u (C.tBot C.sComp)

        -- TyConExists
        XApp (XSym "tcy") [XNat n, ssType]
         -> Just $ C.TyConExists (fromIntegral n) $ fromType c ssType

        -- TyConSort
        XSym "tsp"      -> Just $ C.TyConSort    C.SoConProp
        XSym "tsc"      -> Just $ C.TyConSort    C.SoConComp

        -- TyConKind
        XSym "tkf"      -> Just $ C.TyConKind    C.KiConFun
        XSym "tkw"      -> Just $ C.TyConKind    C.KiConWitness
        XSym "tkd"      -> Just $ C.TyConKind    C.KiConData
        XSym "tkr"      -> Just $ C.TyConKind    C.KiConRegion
        XSym "tke"      -> Just $ C.TyConKind    C.KiConEffect
        XSym "tkc"      -> Just $ C.TyConKind    C.KiConClosure

        -- TyConWitness
        XSym "twl"      -> Just $ C.TyConWitness C.TwConImpl
        XSym "twp"      -> Just $ C.TyConWitness C.TwConPure
        XSym "twc"      -> Just $ C.TyConWitness C.TwConConst
        XSym "twd"      -> Just $ C.TyConWitness C.TwConDeepConst
        XSym "twm"      -> Just $ C.TyConWitness C.TwConMutable
        XSym "twv"      -> Just $ C.TyConWitness C.TwConDeepMutable
        XApp (XSym "twt") [XNat n]
                        -> Just $ C.TyConWitness $ C.TwConDistinct $ fromIntegral n
        XSym "twj"      -> Just $ C.TyConWitness $ C.TwConDisjoint

        -- TyConSpec
        XSym "tcu"      -> Just $ C.TyConSpec    C.TcConUnit
        XSym "tcf"      -> Just $ C.TyConSpec    C.TcConFunExplicit
        XSym "tci"      -> Just $ C.TyConSpec    C.TcConFunImplicit
        XSym "tcs"      -> Just $ C.TyConSpec    C.TcConSusp

        XApp (XSym "tco") sfs
         | Just ts <- sequence $ map takeXSym sfs
         -> Just $ C.TyConSpec $ C.TcConRecord ts

        XSym "tcr"      -> Just $ C.TyConSpec    C.TcConRead
        XSym "tch"      -> Just $ C.TyConSpec    C.TcConHeadRead
        XSym "tce"      -> Just $ C.TyConSpec    C.TcConDeepRead
        XSym "tcw"      -> Just $ C.TyConSpec    C.TcConWrite
        XSym "tcq"      -> Just $ C.TyConSpec    C.TcConDeepWrite
        XSym "tca"      -> Just $ C.TyConSpec    C.TcConAlloc
        XSym "tcb"      -> Just $ C.TyConSpec    C.TcConDeepAlloc

        -- Soz.
        _               -> Nothing


-- Utils ------------------------------------------------------------------------------------------
takeXSym :: SExp -> Maybe Text
takeXSym (XSym tx) = Just tx
takeXSym _         = Nothing

takeXTxt :: SExp -> Maybe Text
takeXTxt (XTxt tx) = Just tx
takeXTxt _         = Nothing

pattern XNone           = S.XRef (S.RSym "n")
pattern XSome x         = S.XApp (S.XRef (S.RSym "s")) [x]

pattern XNat n          = S.XRef (S.RPrm (S.PrimLitNat n))
pattern XList xs        = S.XApp (S.XRef (S.RSym "l")) xs

pattern XApp x1 xs      = S.XApp x1 xs
pattern XAps tx xs      = S.XApp (S.XRef (S.RSym tx)) xs
pattern XSym tx         = S.XRef (S.RSym tx)
pattern XMac tx         = S.XRef (S.RMac tx)
pattern XTxt tx         = S.XRef (S.RTxt tx)


splitLast :: [a] -> ([a], a)
splitLast xx
 = go [] xx
 where  go _   []       = failDecode "splitLast failed"
        go acc [x]      = (reverse acc, x)
        go acc (x : xs) = go (x : acc) xs

{-}
takeMaybe :: SExp -> Maybe (Maybe SExp)
takeMaybe ss
 = case ss of
    S.XApp (S.XRef (S.RSym "s")) [x] -> Just (Just x)
    S.XRef (S.RSym "n") -> Just Nothing
    _                   -> Nothing
-}
fromList  :: SExp -> [SExp]
fromList ss
 = case ss of
    S.XApp (S.XRef (S.RSym "l")) xs -> xs
    S.XRef (S.RSym "o") -> []
    _                   -> failDecode "takeList failed"

fromI = fromIntegral

failDecode str
 = error $ "ddc-core.Shimmer.Decode." ++ str
