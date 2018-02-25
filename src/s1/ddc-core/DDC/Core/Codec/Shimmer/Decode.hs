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
-- import qualified DDC.Type.DataDef               as C
import qualified DDC.Type.Sum                   as Sum

import qualified SMR.Core.Exp                   as S
import qualified SMR.Prim.Name                  as S

import qualified Data.Text                      as T
import qualified System.IO.Unsafe               as System
import Data.IORef
import Data.Text                                (Text)
-- import Data.Monoid
import Prelude hiding (read)

---------------------------------------------------------------------------------------------------
type SExp  = S.Exp  Text S.Prim
type SDecl = S.Decl Text S.Prim


-- | Config holding functions to extract various sorts of names from the tree.
data Config n
        = Config
        { configTakeRef         :: SExp -> Maybe n }


-- Module -----------------------------------------------------------------------------------------
takeModuleDecls :: Config n -> [SDecl] -> Maybe (C.Module () n)
takeModuleDecls _config decls
 = let  col = collectModuleDecls decls

        Just modName
         = case colName col of
                [S.DeclSet _ (XAps "l" ssParts)]
                  |  Just sParts <- sequence $ map takeXTxt ssParts
                  -> Just $ C.ModuleName $ map T.unpack sParts
                _ -> Nothing


   in   Just $ C.ModuleCore
         { C.moduleName                 = modName
         , C.moduleIsHeader             = False
         , C.moduleExportTypes          = []
         , C.moduleExportValues         = []
         , C.moduleImportTypes          = []
         , C.moduleImportDataDefs       = []
         , C.moduleImportTypeDefs       = []
         , C.moduleImportCaps           = []
         , C.moduleImportValues         = []
         , C.moduleDataDefsLocal        = []
         , C.moduleTypeDefsLocal        = []
         , C.moduleBody                 = C.xUnit () }


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
        dsImTyp <- read refImTyp; dsImDat <- read refImDat;
        dsImSyn <- read refImSyn; dsImCap <- read refImCap; dsImTrm <- read refImTrm
        dsLcDat <- read refLcDat; dsLcSym <- read refLcSyn

        return  $ Collect
                { colName  = rev dsName
                , colExTyp = rev dsExTyp, colExTrm = rev dsExTrm
                , colImTyp = rev dsImTyp, colImDat = rev dsImDat
                , colImSyn = rev dsImSyn, colImCap = rev dsImCap, colImTrm = rev dsImTrm
                , colLcDat = rev dsLcDat, colLcSyn = rev dsLcSym }

data Collect
        = Collect
        { colName  :: [SDecl]
        , colExTyp :: [SDecl], colExTrm :: [SDecl]
        , colImTyp :: [SDecl], colImDat :: [SDecl]
        , colImSyn :: [SDecl], colImCap :: [SDecl], colImTrm :: [SDecl]
        , colLcDat :: [SDecl], colLcSyn :: [SDecl] }


-- Type -------------------------------------------------------------------------------------------
takeType :: Ord n => Config n -> SExp -> Maybe (C.Type n)
takeType c ss
 = case ss of
        -- Applications of function type constructors.
        XAps "tf" ssParamResult
         |  Just (ssParam, ssResult)  <- splitLast ssParamResult
         ,  Just tResult              <- takeType c ssResult
         -> takeTypeFun c ssParam tResult

        -- Applications of a data type constructor.
        XAps "tu" (ssBound : ssArgs)
         |  Just u      <- takeBound c ssBound
         ,  Just tsArgs <- sequence $ map (takeType c) ssArgs
         -> Just $ C.tApps (C.TCon $ C.TyConBound u (C.tBot C.sComp)) tsArgs

        -- Abstraction.
        XAps "tb" [ssBind, ssBody]
         | Just b       <- takeBind c ssBind
         , Just t       <- takeType c ssBody
         -> Just $ C.TAbs b t

        -- Application
        XAps "ta" ssArgs
         | Just (t1 : ts) <- sequence $ map (takeType c) ssArgs
         -> Just $ C.tApps t1 ts

        -- Forall
        XAps "tl" [ssBind, ssBody]
         | Just b       <- takeBind c ssBind
         , Just t       <- takeType c ssBody
         -> Just $ C.TForall b t

        -- Sum
        XAps "ts" (ssKind : ssArgs)
         |  Just tKind  <- takeType c ssKind
         ,  Just tsArgs <- sequence $ map (takeType c) ssArgs
         -> Just $ C.TSum $ Sum.fromList tKind tsArgs

          -- Con
        _ |  Just tc    <- takeTyCon c ss
          -> Just $ C.TCon tc

          -- Bound
          |  Just u     <- takeBound c ss
          -> Just $ C.TVar u

          | otherwise   -> Nothing


takeTypeFun :: Ord n => Config n -> [SExp] -> C.Type n -> Maybe (C.Type n)
takeTypeFun c ssParam tResult
 = case ssParam of
        []      -> Just tResult

        -- Explicit function parameter.
        ssType : ssParamRest
         |  Just tParam <- takeType c ssType
         ,  Just tRest  <- takeTypeFun c ssParamRest tResult
         -> Just $ C.tApps (C.TCon $ C.TyConSpec C.TcConFunExplicit) [tParam, tRest]

        -- Implicit function parameter.
        XAps "ni" [ssType] : ssParamRest
         |  Just tParam <- takeType c ssType
         ,  Just tRest  <- takeTypeFun c ssParamRest tResult
         -> Just $ C.tApps (C.TCon $ C.TyConSpec C.TcConFunImplicit) [tParam, tRest]

        -- Some other function constructor.
        XAps "nn" [ssTyCon, ssType] : ssParamRest
         |  Just tcFun  <- takeTyCon   c ssTyCon
         ,  Just tParam <- takeType    c ssType
         ,  Just tRest  <- takeTypeFun c ssParamRest tResult
         -> Just $ C.tApps (C.TCon tcFun) [tParam, tRest]

        _ -> Nothing


-- Bind -------------------------------------------------------------------------------------------
takeBind :: Ord n => Config n -> SExp -> Maybe (C.Bind n)
takeBind c ss
 = case ss of
        XAps "bo" [ssType]
         |  Just t      <- takeType c ssType
         -> Just $ C.BNone t

        XAps "ba" [ssType]
         |  Just t      <- takeType c ssType
         -> Just $ C.BAnon t

        XAps "bn" [ssRef, ssType]
         |  Just n      <- configTakeRef c ssRef
         ,  Just t      <- takeType c ssType
         -> Just $ C.BName n t

        _ -> Nothing

-- Bound ------------------------------------------------------------------------------------------
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
         | Just k       <- takeType c ssType
         -> Just $ C.TyConExists (fromIntegral n) k

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

        -- TyConCpec
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

pattern XApp x1 xs      = S.XApp x1 xs
pattern XAps tx xs      = S.XApp (S.XRef (S.RSym tx)) xs
pattern XSym tx         = S.XRef (S.RSym tx)
pattern XTxt tx         = S.XRef (S.RTxt tx)
pattern XNat n          = S.XRef (S.RPrm (S.PrimLitNat n))

splitLast :: [a] -> Maybe ([a], a)
splitLast xx
 = go [] xx
 where  go _   []       = Nothing
        go acc [x]      = Just (reverse acc, x)
        go acc (x : xs) = go (x : acc) xs

