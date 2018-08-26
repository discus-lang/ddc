
-- | Source Discus conversion to Disciple Core Discus language.
module DDC.Source.Discus.Convert
        ( ConvertM
        , ErrorConvert (..)
        , coreOfSourceModule
        , runConvertM)
where
import DDC.Source.Discus.Convert.Error
import DDC.Source.Discus.Convert.Witness
import DDC.Source.Discus.Convert.Clause
import DDC.Source.Discus.Convert.Type
import DDC.Source.Discus.Convert.Prim
import DDC.Source.Discus.Convert.Base

import qualified DDC.Source.Discus.Module       as S
import qualified DDC.Source.Discus.Exp          as S

import qualified DDC.Core.Discus.Compounds      as C
import qualified DDC.Core.Discus.Prim           as C
import qualified DDC.Core.Module                as C
import qualified DDC.Type.DataDef               as C
import qualified Data.Text                      as Text
import qualified Data.Set                       as Set
import Data.Maybe

import DDC.Data.Label

import DDC.Core.Module
        ( ExportType    (..)
        , ExportValue   (..)
        , ImportType    (..)
        , ImportCap     (..)
        , ImportValue   (..))


---------------------------------------------------------------------------------------------------
-- | Run a conversion computation.
runConvertM :: ConvertM a x -> Either (ErrorConvert a) x
runConvertM cc = cc

-- | Convert a Source Discus module to Core Discus.
coreOfSourceModule
        :: SP
        -> S.Module SP
        -> Either (ErrorConvert SP) (C.Module SP C.Name)

coreOfSourceModule a mm
        = runConvertM
        $ coreOfSourceModuleM a mm


-- Module -----------------------------------------------------------------------------------------
-- | Convert a Source Discus module to Core Discus.
--
--   The Source code needs to already have been desugared and cannot contain,
--   and `XDefix`, `XInfixOp`, or `XInfixVar` nodes, else `error`.
--
--   We use the map of core headers to add imports for all the names that this
--   module uses from its environment.
--
coreOfSourceModuleM
        :: SP
        -> S.Module SP
        -> ConvertM SP (C.Module SP C.Name)

coreOfSourceModuleM a mm
 = do
        -- Exported types.
        exportTypes'
         <- sequence
         $  fmap (\n -> (,) <$> toCoreTUCN n
                            <*> (fmap ExportTypeLocalNoKind $ toCoreTUCN n))
         $  S.moduleExportTypes mm


        -- Exported values.
        --  Auto-export the 'main' binding if it isn't already.
        exportValues'
         <- sequence
         $  fmap (\(n, ev) -> (,) <$> toCoreXUVN n <*> toCoreExportValue ev)
         $  S.moduleExportValues mm

        let exportValues_main
                | C.isMainModuleName (S.moduleName mm)
                , not   $ elem (S.UName (Text.pack "main"))
                        $ map fst $ S.moduleExportValues mm
                =  exportValues'
                ++ [ ( C.NameVar "main"
                     , ExportValueLocalNoType (C.NameVar "main"))]

                | otherwise
                = exportValues'


        -- Imported types, capabilities and values.
        importTypes'
         <- sequence
         $  fmap (\(n, it) -> (,) <$> toCoreTBCN n <*> toCoreImportType it)
         $  S.moduleImportTypes  mm

        importCaps'
         <- sequence
         $  fmap (\(n, iv) -> (,) <$> toCoreXBVN n <*> toCoreImportCap   iv)
         $  S.moduleImportCaps   mm

        importValues'
         <- sequence
         $  fmap (\(n, iv) -> (,) <$> toCoreXBVN n <*> toCoreImportValue iv)
         $  S.moduleImportValues mm

        -- Data type definitions.
        dataDefsLocal
         <- sequence $ fmap (toCoreDataDef (S.moduleName mm))
         $  [ def    | S.TopData _ def <- S.moduleTops mm ]

        -- Type equations.
        typeDefsLocal
         <- sequence $ fmap toCoreTypeDef
         $  [ (b, t) | S.TopType _ b t <- S.moduleTops mm ]

        -- Top level bindings.
        ltsTops
         <- letsOfTops $ S.moduleTops mm

        return
         $ C.ModuleCore
         { C.moduleName           = S.moduleName mm
         , C.moduleTransitiveDeps = Set.empty
         , C.moduleIsHeader       = False
         , C.moduleExportTypes    = exportTypes'
         , C.moduleExportValues   = exportValues_main
         , C.moduleImportModules  = S.moduleImportModules mm
         , C.moduleImportTypes    = importTypes'
         , C.moduleImportCaps     = importCaps'
         , C.moduleImportValues   = importValues'
         , C.moduleImportDataDefs = []
         , C.moduleImportTypeDefs = []
         , C.moduleLocalDataDefs  = dataDefsLocal
         , C.moduleLocalTypeDefs  = typeDefsLocal
         , C.moduleBody           = C.XLet  a ltsTops (C.xUnit a) }


-- Tops -------------------------------------------------------------------------------------------
-- | Extract the top-level bindings from some source definitions.
letsOfTops
        :: [S.Top SP]   -- ^ Top-level clauses to convert.
        -> ConvertM SP (C.Lets SP C.Name)

letsOfTops tops
 = do
        -- Collect up the type signatures defined at top level.
        let cls         = [cl   | S.TopClause _ cl      <- tops]
        let sigs        = collectSigsFromClauses      cls
        let vals        = collectBoundVarsFromClauses cls

        bxps    <- fmap catMaybes
                $  mapM (makeBindingFromClause sigs vals) cls

        let (bms, xps)  =  unzip bxps
        bs'             <- mapM (toCoreBM UniverseSpec) bms
        xs'             <- mapM (\(sp, x) -> toCoreX sp x) xps
        return $ C.LRec $ zip bs' xs'


-- ExportValue ------------------------------------------------------------------------------------
toCoreExportValue
        :: ExportValue S.Bound S.Type
        -> ConvertM a (ExportValue C.Name (C.Type C.Name))

toCoreExportValue ev
 = case ev of
        ExportValueLocalNoType n
         -> ExportValueLocalNoType <$> toCoreXUVN n

        ExportValueLocal mn n t mArity
         -> ExportValueLocal mn    <$> toCoreXUVN n <*> toCoreT UniverseSpec t <*> pure mArity

        ExportValueSea mn n tx t
         -> ExportValueSea mn      <$> toCoreXUVN n <*> pure tx <*> toCoreT UniverseSpec t


-- ImportType -------------------------------------------------------------------------------------
toCoreImportType
        :: ImportType n S.Type
        -> ConvertM a (ImportType C.Name (C.Type C.Name))

toCoreImportType src
 = case src of
        ImportTypeAbstract t
         -> ImportTypeAbstract <$> toCoreT UniverseKind t

        ImportTypeBoxed t
         -> ImportTypeBoxed    <$> toCoreT UniverseKind t


-- ImportCap --------------------------------------------------------------------------------------
toCoreImportCap
        :: ImportCap S.Bind S.Type
        -> ConvertM a (ImportCap C.Name (C.Type C.Name))

toCoreImportCap src
 = case src of
        ImportCapAbstract t
         -> ImportCapAbstract   <$> toCoreT UniverseSpec t


-- ImportValue ------------------------------------------------------------------------------------
toCoreImportValue
        :: ImportValue S.Bind S.Type
        -> ConvertM a (ImportValue C.Name (C.Type C.Name))

toCoreImportValue src
 = case src of
        ImportValueModule mn n t mA
         ->  ImportValueModule
         <$> pure mn    <*> toCoreXBVN n
                        <*> toCoreT UniverseSpec t
                        <*> pure mA

        ImportValueSea mn n v t
         -> ImportValueSea
         <$> pure mn    <*> toCoreXBVN n
                        <*> pure v
                        <*> toCoreT UniverseSpec t


-- DataDef ----------------------------------------------------------------------------------------
toCoreDataDef :: S.ModuleName -> S.DataDef SP -> ConvertM a (C.Name, C.DataDef C.Name)
toCoreDataDef modName def
 = do
        defParams
         <- sequence
                $ fmap toCoreTBK
                $ S.dataDefParams def

        defCtors
         <- sequence
                $ fmap (\(ctor, tag) -> toCoreDataCtor modName def tag ctor)
                $ [(ctor, tag) | ctor <- S.dataDefCtors def
                               | tag  <- [0..]]

        let (S.TyConBindName txTyConName) = S.dataDefTypeName def
        let nType       = C.NameCon txTyConName

        return  ( nType
                , C.DataDef
                  { C.dataDefModuleName  = modName
                  , C.dataDefTypeName    = C.NameCon txTyConName
                  , C.dataDefParams      = defParams
                  , C.dataDefCtors       = Just $ defCtors
                  , C.dataDefIsAlgebraic = True })


-- DataCtor ---------------------------------------------------------------------------------------
toCoreDataCtor
        :: S.ModuleName
        -> S.DataDef  SP
        -> Integer
        -> S.DataCtor SP
        -> ConvertM a (C.DataCtor C.Name)

toCoreDataCtor modName dataDef tag ctor
 = do   typeParams      <- sequence $ fmap toCoreTBK $ S.dataDefParams dataDef
        fieldTypes      <- sequence $ fmap (toCoreT UniverseSpec)   $ S.dataCtorFieldTypes ctor
        resultType      <- toCoreT UniverseSpec (S.dataCtorResultType ctor)
        let (S.TyConBindName txTyConName) = S.dataDefTypeName dataDef

        return $ C.DataCtor
         { C.dataCtorModuleName  = modName
         , C.dataCtorName        = toCoreDaConBind (S.dataCtorName ctor)
         , C.dataCtorTag         = tag
         , C.dataCtorFieldTypes  = fieldTypes
         , C.dataCtorResultType  = resultType
         , C.dataCtorTypeName    = C.NameCon txTyConName
         , C.dataCtorTypeParams  = typeParams }


-- Exp --------------------------------------------------------------------------------------------
toCoreX :: SP -> S.Exp -> ConvertM SP (C.Exp SP C.Name)
toCoreX a xx
 = case xx of
        S.XAnnot a' x
         -> toCoreX a' x

        S.XPrim S.PrimValElaborate
         -> return $ C.XPrim a C.PElaborate

        S.XPrim (S.PrimValProject n)
         -> return $ C.XApp a (C.XAtom a (C.MAPrim C.PProject))
                              (C.RTerm (C.XAtom a (C.MALabel (labelOfText n))))

        S.XPrim p
         -> case toCorePrimVal p of
                Just p' -> return $ C.XVar a (C.UName p')
                _       -> error "ddc-source-discus: cannot convert prim"

        S.XVar u
         -> C.XVar <$> pure a <*> toCoreU u

        -- Wrap text literals into Text during conversion to Core.
        -- The 'textLit' variable refers to whatever is in scope.
        S.XCon dc@(C.DaConBound (C.DaConBoundName _ _ (S.DaConBoundLit (S.PrimLitTextLit{}))))
         -> C.XApp <$> pure a
                   <*> (C.XVar  <$> pure a <*> (pure $ C.UName (C.NameVar "textLit")))
                   <*> (C.RTerm <$> (C.XCon <$> pure a <*> (toCoreDC dc)))

        S.XCon  dc
         -> C.XCon <$> pure a <*> toCoreDC dc

        S.XAbs  p x
         -> C.XAbs <$> pure a <*> toCoreParam p  <*> toCoreX a x

        -- We don't want to wrap the source file path passed to the default# prim
        -- in a Text constructor, so detect this case separately.
        S.XApp  _ _
         |  Just ( S.XPrim p@(S.PrimValError S.OpErrorDefault)
                 , [S.RTerm (S.XCon dc1), S.RTerm (S.XCon dc2)])
                 <- S.takeXApps xx
         -> do  xPrim'  <- toCoreX  a (S.XPrim p)
                dc1'    <- toCoreDC dc1
                dc2'    <- toCoreDC dc2
                return  $  C.xApps a xPrim'
                                [ C.RTerm (C.XCon a dc1')
                                , C.RTerm (C.XCon a dc2')]
        S.XApp x1 x2
         -> C.XApp  <$> pure a  <*> toCoreX a x1 <*> toCoreArg a x2

        S.XLet lts x
         -> toCoreLtsX a lts x

        S.XCase x alts
         -> C.XCase <$> pure a  <*> toCoreX a x
                                <*> (sequence $ map (toCoreA a) alts)

        S.XCast c x
         -> C.XCast <$> pure a  <*> toCoreC a c <*> toCoreX a x

        -- These shouldn't exist in the desugared source tetra code.
        S.XDefix{}      -> Left $ ErrorConvertSugaredExp xx
        S.XInfixOp{}    -> Left $ ErrorConvertSugaredExp xx
        S.XInfixVar{}   -> Left $ ErrorConvertSugaredExp xx
        S.XMatch{}      -> Left $ ErrorConvertSugaredExp xx
        S.XWhere{}      -> Left $ ErrorConvertSugaredExp xx
        S.XAbsPat{}     -> Left $ ErrorConvertSugaredExp xx
        S.XLamCase{}    -> Left $ ErrorConvertSugaredExp xx
        S.XTuple{}      -> Left $ ErrorConvertSugaredExp xx
        S.XRecord{}     -> Left $ ErrorConvertSugaredExp xx
        S.XVariant{}    -> Left $ ErrorConvertSugaredExp xx
        S.XArray{}      -> Left $ ErrorConvertSugaredExp xx


-- Arg --------------------------------------------------------------------------------------------
toCoreArg :: SP -> S.Arg  -> ConvertM SP (C.Arg  SP C.Name)
toCoreArg sp xx
 = case xx of
        S.RType t
          -> C.RType     <$> toCoreT UniverseSpec t

        S.RWitness w
          -> C.RWitness  <$> toCoreW sp w

        S.RTerm x
          -> C.RTerm     <$> toCoreX sp x

        S.RImplicit arg'
          -> C.RImplicit <$> toCoreArg sp arg'


-- Lets -------------------------------------------------------------------------------------------
toCoreLtsX
        :: SP
        -> S.Lets -> S.Exp
        -> ConvertM SP (C.Exp SP C.Name)
toCoreLtsX a lts xBody
 = case lts of
        S.LLet b xBind
         -> C.XLet a
                <$> (C.LLet <$> toCoreBM UniverseSpec b <*> toCoreX a xBind)
                <*> toCoreX a xBody

        S.LRec bxs
         -> C.XLet a
                <$> (C.LRec
                        <$> (sequence
                                $ map (\(b, x) -> (,) <$> toCoreBM UniverseSpec b <*> toCoreX a x)
                                      bxs))
                <*> toCoreX a xBody

        S.LPrivate bs (S.CapsList bts)
         -> C.XLet a
                <$> (C.LPrivate
                        <$> (sequence  $ fmap (toCoreBM UniverseKind)
                                       $ [S.XBindVarMT b (Just S.KRegion) | b <- bs])
                        <*>  pure Nothing
                        <*> (sequence  $ fmap toCoreTBK bts))
                <*> toCoreX a xBody

        S.LPrivate bs S.CapsMutable
         -> do  bs'          <- sequence $ fmap (toCoreBM UniverseKind)
                                         $ [S.XBindVarMT b (Just S.KRegion) | b <- bs]
                let Just us' =  sequence $ map C.takeSubstBoundOfBind bs'

                C.XLet a (C.LPrivate bs' Nothing (bsCapsMutable us'))
                        <$> toCoreX a xBody

        S.LPrivate bs S.CapsConstant
         -> do  bs'          <- sequence $ fmap (toCoreBM UniverseKind)
                                         $ [S.XBindVarMT b (Just S.KRegion) | b <- bs]
                let Just us' =  sequence $ map C.takeSubstBoundOfBind bs'

                C.XLet a (C.LPrivate bs' Nothing (bsCapsConstant us'))
                        <$> toCoreX a xBody

        S.LExtend bs tParent (S.CapsList bts)
         -> C.XLet a
                <$> (C.LPrivate
                        <$> (sequence  $ fmap (toCoreBM UniverseKind)
                                       $ [S.XBindVarMT b (Just S.KRegion) | b <- bs])
                        <*> (fmap Just $ toCoreT UniverseKind tParent)
                        <*> (sequence  $ fmap toCoreTBK bts))
                <*> toCoreX a xBody

        S.LExtend bs tParent S.CapsMutable
         -> do  bs'          <- sequence $ fmap (toCoreBM UniverseKind)
                                         $ [S.XBindVarMT b (Just S.KRegion) | b <- bs]
                let Just us' =  sequence $ map C.takeSubstBoundOfBind bs'
                C.XLet a
                 <$> (C.LPrivate bs'
                        <$> (fmap Just $ toCoreT UniverseKind tParent)
                        <*> pure (bsCapsMutable us'))
                 <*> toCoreX a xBody

        S.LExtend bs tParent S.CapsConstant
         -> do  bs'          <- sequence $ fmap (toCoreBM UniverseKind)
                                         $ [S.XBindVarMT b (Just S.KRegion) | b <- bs]
                let Just us' =  sequence $ map C.takeSubstBoundOfBind bs'
                C.XLet a
                 <$> (C.LPrivate bs'
                        <$> (fmap Just $ toCoreT UniverseKind tParent)
                        <*> pure (bsCapsConstant us'))
                 <*> toCoreX a xBody

        S.LGroup bRec cls
         -> do  let sigs  = collectSigsFromClauses cls
                let vals  = collectBoundVarsFromClauses cls

                bxxs    <- fmap catMaybes
                        $  mapM (makeBindingFromClause sigs vals) cls

                let bxs = [(b, x) | (b, (_, x)) <- bxxs]

                if bRec
                 then toCoreLtsX a (S.LRec bxs) xBody
                 else toCoreX a (foldr (\(b, xBind) -> S.XLet (S.LLet b xBind)) xBody bxs)

 where  -- Anonymous capabilities for a mutable region.
        bsCapsMutable us
         = concat [ let t = C.TVar u
                    in   [ C.BNone (C.tRead t)
                         , C.BNone (C.tWrite t)
                         , C.BNone (C.tAlloc t)]
                  | u <- us ]

        -- Anonymous capabilities for a constant region.
        bsCapsConstant us
         = concat [ let t = C.TVar u
                    in   [ C.BNone (C.tRead t)
                         , C.BNone (C.tAlloc t)]
                  | u <- us ]


-- Cast -------------------------------------------------------------------------------------------
toCoreC :: SP -> S.Cast -> ConvertM SP (C.Cast SP C.Name)
toCoreC _a cc
 = case cc of
        S.CastWeakenEffect eff
         -> C.CastWeakenEffect <$> toCoreT UniverseSpec eff

        S.CastBox
         -> pure C.CastBox

        S.CastRun
         -> pure C.CastRun


-- Alt --------------------------------------------------------------------------------------------
toCoreA  :: SP -> S.AltCase -> ConvertM SP (C.Alt SP C.Name)
toCoreA sp alt
 = case alt of
        S.AAltCase w [S.GExp x]
         -> C.AAlt <$> toCoreP alt w <*> toCoreX sp x

        _ -> error $ unlines
                [ "ddc-source-discus: cannot convert sugared alt" ]


-- Pat --------------------------------------------------------------------------------------------
toCoreP  :: S.AltCase -> S.Pat -> ConvertM a (C.Pat C.Name)
toCoreP _aa pp
 = case pp of
        S.PDefault
         -> pure C.PDefault

        S.PAt{}
         -> error $ unlines
                  [ "ddc-source-discus: cannot convert PAt pattern" ]

        S.PVar{}
         -> error $ unlines
                  [ "ddc-source-discus: cannot convert PVar pattern" ]

        S.PData dc bs
         -> C.PData <$> toCoreDC dc <*> (sequence $ fmap toCorePasB bs)


-- | Convert a pattern to a core binder.
--   Only default and var patterns are supported,
--   nested patterns need to have been eliminated by the desugarer.
toCorePasB :: S.Pat -> ConvertM a (C.Bind C.Name)
toCorePasB pp
 = let  hole = C.TVar (C.UName C.NameHole)
   in   case pp of
         S.PDefault
          -> pure $ C.BAnon hole

         S.PAt{}
          -> error $ "ddc-source-discus: cannot convert pattern"

         S.PVar b
          -> toCoreB b

         S.PData{}
          -> error $ "ddc-source-discus: cannot convert nested pattern"


-- DaCon ------------------------------------------------------------------------------------------
toCoreDC :: S.DaCon S.DaConBound S.Type
         -> ConvertM a (C.DaCon C.Name (C.Type (C.Name)))

toCoreDC dc
 = case dc of
        S.DaConUnit
         -> pure $ C.DaConUnit

        S.DaConRecord ns
         -> pure $ C.DaConRecord ns

        S.DaConPrim  n
         -> C.DaConPrim  <$> (pure $ toCoreDaConBound n)

        S.DaConBound (C.DaConBoundName _ _ n)
         -> C.DaConBound <$> (C.DaConBoundName
                <$> pure Nothing
                <*> pure Nothing
                <*> (pure $ toCoreDaConBound n))
