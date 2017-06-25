{-# LANGUAGE TypeFamilies #-}

-- | Source Tetra conversion to Disciple Core Tetra language.
module DDC.Source.Tetra.Convert
        ( ConvertM
        , ErrorConvert (..)
        , coreOfSourceModule
        , runConvertM)
where
import DDC.Source.Tetra.Convert.Error
import DDC.Source.Tetra.Convert.Witness
import DDC.Source.Tetra.Convert.Clause
import DDC.Source.Tetra.Convert.Type
import DDC.Source.Tetra.Convert.Prim
import DDC.Source.Tetra.Convert.Base

import qualified DDC.Source.Tetra.Module                as S
import qualified DDC.Source.Tetra.DataDef               as S
import qualified DDC.Source.Tetra.Exp                   as S
import qualified DDC.Source.Tetra.Env                   as Env

import qualified DDC.Core.Tetra.Compounds               as C
import qualified DDC.Core.Tetra.Prim                    as C
import qualified DDC.Core.Module                        as C
import qualified DDC.Type.DataDef                       as C
import qualified Data.Text                              as Text
import Data.Maybe

import DDC.Core.Module
        ( ExportSource  (..)
        , ImportType    (..)
        , ImportCap     (..)
        , ImportValue   (..))

---------------------------------------------------------------------------------------------------
-- | Run a conversion computation.
runConvertM :: ConvertM a x -> Either (ErrorConvert a) x
runConvertM cc = cc

-- | Convert a Source Tetra module to Core Tetra.
coreOfSourceModule
        :: SP
        -> S.Module S.Source
        -> Either (ErrorConvert S.Source) (C.Module SP C.Name)

coreOfSourceModule a mm
        = runConvertM
        $ coreOfSourceModuleM a mm


-- Module -----------------------------------------------------------------------------------------
-- | Convert a Source Tetra module to Core Tetra.
--
--   The Source code needs to already have been desugared and cannot contain,
--   and `XDefix`, `XInfixOp`, or `XInfixVar` nodes, else `error`.
--
--   We use the map of core headers to add imports for all the names that this
--   module uses from its environment.
--
coreOfSourceModuleM
        :: SP
        -> S.Module S.Source
        -> ConvertM S.Source (C.Module SP C.Name)

coreOfSourceModuleM a mm
 = do
        -- Exported types and values.
        exportTypes'
         <- sequence
         $  fmap (\n -> (,) <$> toCoreTUCN n
                            <*> (fmap ExportSourceLocalNoType $ toCoreTUCN n))
         $  S.moduleExportTypes mm

        exportValues'
         <- sequence
         $  fmap (\n -> (,) <$> toCoreXUVN n
                            <*> (fmap ExportSourceLocalNoType (toCoreXUVN n)))
         $  S.moduleExportValues mm


        -- Imported types, capabilities and values.
        importTypes'
         <- sequence
         $  fmap (\(n, it) -> (,) <$> toCoreTBCN n <*> (toCoreImportType it))
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
         <- sequence $ fmap toCoreDataDef
         $  [ def    | S.TopData _ def <- S.moduleTops mm ]

        -- Type equations.
        typeDefsLocal
         <- sequence $ fmap toCoreTypeDef
         $  [ (b, t) | S.TopType _ b t <- S.moduleTops mm ]

        -- Top level bindings.
        ltsTops
         <- letsOfTops $  S.moduleTops mm

        return
         $ C.ModuleCore
                { C.moduleName          = S.moduleName mm
                , C.moduleIsHeader      = False

                , C.moduleExportTypes   = exportTypes'

                , C.moduleExportValues
                   =  exportValues'
                   ++ (if C.isMainModuleName (S.moduleName mm)
                        && (not $ elem (S.UName (Text.pack "main"))
                                $ S.moduleExportValues mm)

                        then [ ( C.NameVar "main"
                             , ExportSourceLocalNoType (C.NameVar "main"))]

                        else [])

                , C.moduleImportTypes    = importTypes'
                , C.moduleImportCaps     = importCaps'
                , C.moduleImportValues   = importValues'
                , C.moduleImportDataDefs = []
                , C.moduleImportTypeDefs = []
                , C.moduleDataDefsLocal  = dataDefsLocal
                , C.moduleTypeDefsLocal  = typeDefsLocal
                , C.moduleBody           = C.XLet  a ltsTops (C.xUnit a) }


-- | Extract the top-level bindings from some source definitions.
letsOfTops :: [S.Top S.Source]
           -> ConvertM S.Source (C.Lets SP C.Name)
letsOfTops tops
 = do
        -- Collect up the type signatures defined at top level.
        let cls         = [cl   | S.TopClause _ cl      <- tops]
        let sigs        = collectSigsFromClauses      cls
        let vals        = collectBoundVarsFromClauses cls

        bxps            <- fmap catMaybes
                        $  mapM (makeBindingFromClause sigs vals) cls

        let (bms, xps)  =  unzip bxps
        bs'             <- mapM (toCoreBM UniverseSpec) bms
        xs'             <- mapM (\(sp, x) -> toCoreX sp x) xps
        return $ C.LRec $ zip bs' xs'


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

        ImportValueSea v t
         -> ImportValueSea
         <$> pure v     <*> toCoreT UniverseSpec t


-- DataDef ----------------------------------------------------------------------------------------
toCoreDataDef :: S.DataDef S.Source -> ConvertM a (C.DataDef C.Name)
toCoreDataDef def
 = do
        defParams       <- sequence $ fmap toCoreTBK $ S.dataDefParams def

        defCtors        <- sequence $ fmap (\(ctor, tag) -> toCoreDataCtor def tag ctor)
                                    $ [(ctor, tag) | ctor <- S.dataDefCtors def
                                                   | tag  <- [0..]]

        let (S.TyConBindName txTyConName) = S.dataDefTypeName def

        return $ C.DataDef
         { C.dataDefTypeName    = C.NameCon (Text.unpack txTyConName)
         , C.dataDefParams      = defParams
         , C.dataDefCtors       = Just $ defCtors
         , C.dataDefIsAlgebraic = True }


-- DataCtor ---------------------------------------------------------------------------------------
toCoreDataCtor
        :: S.DataDef  S.Source
        -> Integer
        -> S.DataCtor S.Source
        -> ConvertM a (C.DataCtor C.Name)

toCoreDataCtor dataDef tag ctor
 = do   typeParams      <- sequence $ fmap toCoreTBK $ S.dataDefParams dataDef
        fieldTypes      <- sequence $ fmap (toCoreT UniverseSpec)   $ S.dataCtorFieldTypes ctor
        resultType      <- toCoreT UniverseSpec (S.dataCtorResultType ctor)
        let (S.TyConBindName txTyConName) = S.dataDefTypeName dataDef

        return $ C.DataCtor
         { C.dataCtorName        = toCoreDaConBind (S.dataCtorName ctor)
         , C.dataCtorTag         = tag
         , C.dataCtorFieldTypes  = fieldTypes
         , C.dataCtorResultType  = resultType
         , C.dataCtorTypeName    = C.NameCon (Text.unpack txTyConName)
         , C.dataCtorTypeParams  = typeParams }


-- Exp --------------------------------------------------------------------------------------------
toCoreX :: SP -> S.Exp -> ConvertM S.Source (C.Exp SP C.Name)
toCoreX a xx
 = case xx of
        S.XAnnot a' x
         -> toCoreX a' x

        S.XPrim p
         ->     return  $ C.XPrim a p

        S.XFrag p
         -> do  let p'  =  toCorePrimVal p
                t'      <- toCoreT UniverseSpec  $ Env.typeOfPrimVal p
                return  $ C.XVar a (C.UPrim p' t')

        S.XVar u
         -> C.XVar      <$> pure a <*> toCoreU u

        -- Wrap text literals into Text during conversion to Core.
        -- The 'textLit' variable refers to whatever is in scope.
        S.XCon dc@(C.DaConPrim (S.DaConBoundLit (S.PrimLitTextLit{})) _)
         -> C.XApp      <$> pure a
                        <*> (C.XVar  <$> pure a <*> (pure $ C.UName (C.NameVar "textLit")))
                        <*> (C.RTerm <$> (C.XCon <$> pure a <*> (toCoreDC dc)))

        S.XCon  dc
         -> C.XCon      <$> pure a <*> toCoreDC dc

        S.XAbs  p x
         -> C.XAbs      <$> pure a <*> toCoreParam p  <*> toCoreX a x

        -- We don't want to wrap the source file path passed to the default# prim
        -- in a Text constructor, so detect this case separately.
        S.XApp  _ _
         |  Just ( p@(S.PrimValError S.OpErrorDefault)
                 , [S.RTerm (S.XCon dc1), S.RTerm (S.XCon dc2)])
                 <- S.takeXFragApps xx
         -> do  xPrim'  <- toCoreX  a (S.XFrag p)
                dc1'    <- toCoreDC dc1
                dc2'    <- toCoreDC dc2
                return  $  C.xApps a xPrim'
                                [ C.RTerm (C.XCon a dc1')
                                , C.RTerm (C.XCon a dc2')]
        S.XApp x1 x2
         -> C.XApp      <$> pure a  <*> toCoreX a x1 <*> toCoreArg a x2

        S.XLet lts x
         -> C.XLet      <$> pure a  <*> toCoreLts a lts <*> toCoreX a x

        S.XCase x alts
         -> C.XCase     <$> pure a  <*> toCoreX a x
                                    <*> (sequence $ map (toCoreA a) alts)

        S.XCast c x
         -> C.XCast     <$> pure a  <*> toCoreC a c <*> toCoreX a x

        -- These shouldn't exist in the desugared source tetra code.
        S.XDefix{}      -> Left $ ErrorConvertSugaredExp xx
        S.XInfixOp{}    -> Left $ ErrorConvertSugaredExp xx
        S.XInfixVar{}   -> Left $ ErrorConvertSugaredExp xx
        S.XMatch{}      -> Left $ ErrorConvertSugaredExp xx
        S.XWhere{}      -> Left $ ErrorConvertSugaredExp xx
        S.XAbsPat{}     -> Left $ ErrorConvertSugaredExp xx
        S.XLamCase{}    -> Left $ ErrorConvertSugaredExp xx


-- Arg --------------------------------------------------------------------------------------------
toCoreArg :: SP -> S.Arg  -> ConvertM S.Source (C.Arg  SP C.Name)
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
toCoreLts :: SP -> S.Lets -> ConvertM S.Source (C.Lets SP C.Name)
toCoreLts a lts
 = case lts of
        S.LLet b x
         -> C.LLet <$> toCoreBM UniverseSpec b <*> toCoreX a x

        S.LRec bxs
         -> C.LRec <$> (sequence
                $ map (\(b, x) -> (,) <$> toCoreBM UniverseSpec b <*> toCoreX a x) bxs)

        S.LPrivate bs Nothing bts
         -> C.LPrivate
                <$> (sequence  $ fmap (toCoreBM UniverseKind)
                               $ [S.XBindVarMT b (Just S.KRegion) | b <- bs])
                <*>  pure Nothing
                <*> (sequence  $ fmap toCoreTBK bts)

        S.LPrivate bs (Just tParent) bts
         -> C.LPrivate
                <$> (sequence  $ fmap (toCoreBM UniverseKind)
                               $ [S.XBindVarMT b (Just S.KRegion) | b <- bs])
                <*> (fmap Just $ toCoreT UniverseKind tParent)
                <*> (sequence  $ fmap toCoreTBK bts)

        S.LGroup cls
         -> do  let sigs  = collectSigsFromClauses cls
                let vals  = collectBoundVarsFromClauses cls

                bxs       <- fmap catMaybes
                          $  mapM (makeBindingFromClause sigs vals) cls

                let bxs' = [(b, x) | (b, (_, x)) <- bxs]
                toCoreLts a (S.LRec bxs')


-- Cast -------------------------------------------------------------------------------------------
toCoreC :: SP -> S.Cast -> ConvertM S.Source (C.Cast SP C.Name)
toCoreC a cc
 = case cc of
        S.CastWeakenEffect eff
         -> C.CastWeakenEffect <$> toCoreT UniverseSpec eff

        S.CastPurify w
         -> C.CastPurify       <$> toCoreW a w

        S.CastBox
         -> pure C.CastBox

        S.CastRun
         -> pure C.CastRun


-- Alt --------------------------------------------------------------------------------------------
toCoreA  :: SP -> S.AltCase -> ConvertM S.Source (C.Alt SP C.Name)
toCoreA sp alt
 = case alt of
        S.AAltCase w [S.GExp x]
         -> C.AAlt <$> toCoreP alt w <*> toCoreX sp x

        _ -> error $ unlines
                [ "ddc-source-tetra: cannot convert sugared alt" ]


-- Pat --------------------------------------------------------------------------------------------
toCoreP  :: S.AltCase -> S.Pat -> ConvertM a (C.Pat C.Name)
toCoreP _aa pp
 = case pp of
        S.PDefault
         -> pure C.PDefault

        S.PAt{}
         -> error $ unlines
                  [ "ddc-source-tetra: cannot convert PAt pattern" ]

        S.PVar{}
         -> error $ unlines
                  [ "ddc-source-tetra: cannot convert PVar pattern" ]

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
          -> error $ "ddc-source-tetra: cannot convert pattern"

         S.PVar b
          -> toCoreB b

         S.PData{}
          -> error $ "ddc-source-tetra: cannot convert nested pattern"


-- DaCon ------------------------------------------------------------------------------------------
toCoreDC :: S.DaCon S.DaConBound S.Type
         -> ConvertM a (C.DaCon C.Name (C.Type (C.Name)))

toCoreDC dc
 = case dc of
        S.DaConUnit
         -> pure $ C.DaConUnit

        S.DaConRecord ns
         -> pure $ C.DaConRecord ns

        S.DaConPrim  n t
         -> C.DaConPrim  <$> (pure $ toCoreDaConBound n) <*> toCoreT UniverseSpec t

        S.DaConBound n
         -> C.DaConBound <$> (pure $ toCoreDaConBound n)
