{-# LANGUAGE TypeFamilies #-}

-- | Source Tetra conversion to Disciple Core Tetra language.
module DDC.Source.Tetra.Convert
        ( ConvertM
        , ErrorConvert (..)
        , coreOfSourceModule
        , runConvertM)
where
import DDC.Source.Tetra.Convert.Error
import DDC.Data.SourcePos
import DDC.Type.Universe                                (Universe (..), universeUp)

import qualified DDC.Source.Tetra.Module                as S
import qualified DDC.Source.Tetra.DataDef               as S
import qualified DDC.Source.Tetra.Exp                   as S
import qualified DDC.Source.Tetra.Prim                  as S
import qualified DDC.Source.Tetra.Env                   as Env

import qualified DDC.Core.Tetra.Compounds               as C
import qualified DDC.Core.Tetra.Prim                    as C
import qualified DDC.Core.Module                        as C
import qualified DDC.Core.Exp.Annot                     as C
import qualified DDC.Type.DataDef                       as C
import qualified DDC.Type.Sum                           as CSum
import qualified Data.Text                              as Text
import Data.Maybe
import qualified Text.Show.Pretty                       as Text

import DDC.Core.Module 
        ( ExportSource  (..)
        , ImportType    (..)
        , ImportCap     (..)
        , ImportValue   (..))


---------------------------------------------------------------------------------------------------
type ConvertM a x
        = Either (ErrorConvert a) x

type SP = SourcePos


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
 = C.LRec <$> (sequence $ mapMaybe bindOfTop tops)


---------------------------------------------------------------------------------------------------
-- | Try to convert a `TopBind` to a top-level binding, 
--   or `Nothing` if it isn't one.
bindOfTop :: S.Top S.Source
          -> Maybe (ConvertM S.Source (C.Bind C.Name, C.Exp SP C.Name))

bindOfTop tt
 = case tt of
        S.TopClause _ (S.SLet a bm ps [S.GExp x])
         -> Just ((,)   <$> toCoreBM UniverseSpec bm 
                        <*> toCoreX a (wrapParams ps x))

        S.TopClause _ (S.SLet{})
         -> error $ unlines
                [ "bindOfTop: cannot convert top"
                , Text.ppShow tt]

        -- Sigs don't make bindings.
        S.TopClause _ S.SSig{}
         -> Nothing

        S.TopType{}     -> Nothing
        S.TopData{}     -> Nothing


wrapParams :: [S.Param] -> S.Exp -> S.Exp
wrapParams [] x = x
wrapParams (p:ps) x
 = case p of
        S.MType    b mt    
         -> S.XLAM (S.XBindVarMT b mt)       $ wrapParams ps x

        S.MWitness b mt
         -> S.XLam (S.XBindVarMT b mt)       $ wrapParams ps x

        S.MValue   S.PDefault mt
         -> S.XLam (S.XBindVarMT S.BNone mt) $ wrapParams ps x

        S.MValue   (S.PVar b) mt
         -> S.XLam (S.XBindVarMT b mt)       $ wrapParams ps x

        S.MValue   _ _
         -> error $ unlines
                [ "wrapParams: cannot convert pat"
                , Text.ppShow p ]


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


-- TypeDef ----------------------------------------------------------------------------------------
toCoreTypeDef
        :: (S.TyConBind, S.Type)
        -> ConvertM a (C.Name, (C.Kind C.Name, C.Type C.Name))

toCoreTypeDef (b, t)
 = do   n       <- toCoreTBCN b
        t'      <- toCoreT UniverseSpec t
        let hole = C.TVar (C.UName C.NameHole)
        return  (n, (hole, t'))


-- Type -------------------------------------------------------------------------------------------
toCoreT :: Universe -> S.Type -> ConvertM a (C.Type C.Name)
toCoreT uu tt
 = case tt of
        S.TAnnot _ t
         -> toCoreT uu t

        S.TCon (S.TyConBot k)     
         -> do  k'      <- toCoreT uu k
                return  $ C.tBot k'

        S.TCon (S.TyConVoid)
         -> do  return  $ C.tVoid

        S.TCon tc
         -> do  mtc'    <- toCoreTC uu tc
                case mtc' of
                 Nothing        -> error  $ "ddc-soure-tetra.toCoreT: " ++ show tt
                 Just tc'       -> return $ C.TCon tc'

        S.TVar u
         -> C.TVar <$> toCoreU  u

        S.TAbs b k t
         -> do  b'      <- toCoreTBK (b, k)
                t'      <- toCoreT uu  t
                return  $  C.TAbs b' t'

        S.TApp (S.TCon (S.TyConForall _)) (S.TAbs b k t)
         -> let uu'     =  universeUp uu
            in  C.TForall <$> toCoreBM uu' (S.XBindVarMT b (Just k)) <*> toCoreT uu t

        S.TApp{}
         | Just (k, ts) <- S.takeTSums tt
         -> do  let uu' =  universeUp uu
                k'      <- toCoreT uu' k
                ts'     <- sequence $ fmap (toCoreT uu) ts
                return  $  C.TSum (CSum.fromList k' ts')

        S.TApp t1 t2
         -> C.TApp      <$> toCoreT uu t1 <*> toCoreT uu t2



-- TyCon ------------------------------------------------------------------------------------------
-- | Convert a Source TyCon to Core, or Nothing if it cannot be converted in isolation.
toCoreTC :: Universe -> S.TyCon -> ConvertM a (Maybe (C.TyCon C.Name))
toCoreTC uu tc
 = case tc of
        S.TyConVoid             -> return Nothing
        S.TyConUnit             -> return $ Just $ C.TyConSpec C.TcConUnit

        S.TyConFun       
         -> case uu of
                UniverseSpec    -> return $ Just $ C.TyConSpec C.TcConFun
                UniverseKind    -> return $ Just $ C.TyConKind C.KiConFun
                _               -> return Nothing


        S.TyConSum _     -> return Nothing

        S.TyConBot _k    -> return Nothing
        S.TyConForall _k -> return Nothing
        S.TyConExists _k -> return Nothing

        -- Primitive type constructors.
        S.TyConPrim pt
         -> case pt of
                -- Ambient TyCons
                S.PrimTypeSoCon sc -> return $ Just $ C.TyConSort    sc
                S.PrimTypeKiCon kc -> return $ Just $ C.TyConKind    kc
                S.PrimTypeTwCon tw -> return $ Just $ C.TyConWitness tw
                S.PrimTypeTcCon ts -> return $ Just $ C.TyConSpec    ts

                -- Primitive TyCons
                S.PrimTypeTyCon tcy
                 -> do  k       <- toCoreT UniverseKind $ S.kindPrimTyCon tcy
                        return  $ Just $ C.TyConBound (C.UPrim (C.NamePrimTyCon tcy) k) k

                S.PrimTypeTyConTetra tct 
                 -> do  k       <- toCoreT UniverseKind $ S.kindPrimTyConTetra tct
                        let tct' =  toCoreTyConTetra tct
                        return  $ Just $ C.TyConBound (C.UPrim (C.NameTyConTetra tct') k) k

        -- Bound type constructors.
        --   The embedded kind is set to Bot. We rely on the spreader
        --   to fill in the real kind before type checking.
        S.TyConBound (S.TyConBoundName tx)
         -> return $ Just 
         $  C.TyConBound (C.UName (C.NameCon (Text.unpack tx))) 
                                  (C.TVar (C.UName C.NameHole))


-- Exp --------------------------------------------------------------------------------------------
toCoreX :: SP -> S.Exp -> ConvertM S.Source (C.Exp SP C.Name)
toCoreX a xx
 = case xx of
        S.XAnnot a' x
         -> toCoreX a' x

        S.XVar u      
         -> C.XVar      <$> pure a <*> toCoreU u

        -- Wrap text literals into Text during conversion to Core.
        -- The 'textLit' variable refers to whatever is in scope.
        S.XCon dc@(C.DaConPrim (S.DaConBoundLit (S.PrimLitTextLit{})) _)
         -> C.XApp      <$> pure a 
                        <*> (C.XVar <$> pure a <*> (pure $ C.UName (C.NameVar "textLit")))
                        <*> (C.XCon <$> pure a <*> (toCoreDC dc))

        S.XPrim p
         -> do  let p'  =  toCorePrimVal p 
                t'      <- toCoreT UniverseSpec  $ Env.typeOfPrimVal p
                return  $ C.XVar a (C.UPrim p' t')

        S.XCon  dc
         -> C.XCon      <$> pure a <*> toCoreDC dc

        S.XLAM  bm x
         -> C.XLAM      <$> pure a <*> toCoreBM UniverseKind bm  <*> toCoreX a x

        S.XLam  bm x
         -> C.XLam      <$> pure a <*> toCoreBM UniverseSpec bm  <*> toCoreX a x

        -- We don't want to wrap the source file path passed to the default# prim
        -- in a Text constructor, so detect this case separately.
        S.XApp  _ _
         |  Just ( p@(S.PrimValError S.OpErrorDefault)
                 , [S.XCon dc1, S.XCon dc2])
                 <- S.takeXPrimApps xx
         -> do  xPrim'  <- toCoreX  a (S.XPrim p)
                dc1'    <- toCoreDC dc1
                dc2'    <- toCoreDC dc2
                return  $  C.xApps a xPrim' [C.XCon a dc1', C.XCon a dc2']

        S.XApp x1 x2
         -> C.XApp      <$> pure a  <*> toCoreX a x1 <*> toCoreX a x2

        S.XLet lts x
         -> C.XLet      <$> pure a  <*> toCoreLts a lts <*> toCoreX a x

        S.XCase x alts
         -> C.XCase     <$> pure a  <*> toCoreX a x 
                                    <*> (sequence $ map (toCoreA a) alts)

        S.XCast c x
         -> C.XCast     <$> pure a  <*> toCoreC a c <*> toCoreX a x

        S.XType t
         -> C.XType     <$> pure a  <*> toCoreT UniverseSpec t

        S.XWitness w
         -> C.XWitness  <$> pure a  <*> toCoreW a w

        -- These shouldn't exist in the desugared source tetra code.
        S.XDefix{}      -> Left $ ErrorConvertCannotConvertSugarExp xx
        S.XInfixOp{}    -> Left $ ErrorConvertCannotConvertSugarExp xx
        S.XInfixVar{}   -> Left $ ErrorConvertCannotConvertSugarExp xx
        S.XMatch{}      -> Left $ ErrorConvertCannotConvertSugarExp xx
        S.XWhere{}      -> Left $ ErrorConvertCannotConvertSugarExp xx
        S.XLamPat{}     -> Left $ ErrorConvertCannotConvertSugarExp xx
        S.XLamCase{}    -> Left $ ErrorConvertCannotConvertSugarExp xx        


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

{-
        S.LGroup [c]
         | [(b, x')] <- stripClause c
         -> toCoreLts a (S.LLet b x')
-}
        S.LGroup cs
         -> do  let bxs = concatMap stripClause cs
                toCoreLts a (S.LRec bxs)


stripClause :: S.Clause -> [(S.BindVarMT, S.Exp)]
stripClause cc
 = case cc of
        S.SLet _ bm ps [S.GExp x]
         -> [(bm, wrapParams ps x)]

        S.SLet{}       
         -> error $ unlines
                [ "stripClause: cannot strip"
                , Text.ppShow cc]

        S.SSig{}
         -> []


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
                [ "ddc-source-tetra: cannot convert sugared alt"       
                , Text.ppShow alt]


-- Pat --------------------------------------------------------------------------------------------
toCoreP  :: S.AltCase -> S.Pat -> ConvertM a (C.Pat C.Name)
toCoreP aa pp
 = case pp of
        S.PDefault 
         -> pure C.PDefault
        
        S.PAt{}
         -> error $ unlines
                  [ "ddc-source-tetra: cannot convert PAt pattern"
                  , Text.ppShow pp]

        S.PVar{}
         -> error $ unlines
                  [ "ddc-source-tetra: cannot convert PVar pattern"
                  , Text.ppShow aa]

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
          -> error $ "ddc-source-tetra: cannot convert at pattern "     ++ Text.ppShow pp

         S.PVar b
          -> do b'      <- toCoreB b
                return  b'

         S.PData{}
          -> error $ "ddc-source-tetra: cannot convert nested pattern " ++ Text.ppShow pp


-- DaCon ------------------------------------------------------------------------------------------
toCoreDC :: S.DaCon S.DaConBound S.Type
         -> ConvertM a (C.DaCon C.Name (C.Type (C.Name)))

toCoreDC dc
 = case dc of
        S.DaConUnit
         -> pure $ C.DaConUnit

        S.DaConPrim  n t 
         -> C.DaConPrim  <$> (pure $ toCoreDaConBound n) <*> toCoreT UniverseSpec t

        S.DaConBound n
         -> C.DaConBound <$> (pure $ toCoreDaConBound n)


-- Witness ----------------------------------------------------------------------------------------
toCoreW :: SP -> S.Witness -> ConvertM a (C.Witness SP C.Name)
toCoreW a ww
 = case ww of
        S.WAnnot a' w   
         -> toCoreW a' w

        S.WVar  u
         -> C.WVar  <$> pure a <*> toCoreU  u

        S.WCon  wc
         -> C.WCon  <$> pure a <*> toCoreWC wc

        S.WApp  w1 w2
         -> C.WApp  <$> pure a <*> toCoreW a w1 <*> toCoreW a w2

        S.WType t
         -> C.WType <$> pure a <*> toCoreT UniverseSpec t


-- WiCon ------------------------------------------------------------------------------------------
toCoreWC :: S.WiCon -> ConvertM a (C.WiCon C.Name)
toCoreWC wc
 = case wc of
        S.WiConBound u t
         -> C.WiConBound <$> toCoreU u 
                         <*> toCoreT UniverseSpec t


-- Bind -------------------------------------------------------------------------------------------
-- | Convert a type constructor binding occurrence to a core name.
toCoreTBCN :: S.GTBindCon S.Source  -> ConvertM a C.Name
toCoreTBCN (S.TyConBindName n)
 = return $ C.NameCon (Text.unpack n)


-- | Convert a type constructor bound occurrence to a core name.
toCoreTUCN :: S.GTBoundCon S.Source -> ConvertM a C.Name
toCoreTUCN (S.TyConBoundName n)
 = return $ C.NameCon (Text.unpack n)


-- | Convert a term variable bound occurrence to a core name.
toCoreXUVN :: S.Bound -> ConvertM a C.Name
toCoreXUVN uu
 = case uu of
        S.UName n -> return $ C.NameVar (Text.unpack n)
        S.UIx  _i -> error "ddc-source-tetra.toCoreXBVN: anon bound"
        S.UHole   -> return $ C.NameHole        


toCoreXBVN  :: S.GTBindVar S.Source -> ConvertM a C.Name
toCoreXBVN bb
 = case bb of
        S.BNone   -> error "ddc-source-tetra.toCoreXBVN: none bound"
        S.BAnon   -> error "ddc-source-tetra.toCoreXBVN: anon bound"
        S.BName n -> return $ C.NameVar (Text.unpack n)


-- | Convert a type binder and kind to core.
toCoreTBK :: (S.GTBindVar S.Source, S.GType S.Source)
          -> ConvertM a (C.Bind C.Name)
toCoreTBK (bb, k)
 = case bb of
        S.BNone   -> C.BNone <$> (toCoreT UniverseKind k)
        S.BAnon   -> C.BAnon <$> (toCoreT UniverseKind k)
        S.BName n -> C.BName <$> (return $ C.NameVar (Text.unpack n)) 
                             <*> (toCoreT UniverseKind k)


-- | Convert an unannoted binder to core.
toCoreB  :: S.Bind -> ConvertM a (C.Bind C.Name)
toCoreB bb
 = let hole     = C.TVar (C.UName C.NameHole)
   in case bb of
        S.BNone   -> return $ C.BNone hole
        S.BAnon   -> return $ C.BAnon hole
        S.BName n -> return $ C.BName (C.NameVar (Text.unpack n)) hole


-- | Convert a possibly annoted binding occurrence of a variable to core.
toCoreBM :: Universe -> S.GXBindVarMT S.Source -> ConvertM a (C.Bind C.Name)
toCoreBM uu bb
 = case bb of
        S.XBindVarMT S.BNone     (Just t)
         -> C.BNone <$> toCoreT uu t

        S.XBindVarMT S.BNone     Nothing
         -> C.BNone <$> (return $ C.TVar (C.UName C.NameHole))


        S.XBindVarMT S.BAnon     (Just t)   
         -> C.BAnon <$> toCoreT uu t

        S.XBindVarMT S.BAnon     Nothing    
         -> C.BAnon <$> (return $ C.TVar (C.UName C.NameHole))


        S.XBindVarMT (S.BName n) (Just t)
         -> C.BName <$> (return $ C.NameVar (Text.unpack n)) 
                    <*> toCoreT uu t

        S.XBindVarMT (S.BName n) Nothing    
         -> C.BName <$> (return $ C.NameVar (Text.unpack n)) 
                    <*> (return $ C.TVar (C.UName C.NameHole))


-- Bound ------------------------------------------------------------------------------------------
toCoreU :: S.Bound -> ConvertM a (C.Bound C.Name)
toCoreU uu
 = case uu of
        S.UName n       -> C.UName <$> pure (C.NameVar (Text.unpack n))
        S.UIx   i       -> C.UIx   <$> (pure i)
        S.UHole         -> C.UName <$> pure (C.NameHole)


-- Name -------------------------------------------------------------------------------------------
-- | Convert a Tetra specific type constructor to core.
toCoreTyConTetra :: S.PrimTyConTetra -> C.TyConTetra
toCoreTyConTetra tc
 = case tc of
        S.PrimTyConTetraTuple n -> C.TyConTetraTuple n
        S.PrimTyConTetraVector  -> C.TyConTetraVector
        S.PrimTyConTetraF       -> C.TyConTetraF
        S.PrimTyConTetraC       -> C.TyConTetraC
        S.PrimTyConTetraU       -> C.TyConTetraU


-- | Convert a binding occurrences of a data constructor to a core name.
toCoreDaConBind :: S.DaConBind -> C.Name
toCoreDaConBind (S.DaConBindName tx)
 = C.NameCon (Text.unpack tx)


-- | Convert a bound occurrence of a data constructor to a core name.
toCoreDaConBound :: S.DaConBound -> C.Name
toCoreDaConBound dcb
 = case dcb of
        S.DaConBoundName tx
         -> C.NameCon (Text.unpack tx)

        S.DaConBoundLit pl
         -> toCorePrimLit pl


-- | Convert a value primtivie to a core name.
toCorePrimVal :: S.PrimVal -> C.Name
toCorePrimVal pv
 = case pv of
        S.PrimValArith  p       -> C.NamePrimArith  p False
        S.PrimValCast   p       -> C.NamePrimCast   p False
        S.PrimValError  p       -> C.NameOpError    p False
        S.PrimValVector p       -> C.NameOpVector   p False
        S.PrimValFun    p       -> C.NameOpFun      p
        S.PrimValLit    p       -> toCorePrimLit    p


-- | Convert a primitive literal to a core name.
toCorePrimLit :: S.PrimLit -> C.Name
toCorePrimLit pl
 = case pl of       
        S.PrimLitBool    x      -> C.NameLitBool    x
        S.PrimLitNat     x      -> C.NameLitNat     x
        S.PrimLitInt     x      -> C.NameLitInt     x
        S.PrimLitSize    x      -> C.NameLitSize    x
        S.PrimLitWord    x s    -> C.NameLitWord    x s
        S.PrimLitFloat   x s    -> C.NameLitFloat   x s
        S.PrimLitChar    x      -> C.NameLitChar    x
        S.PrimLitTextLit x      -> C.NameLitTextLit x

