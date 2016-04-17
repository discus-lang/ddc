{-# LANGUAGE TypeFamilies #-}

-- | Source Tetra conversion to Disciple Core Tetra language.
module DDC.Source.Tetra.Convert
        ( ConvertM
        , ErrorConvert (..)
        , coreOfSourceModule
        , runConvertM)
where
import DDC.Source.Tetra.Convert.Error

import qualified DDC.Source.Tetra.Transform.Guards      as S
import qualified DDC.Source.Tetra.Module                as S
import qualified DDC.Source.Tetra.DataDef               as S
import qualified DDC.Source.Tetra.Env                   as S
import qualified DDC.Source.Tetra.Exp.Annot             as S
import qualified DDC.Source.Tetra.Prim                  as S

import qualified DDC.Core.Tetra.Prim                    as C
import qualified DDC.Core.Compounds                     as C
import qualified DDC.Core.Module                        as C
import qualified DDC.Core.Exp                           as C
import qualified DDC.Type.DataDef                       as C

import qualified DDC.Type.Exp                           as T
import qualified DDC.Type.Sum                           as Sum
import Data.Maybe

import DDC.Core.Module 
        ( ExportSource  (..)
        , ImportType    (..)
        , ImportCap     (..)
        , ImportValue   (..))


---------------------------------------------------------------------------------------------------
type ConvertM a x
        = Either (ErrorConvert a) x


-- | Run a conversion computation.
runConvertM :: ConvertM a x -> Either (ErrorConvert a) x
runConvertM cc = cc

-- | Convert a Source Tetra module to Core Tetra.
coreOfSourceModule 
        :: a 
        -> S.Module (S.Annot a)
        -> Either (ErrorConvert a) (C.Module a C.Name)

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
        :: a 
        -> S.Module (S.Annot a)
        -> ConvertM a (C.Module a C.Name)

coreOfSourceModuleM a mm
 = do   
        -- Exported types and values.
        exportTypes'    <- sequence
                        $  fmap (\n 
                                -> (,)  <$> (pure $ toCoreN n) 
                                        <*> (pure $ ExportSourceLocalNoType (toCoreN n)))
                        $  S.moduleExportTypes mm

        exportValues'   <- sequence
                        $  fmap (\n
                                -> (,)  <$> (pure $ toCoreN n)
                                        <*> (pure $ ExportSourceLocalNoType (toCoreN n)))
                        $  S.moduleExportValues mm

        -- Imported types and values.
        importTypes'    <- sequence
                        $  fmap (\(n, it) 
                                -> (,)  <$> (pure $ toCoreN n) <*> (toCoreImportType it))
                        $  S.moduleImportTypes  mm

        importCaps'     <- sequence 
                        $  fmap (\(n, iv) 
                                -> (,)  <$> (pure $ toCoreN n) <*> (toCoreImportCap iv))
                        $  S.moduleImportCaps   mm

        importValues'   <- sequence 
                        $  fmap (\(n, iv) 
                                -> (,)  <$> (pure $ toCoreN n) <*> (toCoreImportValue iv))
                        $  S.moduleImportValues mm

        -- Data type definitions.
        dataDefsLocal   <- sequence $ fmap toCoreDataDef 
                        $  [ def | S.TopData _ def <- S.moduleTops mm ]

        -- Top level bindings.
        ltsTops         <- letsOfTops $  S.moduleTops mm

        return
         $ C.ModuleCore
                { C.moduleName          = S.moduleName mm
                , C.moduleIsHeader      = False

                , C.moduleExportTypes   = exportTypes'

                , C.moduleExportValues
                   =  exportValues'
                   ++ (if C.isMainModuleName (S.moduleName mm)
                        && (not $ elem (S.NameVar "main") $ S.moduleExportValues mm)
                        then [ ( C.NameVar "main"
                             , ExportSourceLocalNoType (C.NameVar "main"))]
                        else [])

                , C.moduleImportTypes    = importTypes'
                , C.moduleImportCaps     = importCaps'
                , C.moduleImportValues   = importValues'
                , C.moduleImportDataDefs = []
                , C.moduleDataDefsLocal  = dataDefsLocal
                , C.moduleBody           = C.XLet  a ltsTops (C.xUnit a) }


-- | Extract the top-level bindings from some source definitions.
letsOfTops :: [S.Top (S.Annot a)] -> ConvertM a (C.Lets a C.Name)
letsOfTops tops
 = C.LRec <$> (sequence $ mapMaybe bindOfTop tops)


-- | Try to convert a `TopBind` to a top-level binding, 
--   or `Nothing` if it isn't one.
bindOfTop  
        :: S.Top (S.Annot a) 
        -> Maybe (ConvertM a (T.Bind C.Name, C.Exp a C.Name))

bindOfTop (S.TopClause _ (S.SLet _ b [] [S.GExp x]))
 = Just ((,) <$> toCoreB b <*> toCoreX x)

bindOfTop _     
 = Nothing


-- ImportType -------------------------------------------------------------------------------------
toCoreImportType :: ImportType S.Name -> ConvertM a (ImportType C.Name)
toCoreImportType src
 = case src of
        ImportTypeAbstract t    
         -> ImportTypeAbstract <$> toCoreT t

        ImportTypeBoxed t
         -> ImportTypeBoxed    <$> toCoreT t


-- ImportCap --------------------------------------------------------------------------------------
toCoreImportCap :: ImportCap S.Name -> ConvertM a (ImportCap C.Name)
toCoreImportCap src
 = case src of
        ImportCapAbstract t
         -> ImportCapAbstract   <$> toCoreT t


-- ImportValue ------------------------------------------------------------------------------------
toCoreImportValue :: ImportValue S.Name -> ConvertM a (ImportValue C.Name)
toCoreImportValue src
 = case src of
        ImportValueModule mn n t mA
         ->  ImportValueModule 
         <$> (pure mn) <*> (pure $ toCoreN n) <*> toCoreT t <*> pure mA

        ImportValueSea v t
         -> ImportValueSea 
         <$> pure v    <*> toCoreT t


-- Type -------------------------------------------------------------------------------------------
toCoreT :: T.Type S.Name -> ConvertM a (T.Type C.Name)
toCoreT tt
 = case tt of
        T.TVar u
         -> T.TVar <$> toCoreU  u

        T.TCon tc
         -> T.TCon <$> toCoreTC tc

        T.TForall b t
         -> T.TForall <$> toCoreB b  <*> toCoreT t

        T.TApp t1 t2
         -> T.TApp    <$> toCoreT t1 <*> toCoreT t2

        T.TSum ts
         -> do  k'      <- toCoreT $ Sum.kindOfSum ts

                tss'    <- fmap (Sum.fromList k') 
                        $  sequence $ fmap toCoreT $ Sum.toList ts

                return  $ T.TSum tss'


-- TyCon ------------------------------------------------------------------------------------------
toCoreTC :: T.TyCon S.Name -> ConvertM a (T.TyCon C.Name)
toCoreTC tc
 = case tc of
        T.TyConSort sc    
         -> pure $ T.TyConSort sc

        T.TyConKind kc
         -> pure $ T.TyConKind kc

        T.TyConWitness wc
         -> pure $ T.TyConWitness wc

        T.TyConSpec sc
         -> pure $ T.TyConSpec sc

        T.TyConBound u k
         -> T.TyConBound  <$> toCoreU u <*> toCoreT k

        T.TyConExists n k
         -> T.TyConExists <$> pure n    <*> toCoreT k


-- DataDef ----------------------------------------------------------------------------------------
toCoreDataDef :: S.DataDef S.Name -> ConvertM a (C.DataDef C.Name)
toCoreDataDef def
 = do
        defParams       <- sequence $ fmap toCoreB $ S.dataDefParams def

        defCtors        <- sequence $ fmap (\(ctor, tag) -> toCoreDataCtor def tag ctor)
                                    $ [(ctor, tag) | ctor <- S.dataDefCtors def
                                                   | tag  <- [0..]]

        return $ C.DataDef
         { C.dataDefTypeName    = toCoreN     $ S.dataDefTypeName def
         , C.dataDefParams      = defParams
         , C.dataDefCtors       = Just $ defCtors
         , C.dataDefIsAlgebraic = True }


-- DataCtor ---------------------------------------------------------------------------------------
toCoreDataCtor 
        :: S.DataDef S.Name 
        -> Integer
        -> S.DataCtor S.Name 
        -> ConvertM a (C.DataCtor C.Name)

toCoreDataCtor dataDef tag ctor
 = do   typeParams      <- sequence $ fmap toCoreB $ S.dataDefParams dataDef
        fieldTypes      <- sequence $ fmap toCoreT $ S.dataCtorFieldTypes ctor
        resultType      <- toCoreT (S.dataCtorResultType ctor)

        return $ C.DataCtor
         { C.dataCtorName        = toCoreN (S.dataCtorName ctor)
         , C.dataCtorTag         = tag
         , C.dataCtorFieldTypes  = fieldTypes
         , C.dataCtorResultType  = resultType
         , C.dataCtorTypeName    = toCoreN (S.dataDefTypeName dataDef) 
         , C.dataCtorTypeParams  = typeParams }


-- Exp --------------------------------------------------------------------------------------------
toCoreX :: S.Exp a -> ConvertM a (C.Exp a C.Name)
toCoreX xx
 = case xx of
        S.XVar a u      
         -> C.XVar  <$> pure a <*> toCoreU  u

        -- Wrap text literals into Text during conversion to Core.
        -- The 'textLit' variable refers to whatever is in scope.
        S.XCon a dc@(C.DaConPrim (S.NameLitTextLit{}) _)
         -> C.XApp  <$> pure a 
                    <*> (C.XVar <$> pure a <*> (pure $ C.UName (C.NameVar "textLit")))
                    <*> (C.XCon <$> pure a <*> (toCoreDC dc))

        S.XPrim a p
         -> C.XVar  <$> pure a <*> toCoreU (C.UPrim (S.NameVal p) (S.typeOfPrimVal p))

        S.XCon a dc
         -> C.XCon  <$> pure a <*> toCoreDC dc

        S.XLAM a b x
         -> C.XLAM  <$> pure a <*> toCoreB b <*> toCoreX x

        S.XLam a b x
         -> C.XLam  <$> pure a <*> toCoreB b <*> toCoreX x

        S.XApp a x1 x2
         -> C.XApp  <$> pure a <*> toCoreX x1 <*> toCoreX x2

        S.XLet a lts x
         -> C.XLet  <$> pure a <*> toCoreLts lts <*> toCoreX x

        S.XCase a x alts
         -> C.XCase <$> pure a <*> toCoreX x <*> (sequence $ map (toCoreA a) alts)

        S.XCast a c x
         -> C.XCast <$> pure a <*> toCoreC c <*> toCoreX x

        S.XType a t
         -> C.XType    <$> pure a <*> toCoreT t

        S.XWitness a w
         -> C.XWitness <$> pure a <*> toCoreW w

        -- These shouldn't exist in the desugared source tetra code.
        S.XDefix{}      -> Left $ ErrorConvertCannotConvertSugarExp xx
        S.XInfixOp{}    -> Left $ ErrorConvertCannotConvertSugarExp xx
        S.XInfixVar{}   -> Left $ ErrorConvertCannotConvertSugarExp xx


-- Lets -------------------------------------------------------------------------------------------
toCoreLts :: S.Lets a -> ConvertM a (C.Lets a C.Name)
toCoreLts lts
 = case lts of
        S.LLet b x
         -> C.LLet <$> toCoreB b <*> toCoreX x
        
        S.LRec bxs
         -> C.LRec <$> (sequence $ map (\(b, x) -> (,) <$> toCoreB b <*> toCoreX x) bxs)

        S.LPrivate bks Nothing bts
         -> C.LPrivate <$> (sequence $ fmap toCoreB bks) 
                       <*>  pure Nothing 
                       <*> (sequence $ fmap toCoreB bts)

        S.LPrivate bks (Just tParent) bts
         -> C.LPrivate <$> (sequence $ fmap toCoreB bks) 
                       <*> (fmap Just $ toCoreT tParent)
                       <*> (sequence $ fmap toCoreB bts)

        S.LGroup{}
         -> Left $ ErrorConvertCannotConvertSugarLets lts


-- Cast -------------------------------------------------------------------------------------------
toCoreC :: S.Cast a -> ConvertM a (C.Cast a C.Name)
toCoreC cc
 = case cc of
        S.CastWeakenEffect eff
         -> C.CastWeakenEffect <$> toCoreT eff

        S.CastPurify w
         -> C.CastPurify       <$> toCoreW w

        S.CastBox
         -> pure C.CastBox

        S.CastRun
         -> pure C.CastRun


-- Alt --------------------------------------------------------------------------------------------
toCoreA  :: a -> S.Alt a -> ConvertM a (C.Alt a C.Name)
toCoreA a (S.AAlt w gxs)
 = C.AAlt <$> toCoreP w
          <*> (toCoreX (S.desugarGuards a gxs (error "ddc-source-tetra.toCoreA alt fail")))
        -- ISSUE #345: Give pattern inexhaustiveness message.


-- Pat --------------------------------------------------------------------------------------------
toCoreP  :: S.Pat a -> ConvertM a (C.Pat C.Name)
toCoreP pp
 = case pp of
        S.PDefault        
         -> pure C.PDefault
        
        S.PData dc bs
         -> C.PData <$> toCoreDC dc <*> (sequence $ fmap toCoreB bs)


-- DaCon ------------------------------------------------------------------------------------------
toCoreDC :: S.DaCon S.Name -> ConvertM a (C.DaCon C.Name)
toCoreDC dc
 = case dc of
        S.DaConUnit
         -> pure $ C.DaConUnit

        S.DaConPrim n t 
         -> C.DaConPrim  <$> (pure $ toCoreN n) <*> toCoreT t

        S.DaConBound n
         -> C.DaConBound <$> (pure $ toCoreN n)


-- Witness ----------------------------------------------------------------------------------------
toCoreW :: S.Witness a -> ConvertM a (C.Witness a C.Name)
toCoreW ww
 = case ww of
        S.WVar a u
         -> C.WVar  <$> pure a <*> toCoreU  u

        S.WCon a wc
         -> C.WCon  <$> pure a <*> toCoreWC wc

        S.WApp a w1 w2
         -> C.WApp  <$> pure a <*> toCoreW  w1 <*> toCoreW w2

        S.WType a t
         -> C.WType <$> pure a <*> toCoreT  t


-- WiCon ------------------------------------------------------------------------------------------
toCoreWC :: S.WiCon a -> ConvertM a (C.WiCon C.Name)
toCoreWC wc
 = case wc of
        S.WiConBound u t
         -> C.WiConBound <$> toCoreU u <*> toCoreT t


-- Bind -------------------------------------------------------------------------------------------
toCoreB :: S.Bind -> ConvertM a (C.Bind C.Name)
toCoreB bb
 = case bb of
        T.BName n t
         -> T.BName <$> (pure $ toCoreN n) <*> toCoreT t

        T.BAnon t
         -> T.BAnon <$> toCoreT t

        T.BNone t
         -> T.BNone <$> toCoreT t


-- Bound ------------------------------------------------------------------------------------------
toCoreU :: S.Bound -> ConvertM a (C.Bound C.Name)
toCoreU uu
 = case uu of
        T.UName n
         -> T.UName <$> (pure $ toCoreN n)

        T.UIx   i
         -> T.UIx   <$> (pure i)

        T.UPrim n t
         -> T.UPrim <$> (pure $ toCoreN n) <*> toCoreT t


-- Name -------------------------------------------------------------------------------------------
toCoreN :: S.Name -> C.Name
toCoreN nn
 = case nn of
        S.NameVar str
         -> C.NameVar str

        S.NameCon str
         -> C.NameCon str

        S.NamePrim (S.PrimNameType (S.PrimTypeTyConTetra tc))
         -> C.NameTyConTetra (toCoreTyConTetra tc)

        S.NamePrim (S.PrimNameType (S.PrimTypeTyCon p))
         -> C.NamePrimTyCon  p

        S.NamePrim (S.PrimNameVal (S.PrimValLit (S.PrimLitBool    x)))
         -> C.NameLitBool    x

        S.NamePrim (S.PrimNameVal (S.PrimValLit (S.PrimLitNat     x)))
         -> C.NameLitNat     x

        S.NamePrim (S.PrimNameVal (S.PrimValLit (S.PrimLitInt     x)))
         -> C.NameLitInt     x

        S.NamePrim (S.PrimNameVal (S.PrimValLit (S.PrimLitSize    x)))
         -> C.NameLitSize    x

        S.NamePrim (S.PrimNameVal (S.PrimValLit (S.PrimLitWord    x s)))
         -> C.NameLitWord    x s

        S.NamePrim (S.PrimNameVal (S.PrimValLit (S.PrimLitFloat   x s)))
         -> C.NameLitFloat   x s

        S.NamePrim (S.PrimNameVal (S.PrimValLit (S.PrimLitTextLit x)))
         -> C.NameLitTextLit x

        S.NamePrim (S.PrimNameVal (S.PrimValArith p))
         -> C.NamePrimArith  p False

        S.NamePrim (S.PrimNameVal (S.PrimValVector p))
         -> C.NameOpVector   p False

        S.NamePrim (S.PrimNameVal (S.PrimValFun   p))
         -> C.NameOpFun      p

        S.NameHole
         -> C.NameHole


-- | Convert a Tetra specific type constructor to core.
toCoreTyConTetra :: S.PrimTyConTetra -> C.TyConTetra
toCoreTyConTetra tc
 = case tc of
        S.PrimTyConTetraTuple n -> C.TyConTetraTuple n
        S.PrimTyConTetraVector  -> C.TyConTetraVector
        S.PrimTyConTetraF       -> C.TyConTetraF
        S.PrimTyConTetraC       -> C.TyConTetraC
        S.PrimTyConTetraU       -> C.TyConTetraU

