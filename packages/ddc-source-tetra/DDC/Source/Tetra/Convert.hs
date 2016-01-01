
-- | Source Tetra conversion to Disciple Core Tetra language.
module DDC.Source.Tetra.Convert
        ( ConvertM
        , ErrorConvert (..)
        , coreOfSourceModule
        , runConvertM
        )
where
import qualified DDC.Source.Tetra.Transform.Guards      as S
import qualified DDC.Source.Tetra.Module                as S
import qualified DDC.Source.Tetra.DataDef               as S
import qualified DDC.Source.Tetra.Exp                   as S
import qualified DDC.Source.Tetra.Prim                  as S

import qualified DDC.Core.Tetra.Prim                    as C
import qualified DDC.Core.Compounds                     as C
import qualified DDC.Core.Module                        as C
import qualified DDC.Core.Exp                           as C
import qualified DDC.Type.DataDef                       as C

import qualified DDC.Type.Sum                           as Sum
import Data.Maybe

-- Things shared between both Source and Core languages.
import DDC.Core.Exp
        ( Bind          (..)
        , Bound         (..)
        , Type          (..)
        , TyCon         (..)
        , Pat           (..)
        , DaCon         (..)
        , Witness       (..)
        , WiCon         (..))

import DDC.Core.Module 
        ( ExportSource  (..)
        , ImportType    (..)
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
        -> S.Module a S.Name
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
        -> S.Module a S.Name 
        -> ConvertM a (C.Module a C.Name)

coreOfSourceModuleM a mm
 = do   
        -- Exported types and values.
        exportTypes'    <- sequence
                        $  fmap (\n -> (,) <$> (pure $ toCoreN n) 
                                           <*> (pure $ ExportSourceLocalNoType (toCoreN n)))
                        $  S.moduleExportTypes mm

        exportValues'   <- sequence
                        $  fmap (\n -> (,) <$> (pure $ toCoreN n)
                                           <*> (pure $ ExportSourceLocalNoType (toCoreN n)))
                        $  S.moduleExportValues mm

        -- Imported types and values.
        importTypes'    <- sequence
                        $  fmap (\(n, it) -> (,) <$> (pure $ toCoreN n) <*> (toCoreImportType it))
                        $  S.moduleImportTypes mm

        importValues'   <- sequence 
                        $  fmap (\(n, iv) -> (,) <$> (pure $ toCoreN n) <*> (toCoreImportValue iv))
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
                , C.moduleImportValues   = importValues'
                , C.moduleImportDataDefs = []
                , C.moduleDataDefsLocal  = dataDefsLocal
                , C.moduleBody           = C.XLet  a ltsTops (C.xUnit a) }


-- | Extract the top-level bindings from some source definitions.
letsOfTops :: [S.Top a S.Name] -> ConvertM a (C.Lets a C.Name)
letsOfTops tops
 = C.LRec <$> (sequence $ mapMaybe bindOfTop tops)


-- | Try to convert a `TopBind` to a top-level binding, 
--   or `Nothing` if it isn't one.
bindOfTop  
        :: S.Top a S.Name 
        -> Maybe (ConvertM a (Bind C.Name, C.Exp a C.Name))

bindOfTop (S.TopClause _ (S.SLet _ b [] [S.GExp x]))
 = Just ((,) <$> toCoreB b <*> toCoreX x)

bindOfTop _     
 = Nothing


-- ImportType -------------------------------------------------------------------------------------
toCoreImportType :: ImportType S.Name -> ConvertM a (ImportType C.Name)
toCoreImportType src
 = case src of
        ImportTypeAbstract t    -> ImportTypeAbstract <$> toCoreT t
        ImportTypeBoxed t       -> ImportTypeBoxed    <$> toCoreT t


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
toCoreT :: Type S.Name -> ConvertM a (Type C.Name)
toCoreT tt
 = case tt of
        TVar u
         -> TVar <$> toCoreU  u

        TCon tc
         -> TCon <$> toCoreTC tc

        TForall b t
         -> TForall <$> toCoreB b  <*> toCoreT t

        TApp t1 t2
         -> TApp    <$> toCoreT t1 <*> toCoreT t2

        TSum ts
         -> do  k'      <- toCoreT $ Sum.kindOfSum ts

                tss'    <- fmap (Sum.fromList k') 
                        $  sequence $ fmap toCoreT $ Sum.toList ts

                return  $ TSum tss'


-- TyCon ------------------------------------------------------------------------------------------
toCoreTC :: TyCon S.Name -> ConvertM a (TyCon C.Name)
toCoreTC tc
 = case tc of
        TyConSort sc    
         -> pure $ TyConSort sc

        TyConKind kc
         -> pure $ TyConKind kc

        TyConWitness wc
         -> pure $ TyConWitness wc

        TyConSpec sc
         -> pure $ TyConSpec sc

        TyConBound u k
         -> TyConBound  <$> toCoreU u <*> toCoreT k

        TyConExists n k
         -> TyConExists <$> pure n    <*> toCoreT k


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
         , C.dataDefIsAlgebraic = True
         }


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
toCoreX :: S.Exp a S.Name -> ConvertM a (C.Exp a C.Name)
toCoreX xx
 = case xx of
        S.XVar a u      
         -> C.XVar  <$> pure a <*> toCoreU  u

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
toCoreLts :: S.Lets a S.Name -> ConvertM a (C.Lets a C.Name)
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
toCoreC :: S.Cast a S.Name -> ConvertM a (C.Cast a C.Name)
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
toCoreA  :: a -> S.Alt a S.Name -> ConvertM a (C.Alt a C.Name)
toCoreA a (S.AAlt w gxs)
 = C.AAlt <$> toCoreP w
          <*> (toCoreX (S.desugarGuards a gxs (error "toCoreA alt fail")))
                -- TODO: need pattern inexhaustiveness message.


-- Pat --------------------------------------------------------------------------------------------
toCoreP  :: Pat S.Name -> ConvertM a (Pat C.Name)
toCoreP pp
 = case pp of
        PDefault        
         -> pure PDefault
        
        PData dc bs
         -> PData <$> toCoreDC dc <*> (sequence $ fmap toCoreB bs)


-- DaCon ------------------------------------------------------------------------------------------
toCoreDC :: DaCon S.Name -> ConvertM a (DaCon C.Name)
toCoreDC dc
 = case dc of
        DaConUnit
         -> pure $ DaConUnit

        DaConPrim n t 
         -> DaConPrim  <$> (pure $ toCoreN n) <*> toCoreT t

        DaConBound n
         -> DaConBound <$> (pure $ toCoreN n)


-- Witness ----------------------------------------------------------------------------------------
toCoreW :: Witness a S.Name -> ConvertM a (Witness a C.Name)
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
toCoreWC :: WiCon S.Name -> ConvertM a (WiCon C.Name)
toCoreWC wc
 = case wc of
        WiConBound u t
         -> WiConBound <$> toCoreU u <*> toCoreT t


-- Bind -------------------------------------------------------------------------------------------
toCoreB :: Bind S.Name -> ConvertM a (Bind C.Name)
toCoreB bb
 = case bb of
        BName n t
         -> BName <$> (pure $ toCoreN n) <*> toCoreT t

        BAnon t
         -> BAnon <$> toCoreT t

        BNone t
         -> BNone <$> toCoreT t


-- Bound ------------------------------------------------------------------------------------------
toCoreU :: Bound S.Name -> ConvertM a (Bound C.Name)
toCoreU uu
 = case uu of
        UName n
         -> UName <$> (pure $ toCoreN n)

        UIx   i
         -> UIx   <$> (pure i)

        UPrim n t
         -> UPrim <$> (pure $ toCoreN n) <*> toCoreT t


-- Name -------------------------------------------------------------------------------------------
toCoreN :: S.Name -> C.Name
toCoreN nn
 = case nn of
        S.NameVar        str -> C.NameVar        str
        S.NameCon        str -> C.NameCon        str
        S.NameTyConTetra tc  -> C.NameTyConTetra (toCoreTyConTetra tc)
        S.NameOpFun      tc  -> C.NameOpFun      tc
        S.NamePrimTyCon  p   -> C.NamePrimTyCon  p
        S.NamePrimArith  p   -> C.NamePrimArith  p
        S.NameHole           -> C.NameHole

        S.NamePrimVal (S.PrimValLit (S.PrimLitBool    x))   -> C.NameLitBool    x
        S.NamePrimVal (S.PrimValLit (S.PrimLitNat     x))   -> C.NameLitNat     x
        S.NamePrimVal (S.PrimValLit (S.PrimLitInt     x))   -> C.NameLitInt     x
        S.NamePrimVal (S.PrimValLit (S.PrimLitSize    x))   -> C.NameLitSize    x
        S.NamePrimVal (S.PrimValLit (S.PrimLitWord    x s)) -> C.NameLitWord    x s
        S.NamePrimVal (S.PrimValLit (S.PrimLitFloat   x s)) -> C.NameLitFloat   x s
        S.NamePrimVal (S.PrimValLit (S.PrimLitTextLit x))   -> C.NameLitTextLit x


-- | Convert a Tetra specific type constructor to core.
toCoreTyConTetra :: S.TyConTetra -> C.TyConTetra
toCoreTyConTetra tc
 = case tc of
        S.TyConTetraTuple n  -> C.TyConTetraTuple n
        S.TyConTetraF        -> C.TyConTetraF
        S.TyConTetraC        -> C.TyConTetraC
        S.TyConTetraU        -> C.TyConTetraU
        S.TyConTetraTextLit  -> C.TyConTetraTextLit


-- Error ------------------------------------------------------------------------------------------
data ErrorConvert a
        -- | Cannot convert sugar expression to core.
        = ErrorConvertCannotConvertSugarExp  (S.Exp a S.Name)

        -- | Cannot convert sugar let bindings to core.
        | ErrorConvertCannotConvertSugarLets (S.Lets a S.Name)

