
-- | Pretty printing for core modules and expressions.
module DDC.Core.Pretty 
        ( module DDC.Type.Pretty
        , module DDC.Base.Pretty)
where
import DDC.Core.Compounds
import DDC.Core.Predicates
import DDC.Core.Module
import DDC.Core.Exp
import DDC.Type.DataDef
import DDC.Type.Pretty
import DDC.Base.Pretty
import Data.List
import qualified Data.Map.Strict        as Map


-- ModuleName -----------------------------------------------------------------
instance Pretty ModuleName where
 ppr (ModuleName parts)
        = text $ intercalate "." parts


-- Module ---------------------------------------------------------------------
instance (Pretty n, Eq n) => Pretty (Module a n) where
 ppr ModuleCore 
        { moduleName            = name
        , moduleExportKinds     = exportKinds
        , moduleExportTypes     = exportTypes
        , moduleImportKinds     = importKinds
        , moduleImportTypes     = importTypes
        , moduleDataDefsLocal   = localData
        , moduleBody            = body }
  = {-# SCC "ppr[Module]" #-}
    let 
        (lts, _)         = splitXLets body

        -- Exports --------------------
        docsExportKinds
         | Map.null exportKinds        = empty
         | otherwise  
         = nest 8 $ line 
         <> vcat  [ text "type" <+> ppr n <+> text "::" <+> ppr t <> semi
                  | (n, t)      <- Map.toList exportKinds ]

        docsExportTypes  
         | Map.null exportTypes        = empty
         | otherwise
         = nest 8 $ line
         <> vcat  [ ppr n                 <+> text "::" <+> ppr t <> semi
                  | (n, t)      <- Map.toList exportTypes ]

        -- Imports --------------------
        docsImportKinds
         | Map.null importKinds        = empty
         | otherwise  
         = nest 8 $ line 
         <> vcat  [ text "type" <+> ppr n <+> text "::" <+> ppr t <> semi
                  | (n, (_, t)) <- Map.toList importKinds ]

        docsImportTypes  
         | Map.null importTypes        = empty
         | otherwise
         = nest 8 $ line
         <> vcat  [ ppr n                 <+> text "::" <+> ppr t <> semi
                  | (n, (_, t)) <- Map.toList importTypes ]

        -- Local Data Definitions -----
        docsLocalData
         | Map.null localData = empty
         | otherwise
         = line
         <> vsep  [ ppr def
                  | def <- Map.elems localData ]

    in  text "module" <+> ppr name 
         <+> (if Map.null exportKinds && Map.null exportTypes
                then empty
                else line
                        <> text "exports" <+> lbrace
                        <> docsExportKinds
                        <> docsExportTypes
                        <> line 
                        <> rbrace <> space)

         <>  (if Map.null importKinds && Map.null importTypes
                then empty
                else line 
                        <> text "imports" <+> lbrace 
                        <> docsImportKinds
                        <> docsImportTypes
                        <> line 
                        <> rbrace <> space)
         <> docsLocalData
         <> text "with" <$$> (vcat $ map ppr lts)


-- DataDef --------------------------------------------------------------------
instance (Pretty n, Eq n) => Pretty (DataDef n) where
 pprPrec _ def
  = {-# SCC "ppr[DataDef]" #-}
      (text "data" 
        <+> ppr (dataDefTypeName def)
        <+> hsep (map (parens . ppr) (dataDefParams def))
        <+> text "where"
        <+>  lbrace)
  <$> (case dataDefCtors def of
        Just ctors
         -> indent 8
          $ vcat [ ppr ctor <> semi | ctor <- ctors]

        Nothing
         -> text "LARGE")
  <> line
  <> rbrace
  <> line


-- DataCtor -------------------------------------------------------------------
instance (Pretty n, Eq n) => Pretty (DataCtor n) where
 pprPrec _ ctor
        =   ppr (dataCtorName ctor)
        <+> text ":"
        <+> (hsep $ punctuate (text " ->") 
                  $ (map (pprPrec 6) 
                        (  dataCtorFieldTypes ctor
                        ++ [dataCtorResultType ctor])))
  

-- Exp ------------------------------------------------------------------------
instance (Pretty n, Eq n) => Pretty (Exp a n) where
 pprPrec d xx
  = {-# SCC "ppr[Exp]" #-}
    case xx of
        XVar  _ u       -> ppr u
        XCon  _ dc      -> ppr dc
        
        XLAM{}
         -> let Just (bs, xBody) = takeXLAMs xx
                groups = partitionBindsByType bs
            in  pprParen' (d > 1)
                 $  (cat $ map (pprBinderGroup (text "/\\")) groups)
                 <>  (if      isXLAM    xBody then empty
                      else if isXLam    xBody then line <> space
                      else if isSimpleX xBody then space
                      else    line)
                 <>  ppr xBody

        XLam{}
         -> let Just (bs, xBody) = takeXLams xx
                groups = partitionBindsByType bs
            in  pprParen' (d > 1)
                 $  (cat $ map (pprBinderGroup (text "\\")) groups) 
                 <> breakWhen (not $ isSimpleX xBody)
                 <> ppr xBody

        XApp _ x1 x2
         -> pprParen' (d > 10)
         $  pprPrec 10 x1 
                <> nest 4 (breakWhen (not $ isSimpleX x2) 
                           <> pprPrec 11 x2)

        XLet _ lts x
         ->  pprParen' (d > 2)
         $   ppr lts <+> text "in"
         <$> ppr x

        XCase _ x1 [AAlt p x2]
         ->  pprParen' (d > 2)
         $   text "letcase" <+> ppr p 
                <+> nest 2 (breakWhen (not $ isSimpleX x1)
                            <> text "=" <+> align (ppr x1))
                <+> text "in"
         <$> ppr x2

        XCase _ x alts
         -> pprParen' (d > 2) 
         $  (nest 2 $ text "case" <+> ppr x <+> text "of" <+> lbrace <> line
                <> (vcat $ punctuate semi $ map ppr alts))
         <> line 
         <> rbrace

        XCast _ CastSuspend x
         -> pprParen' (d > 2)
         $  text "suspend" <$> ppr x

        XCast _ CastRun x
         -> pprParen' (d > 2)
         $  text "run"     <+> ppr x

        XCast _ cc x
         ->  pprParen' (d > 2)
         $   ppr cc <+> text "in"
         <$> ppr x

        XType    t      -> text "[" <> ppr t <> text "]"
        XWitness w      -> text "<" <> ppr w <> text ">"


-- Pat ------------------------------------------------------------------------
instance (Pretty n, Eq n) => Pretty (Pat n) where
 ppr pp
  = case pp of
        PDefault        -> text "_"
        PData u bs      -> ppr u <+> sep (map pprPatBind bs)


-- | Pretty print a binder, 
--   showing its type annotation only if it's not bottom.
pprPatBind :: (Eq n, Pretty n) => Bind n -> Doc
pprPatBind b
        | isBot (typeOfBind b)  = ppr $ binderOfBind b
        | otherwise             = parens $ ppr b


-- Alt ------------------------------------------------------------------------
instance (Pretty n, Eq n) => Pretty (Alt a n) where
 ppr (AAlt p x)
  = ppr p <+> nest 1 (line <> nest 3 (text "->" <+> ppr x))


-- DaCon ----------------------------------------------------------------------
instance (Pretty n, Eq n) => Pretty (DaCon n) where
 ppr dc
  = case dc of
        DaConUnit               -> text "()"
        DaConPrim  n _ _        -> ppr n
        DaConBound n            -> ppr n


-- Cast -----------------------------------------------------------------------
instance (Pretty n, Eq n) => Pretty (Cast a n) where
 ppr cc
  = case cc of
        CastWeakenEffect  eff   
         -> text "weakeff" <+> brackets (ppr eff)

        CastWeakenClosure xs
         -> text "weakclo" 
         <+> braces (hcat $ punctuate (semi <> space) 
                          $ map ppr xs)

        CastPurify w
         -> text "purify"  <+> angles   (ppr w)

        CastForget w
         -> text "forget"  <+> angles   (ppr w)

        CastSuspend
         -> text "suspend"

        CastRun
         -> text "run"


-- Lets -----------------------------------------------------------------------
instance (Pretty n, Eq n) => Pretty (Lets a n) where
 ppr lts
  = case lts of
        LLet b x
         -> let dBind = if isBot (typeOfBind b)
                          then ppr (binderOfBind b)
                          else ppr b
            in  text "let"
                 <+> align (  dBind
                           <> nest 2 ( breakWhen (not $ isSimpleX x)
                                     <> text "=" <+> align (ppr x)))

        LRec bxs
         -> let pprLetRecBind (b, x)
                 =   ppr (binderOfBind b)
                 <+> text ":"
                 <+> ppr (typeOfBind b)
                 <>  nest 2 (  breakWhen (not $ isSimpleX x)
                            <> text "=" <+> align (ppr x))
        
           in   (nest 2 $ text "letrec"
                  <+> lbrace 
                  <>  (  line 
                      <> (vcat $ punctuate (semi <> line)
                               $ map pprLetRecBind bxs)))
                <$> rbrace

        
        LLetRegions [b] []
         -> text "letregion"
                <+> ppr (binderOfBind b)
        
        LLetRegions [b] bs
         -> text "letregion"
                <+> ppr (binderOfBind b)
                <+> text "with"
                <+> braces (cat $ punctuate (text "; ") $ map ppr bs)

        LLetRegions b []
         -> text "letregions"
                <+> (hcat $ punctuate space (map (ppr . binderOfBind) b))

        LLetRegions b bs
         -> text "letregions"
                <+> (hcat $ punctuate space (map (ppr . binderOfBind) b))
                <+> text "with"
                <+> braces (cat $ punctuate (text "; ") $ map ppr bs)

        LWithRegion b
         -> text "withregion"
                <+> ppr b


-- Witness --------------------------------------------------------------------
instance (Pretty n, Eq n) => Pretty (Witness a n) where
 pprPrec d ww
  = case ww of
        WVar _ n         -> ppr n
        WCon _ wc        -> ppr wc

        WApp _ w1 w2
         -> pprParen (d > 10) (ppr w1 <+> pprPrec 11 w2)
         
        WJoin _ w1 w2
         -> pprParen (d > 9)  (ppr w1 <+> text "&" <+> ppr w2)

        WType _ t        -> text "[" <> ppr t <> text "]"


instance (Pretty n, Eq n) => Pretty (WiCon n) where
 ppr wc
  = case wc of
        WiConBuiltin wb   -> ppr wb
        WiConBound   u  _ -> ppr u


instance Pretty WbCon where
 ppr wb
  = case wb of
        WbConPure       -> text "pure"
        WbConEmpty      -> text "empty"
        WbConUse        -> text "use"
        WbConRead       -> text "read"
        WbConAlloc      -> text "alloc"


-- Binder ---------------------------------------------------------------------
pprBinder   :: Pretty n => Binder n -> Doc
pprBinder bb
 = case bb of
        RName v         -> ppr v
        RAnon           -> text "^"
        RNone           -> text "_"


-- | Print a group of binders with the same type.
pprBinderGroup 
        :: (Pretty n, Eq n) 
        => Doc -> ([Binder n], Type n) -> Doc

pprBinderGroup lam (rs, t)
        = lam <> parens ((hsep $ map pprBinder rs) <+> text ":" <+> ppr t) <> dot


-- Utils ----------------------------------------------------------------------
breakWhen :: Bool -> Doc
breakWhen True   = line
breakWhen False  = space


isSimpleX :: Exp a n -> Bool
isSimpleX xx
 = case xx of
        XVar{}          -> True
        XCon{}          -> True
        XType{}         -> True
        XWitness{}      -> True
        XApp _ x1 x2    -> isSimpleX x1 && isAtomX x2
        _               -> False


parens' :: Doc -> Doc
parens' d = lparen <> nest 1 d <> rparen


-- | Wrap a `Doc` in parens if the predicate is true.
pprParen' :: Bool -> Doc -> Doc
pprParen' b c
 = if b then parens' c
        else c
