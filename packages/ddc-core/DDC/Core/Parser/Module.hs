{-# OPTIONS -fno-warn-unused-binds #-}
module DDC.Core.Parser.Module
        (pModule)
where
import DDC.Core.Module
import DDC.Core.Exp
import DDC.Core.Parser.Type
import DDC.Core.Parser.Exp
import DDC.Core.Parser.Context
import DDC.Core.Parser.Base
import DDC.Core.Lexer.Tokens
import DDC.Core.Compounds
import DDC.Type.DataDef
import DDC.Base.Pretty
import Control.Monad
import qualified DDC.Base.Parser        as P


-- Module -----------------------------------------------------------------------------------------
-- | Parse a core module.
pModule :: (Ord n, Pretty n) 
        => Context n
        -> Parser n (Module P.SourcePos n)
pModule c
 = do   sp      <- pTokSP KModule
        name    <- pModuleName

        -- Export definitions.
        tExports        <- liftM concat $ P.many (pExportSpecs c)

        -- Import definitions.
        tImports        <- liftM concat $ P.many (pImportSpecs c)

        -- Data definitions defined in the current module.
        dataDefsLocal   <- P.many (pDataDef c)

        -- Function definitions.
        --  If there is a 'with' keyword then this is a standard module with bindings.
        --  If not, then it is a module header, which doesn't need bindings.
        (lts, isHeader) <- P.choice
                        [ do  pTok KWith

                              -- LET;+
                              lts  <- P.sepBy1 (pLetsSP c) (pTok KIn)
                              return (lts, False)

                        , do  return ([],  True) ]

        -- The body of the module consists of the top-level bindings wrapped
        -- around a unit constructor place-holder.
        let body = xLetsAnnot lts (xUnit sp)

        return  $ ModuleCore
                { moduleName            = name
                , moduleIsHeader        = isHeader
                , moduleExportTypes     = []
                , moduleExportValues    = [(n, s) | ExportValue n s <- tExports]
                , moduleImportTypes     = [(n, s) | ImportType  n s <- tImports]
                , moduleImportValues    = [(n, s) | ImportValue n s <- tImports]
                , moduleImportDataDefs  = [def    | ImportData  def <- tImports]
                , moduleDataDefsLocal   = dataDefsLocal
                , moduleBody            = body }


---------------------------------------------------------------------------------------------------
data ExportSpec n
        = ExportValue   n (ExportSource n)


-- | Parse some export specs.
pExportSpecs
        :: (Ord n, Pretty n)
        => Context n -> Parser n [ExportSpec n]

pExportSpecs c
 = do   pTok KExport

        P.choice 
         [      -- export value { (NAME :: TYPE)+ }
           do   P.choice [ pTok KValue, return () ]
                pTok KBraceBra
                specs   <- P.sepEndBy1 (pExportValue c) (pTok KSemiColon)
                pTok KBraceKet 
                return specs

                -- export foreign X value { (NAME :: TYPE)+ }
         , do   pTok KForeign
                dst     <- liftM (renderIndent . ppr) pName
                pTok KValue
                pTok KBraceBra
                specs   <- P.sepEndBy1 (pExportForeignValue c dst) (pTok KSemiColon)
                pTok KBraceKet
                return specs
         ]


-- | Parse an export spec.
pExportValue
        :: (Ord n, Pretty n)
        => Context n -> Parser n (ExportSpec n)
pExportValue c 
 = do   
        n       <- pName
        pTokSP (KOp ":")
        t       <- pType c
        return  (ExportValue n (ExportSourceLocal n t))


-- | Parse a foreign value export spec.
pExportForeignValue    
        :: (Ord n, Pretty n)
        => Context n -> String -> Parser n (ExportSpec n)
pExportForeignValue c dst
        | "c"           <- dst
        = do    n       <- pName
                pTokSP (KOp ":")
                k       <- pType c

                -- ISSUE #327: Allow external symbol to be specified 
                --             with foreign C imports and exports.
                return  (ExportValue n (ExportSourceLocal n k))

        | otherwise
        = P.unexpected "export mode for foreign value."


---------------------------------------------------------------------------------------------------
-- | An imported foreign type or foreign value.
data ImportSpec n
        = ImportType    n (ImportSource n)
        | ImportValue   n (ImportSource n)
        | ImportData    (DataDef n)
        

-- | Parse some import specs.
pImportSpecs    :: (Ord n, Pretty n)
                => Context n -> Parser n [ImportSpec n]
pImportSpecs c
 = do   
        pTok KImport

        P.choice
         [      -- import type  { (NAME :: TYPE)+ }
           do   pTok KType
                pTok KBraceBra
                specs   <- P.sepEndBy1 (pImportType c) (pTok KSemiColon)
                pTok KBraceKet
                return specs

         , do   def     <- pDataDef c
                return  [ ImportData def ]

                -- import value { (NAME :: TYPE)+ }
         , do   P.choice [ pTok KValue, return () ]
                pTok KBraceBra
                specs   <- P.sepEndBy1 (pImportValue c) (pTok KSemiColon)
                pTok KBraceKet
                return specs

         , do   pTok KForeign
                src     <- liftM (renderIndent . ppr) pName

                P.choice
                 [      -- import foreign X type { (NAME :: TYPE)+ }
                  do    pTok KType
                        pTok KBraceBra
                        sigs <- P.sepEndBy1 (pImportForeignType c src) (pTok KSemiColon)
                        pTok KBraceKet
                        return sigs
        
                        -- imports foreign X value { (NAME :: TYPE)+ }
                 , do   pTok KValue
                        pTok KBraceBra
                        sigs <- P.sepEndBy1 (pImportForeignValue c src) (pTok KSemiColon)
                        pTok KBraceKet
                        return sigs
                 ]
         ]
         P.<?> "something to import"


-- | Parse a type import spec.
pImportType
        :: (Ord n, Pretty n)
        => Context n -> Parser n (ImportSpec n)
pImportType c
 = do   n       <- pName
        pTokSP (KOp ":")
        k       <- pType c
        return  $ ImportType n (ImportSourceModule (ModuleName []) n k Nothing)


-- | Parse a foreign type import spec.
pImportForeignType
        :: (Ord n, Pretty n) 
        => Context n -> String -> Parser n (ImportSpec n)
pImportForeignType c src

        -- Abstract types are not associated with data values,
        -- they can be used as phantom type parameters, 
        -- or have a kind of something that is not Data.
        | "abstract"    <- src
        = do    n       <- pName
                pTokSP (KOp ":")
                k       <- pType c
                return  (ImportType n (ImportSourceAbstract k))

        -- Boxed types are associate with values that follow the standard
        -- heap object layout. They can be passed and return from functions.
        | "boxed"       <- src
        = do    n       <- pName
                pTokSP (KOp ":")
                k       <- pType c
                return  (ImportType n (ImportSourceBoxed k))

        | otherwise
        = P.unexpected "import mode for foreign type."


-- | Parse a value import spec.
pImportValue
        :: (Ord n, Pretty n)
        => Context n -> Parser n (ImportSpec n)
pImportValue c
 = do   n       <- pName
        pTokSP (KOp ":")
        t       <- pType c
        return  (ImportValue n (ImportSourceModule (ModuleName []) n t Nothing))


-- | Parse a foreign value import spec.
pImportForeignValue    
        :: (Ord n, Pretty n)
        => Context n -> String -> Parser n (ImportSpec n)
pImportForeignValue c src
        | "c"           <- src
        = do    n       <- pName
                pTokSP (KOp ":")
                k       <- pType c

                -- ISSUE #327: Allow external symbol to be specified 
                --             with foreign C imports and exports.
                let symbol = renderIndent (ppr n)

                return  (ImportValue n (ImportSourceSea symbol k))

        | otherwise
        = P.unexpected "import mode for foreign value."


-- DataDef ----------------------------------------------------------------------------------------
pDataDef :: Ord n => Context n -> Parser n (DataDef n)
pDataDef c
 = do   pTokSP KData
        nData   <- pName 
        bsParam <- liftM concat $ P.many (pDataParam c)

        P.choice
         [ -- Data declaration with constructors that have explicit types.
           do   pTok KWhere
                pTok KBraceBra
                ctors      <- P.sepEndBy1 (pDataCtor c nData bsParam) (pTok KSemiColon)
                let ctors' = [ ctor { dataCtorTag = tag }
                                | ctor <- ctors
                                | tag  <- [0..] ]
                pTok KBraceKet
                return  $ DataDef 
                        { dataDefTypeName       = nData
                        , dataDefParams         = bsParam 
                        , dataDefCtors          = Just ctors'
                        , dataDefIsAlgebraic    = True }
         
           -- Data declaration with no data constructors.
         , do   return  $ DataDef 
                        { dataDefTypeName       = nData
                        , dataDefParams         = bsParam
                        , dataDefCtors          = Just []
                        , dataDefIsAlgebraic    = True }

         ]


-- | Parse a type parameter to a data type.
pDataParam :: Ord n => Context n -> Parser n [Bind n]
pDataParam c 
 = do   pTok KRoundBra
        ns      <- P.many1 pName
        pTokSP (KOp ":")
        k       <- pType c
        pTok KRoundKet
        return  [BName n k | n <- ns]


-- | Parse a data constructor declaration.
pDataCtor 
        :: Ord n 
        => Context n
        -> n                    -- ^ Name of data type constructor.
        -> [Bind n]             -- ^ Type parameters of data type constructor.
        -> Parser n (DataCtor n)

pDataCtor c nData bsParam
 = do   n       <- pName
        pTokSP (KOp ":")
        t       <- pType c
        let (tsArg, tResult)    
                = takeTFunArgResult t

        return  $ DataCtor
                { dataCtorName          = n

                -- Set tag to 0 for now. We fix this up in pDataDef above.
                , dataCtorTag           = 0
                
                , dataCtorFieldTypes    = tsArg
                , dataCtorResultType    = tResult 
                , dataCtorTypeName      = nData 
                , dataCtorTypeParams    = bsParam }

