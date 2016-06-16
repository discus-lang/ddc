
-- | Parser for Source Tetra modules.
module DDC.Source.Tetra.Parser.Module
        ( -- * Modules
          pModule
        , pTypeSig
        
          -- * Top-level things
        , pTop)
where
import DDC.Source.Tetra.Parser.Exp      as S
import DDC.Source.Tetra.Parser.Base     as S
import DDC.Source.Tetra.Exp.Source      as S
import DDC.Source.Tetra.Compounds       as S
import DDC.Source.Tetra.DataDef         as S
import DDC.Source.Tetra.Module          as S
import DDC.Core.Lexer.Tokens            as K
import DDC.Base.Pretty
import Control.Monad
import qualified DDC.Base.Parser        as P
import qualified Data.Text              as Text

import DDC.Core.Parser
        ( pModuleName
        , pName)


-- Module -----------------------------------------------------------------------------------------
-- | Parse a source tetra module.
pModule :: Parser (Module Source)
pModule 
 = do   
        _sp     <- pTokSP KModule
        name    <- pModuleName <?> "a module name"

        -- export { VAR;+ }
        tExports 
         <- P.choice
            [do pTok KExport
                pTok KBraceBra
                vars    <- P.sepEndBy1 pBoundName (pTok KSemiColon)
                pTok KBraceKet
                return vars

            ,   return []]

        -- import { SIG;+ }
        tImports
         <- liftM concat $ P.many pImportSpecs

        -- top-level declarations.
        tops    
         <- P.choice
            [do pTok KWhere
                pTok KBraceBra

                -- TOP;+
                tops    <- P.sepEndBy pTop (pTok KSemiColon)

                pTok KBraceKet
                return tops

            ,do return [] ]


        -- ISSUE #295: Check for duplicate exported names in module parser.
        --  The names are added to a unique map, so later ones with the same
        --  name will replace earlier ones.
        return  $ Module
                { moduleName            = name
                , moduleExportTypes     = []
                , moduleExportValues    = tExports
                , moduleImportModules   = [mn     | ImportModule mn  <- tImports]
                , moduleImportTypes     = [(n, s) | ImportType  n s  <- tImports]
                , moduleImportCaps      = [(n, s) | ImportCap   n s  <- tImports]
                , moduleImportValues    = [(n, s) | ImportValue n s  <- tImports]
                , moduleTops            = tops }


-- | Parse a type signature.
pTypeSig :: Parser (Bind, Type)
pTypeSig 
 = do   (b, _)  <- pBindNameSP
        pTokSP (KOp ":")
        t       <- pType
        return  (b, t)


---------------------------------------------------------------------------------------------------
-- | An imported foreign type or foreign value.
data ImportSpec 
        = ImportModule  ModuleName
        | ImportType    TyConBind (ImportType  TyConBind Type)
        | ImportCap     Bind      (ImportCap   Bind      Type)
        | ImportValue   Bind      (ImportValue Bind      Type)
        deriving Show
        

-- | Parse some import specs.
pImportSpecs :: Parser [ImportSpec]
pImportSpecs
 = do   pTok KImport

        P.choice
                -- import foreign ...
         [ do   pTok KForeign
                src    <- liftM (renderIndent . ppr) pName

                P.choice
                 [      -- import foreign X type (NAME :: TYPE)+ 
                  do    pTok KType
                        pTok KBraceBra

                        sigs <- P.sepEndBy1 (pImportType src) (pTok KSemiColon)
                        pTok KBraceKet
                        return sigs

                        -- import foreign X capability (NAME :: TYPE)+
                 , do   pTok KCapability
                        pTok KBraceBra

                        sigs <- P.sepEndBy1 (pImportCapability src) (pTok KSemiColon)
                        pTok KBraceKet
                        return sigs

                        -- import foreign X value (NAME :: TYPE)+
                 , do   pTok KValue
                        pTok KBraceBra

                        sigs <- P.sepEndBy1 (pImportValue src) (pTok KSemiColon)
                        pTok KBraceKet
                        return sigs
                 ]

         , do   pTok KBraceBra
                names   <- P.sepEndBy1 pModuleName (pTok KSemiColon) 
                                <?> "module names"
                pTok KBraceKet
                return  [ImportModule n | n <- names]
         ]


-- | Parse a type import spec.
pImportType :: String -> Parser ImportSpec
pImportType src
        | "abstract"    <- src
        = do    b       <- pTyConBindName
                pTokSP (KOp ":")
                k       <- pType
                return  (ImportType b (ImportTypeAbstract k))

        | "boxed"       <- src
        = do    b       <- pTyConBindName
                pTokSP (KOp ":")
                k       <- pType
                return  (ImportType b (ImportTypeBoxed k))

        | otherwise
        = P.unexpected "import mode for foreign type"


-- | Parse a capability import.
pImportCapability :: String -> Parser ImportSpec
pImportCapability src
        | "abstract"    <- src
        = do    (b, _)  <- pBindNameSP
                pTokSP (KOp ":")
                t       <- pType
                return  (ImportCap b (ImportCapAbstract t))

        | otherwise
        = P.unexpected "import mode for foreign capability"


-- | Parse a value import spec.
pImportValue :: String -> Parser ImportSpec
pImportValue src
        | "c"           <- src
        = do    (b@(BName n), _)  <- pBindNameSP
                pTokSP (KOp ":")
                k       <- pType

                -- ISSUE #327: Allow external symbol to be specified 
                --             with foreign C imports and exports.
                let symbol = renderIndent (text $ Text.unpack n)

                return  (ImportValue b (ImportValueSea symbol k))

        | otherwise
        = P.unexpected "import mode for foreign value"


-- Top Level --------------------------------------------------------------------------------------
pTop    :: Parser (Top Source)
pTop
 = P.choice
 [ do   -- A top-level, possibly recursive binding.
        (l, sp)         <- pClauseSP
        return  $ TopClause sp l
 
        -- A data type declaration
 , do   pData
 ]


-- Data -------------------------------------------------------------------------------------------
-- | Parse a data type declaration.
pData  :: Parser (Top Source)
pData
 = do   sp      <- pTokSP K.KData
        b       <- pTyConBindName
        ps      <- liftM concat $ P.many pDataParam
             
        P.choice
         [ -- Data declaration with constructors that have explicit types.
           do   pTok KWhere
                pTok KBraceBra
                ctors   <- P.sepEndBy1 pDataCtor (pTok KSemiColon)
                pTok KBraceKet
                return  $ TopData sp (DataDef b ps ctors)
         
           -- Data declaration with no data constructors.
         , do   return  $ TopData sp (DataDef b ps [])
         ]


-- | Parse a type parameter to a data type.
pDataParam :: Parser [(Bind, Type)]
pDataParam 
 = do   pTok KRoundBra
        bs      <- fmap (fst . unzip) $ P.many1 pBindNameSP
        pTokSP (KOp ":")
        k       <- pType
        pTok KRoundKet
        return  [(b, k) | b <- bs]


-- | Parse a data constructor declaration.
pDataCtor :: Parser (DataCtor Source)
pDataCtor
 = do   n       <- pDaConBindName
        pTokSP (KOp ":")
        t       <- pType
        let (tsArg, tResult) = takeTFuns t

        return  $ DataCtor
                { dataCtorName          = n
                , dataCtorFieldTypes    = tsArg
                , dataCtorResultType    = tResult }

