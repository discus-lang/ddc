{-# OPTIONS -fno-warn-unused-binds #-}
module DDC.Core.Parser.Module
        (pModule)
where
import DDC.Core.Parser.Type
import DDC.Core.Parser.Exp
import DDC.Core.Parser.Context
import DDC.Core.Parser.Base
import DDC.Core.Parser.ExportSpec
import DDC.Core.Parser.ImportSpec
import DDC.Core.Parser.DataDef
import DDC.Core.Module
import DDC.Core.Lexer.Tokens
import DDC.Core.Exp.Annot
import DDC.Data.Pretty
import Data.Char
import qualified Data.Map               as Map
import qualified Data.Text              as T
import qualified DDC.Control.Parser     as P


-- | Parse a core module.
pModule :: (Ord n, Pretty n)
        => Context n
        -> Parser n (Module P.SourcePos n)
pModule c
 = do   sp      <- pTokSP (KKeyword EModule)
        name    <- pModuleName

        -- Parse header declarations
        heads                   <- P.many (pHeadDecl c)
        let importSpecs_noArity = concat $ [specs  | HeadImportSpecs   specs <- heads ]
        let exportSpecs         = concat $ [specs  | HeadExportSpecs   specs <- heads ]

        let dataDefsLocal       = [def         | HeadDataDef     def       <- heads ]
        let typeDefsLocal       = [(n, (k, t)) | HeadTypeDef     n k t     <- heads ]

        -- Attach arity information to import specs.
        --   The aritity information itself comes in the ARITY pragmas,
        --   which are parsed as separate top level things.
        let importArities
                = Map.fromList  [ (n, (iTypes, iValues, iBoxes ))
                                | HeadPragmaArity n iTypes iValues iBoxes <- heads ]

        let attachAritySpec (ImportForeignValue n (ImportValueModule mn v t _))
                = ImportForeignValue n (ImportValueModule mn v t (Map.lookup n importArities))

            attachAritySpec spec = spec

        let importSpecs
                = map attachAritySpec importSpecs_noArity


        -- Parse function definitions.
        --  If there is a 'with' keyword then this is a standard module with bindings.
        --  If not, then it is a module header, which doesn't need bindings.
        (lts, isHeader)
         <- P.choice
                [ do    pTok (KKeyword EWith)

                        -- LET;+
                        lts  <- P.sepBy1 (pLetsSP c) (pTok (KKeyword EIn))
                        let lts' = concat [map (\l -> (l, sp')) ls  | (ls, sp') <- lts]

                        return (lts', False)

                , do    return ([],  True) ]

        -- The body of the module consists of the top-level bindings wrapped
        -- around a unit constructor place-holder.
        let body = xLetsAnnot lts (xUnit sp)

        return  $ ModuleCore
                { moduleName            = name
                , moduleIsHeader        = isHeader
                , moduleExportTypes     = []
                , moduleExportValues    = [(n, s)      | ExportValue n s        <- exportSpecs]
                , moduleImportTypes     = [(n, s)      | ImportForeignType  n s <- importSpecs]
                , moduleImportCaps      = [(n, s)      | ImportForeignCap   n s <- importSpecs]
                , moduleImportValues    = [(n, s)      | ImportForeignValue n s <- importSpecs]
                , moduleImportTypeDefs  = [(n, (k, t)) | ImportType  n k t      <- importSpecs]
                , moduleImportDataDefs  = [def         | ImportData  def        <- importSpecs]
                , moduleDataDefsLocal   = dataDefsLocal
                , moduleTypeDefsLocal   = typeDefsLocal
                , moduleBody            = body }


---------------------------------------------------------------------------------------------------
-- | Wrapper for a declaration that can appear in the module header.
data HeadDecl n
        -- | Import specifications.
        = HeadImportSpecs  [ImportSpec  n]

        -- | Export specifications.
        | HeadExportSpecs  [ExportSpec  n]

        -- | Data type definitions.
        | HeadDataDef      (DataDef     n)

        -- | Type equations.
        | HeadTypeDef       n (Kind n) (Type n)

        -- | Arity pragmas.
        --   Number of type parameters, value parameters, and boxes for some super.
        | HeadPragmaArity  n Int Int Int


-- | Parse one of the declarations that can appear in a module header.
pHeadDecl :: (Ord n, Pretty n)
          => Context n -> Parser n (HeadDecl n)

pHeadDecl ctx
 = P.choice
        [ do    imports <- pImportSpecs ctx
                return  $ HeadImportSpecs imports

        , do    exports <- pExportSpecs ctx
                return  $ HeadExportSpecs exports

        , do    def     <- pDataDef ctx
                return  $ HeadDataDef def

        , do    (n, k, t) <- pTypeDef ctx
                return  $ HeadTypeDef n k t

        , do    pHeadPragma ctx
        ]


-- | Parse a type equation.
pTypeDef :: (Ord n, Pretty n)
         => Context n -> Parser n (n, Kind n, Type n)
pTypeDef c
 = do   pKey    EType
        n       <- pName
        pTokSP  (KOp ":")
        k       <- pType c
        pSym SEquals
        t       <- pType c
        pSym SSemiColon
        return  (n, k, t)


-- | Parse one of the pragmas that can appear in the module header.
pHeadPragma :: Context n -> Parser n (HeadDecl n)
pHeadPragma ctx
 = do   (txt, sp)      <- pPragmaSP
        case words $ T.unpack txt of

         -- The type and value arity of a super.
         ["ARITY", name, strTypes, strValues, strBoxes]
          |  all isDigit strTypes
          ,  all isDigit strValues
          ,  all isDigit strBoxes
          ,  Just makeLitName <- contextMakeLiteralName ctx
          ,  Just n           <- makeLitName sp (LString (T.pack name)) True
          -> return $ HeadPragmaArity n
                (read strTypes) (read strValues) (read strBoxes)

         _ -> P.unexpected $ "pragma " ++ "{-# " ++ T.unpack txt ++ "#-}"
