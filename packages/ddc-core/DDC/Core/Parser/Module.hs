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
import DDC.Core.Compounds
import DDC.Base.Pretty
import Data.Char
import qualified DDC.Base.Parser        as P
import qualified Data.Text              as T


-- | Parse a core module.
pModule :: (Ord n, Pretty n) 
        => Context n
        -> Parser n (Module P.SourcePos n)
pModule c
 = do   sp      <- pTokSP KModule
        name    <- pModuleName

        -- Header declarations
        heads           <- P.many (pHeadDecl c)
        let importSpecs = concat $ [specs | HeadImportSpecs specs <- heads ]
        let exportSpecs = concat $ [specs | HeadExportSpecs specs <- heads ]
        let defsLocal   =          [def   | HeadDataDef     def   <- heads ]

        -- TODO: attach arities to import specs.

        -- Function definitions.
        --  If there is a 'with' keyword then this is a standard module with bindings.
        --  If not, then it is a module header, which doesn't need bindings.
        (lts, isHeader) 
         <- P.choice
                [ do    pTok KWith

                        -- LET;+
                        lts  <- P.sepBy1 (pLetsSP c) (pTok KIn)
                        return (lts, False)

                , do    return ([],  True) ]

        -- The body of the module consists of the top-level bindings wrapped
        -- around a unit constructor place-holder.
        let body = xLetsAnnot lts (xUnit sp)

        return  $ ModuleCore
                { moduleName            = name
                , moduleIsHeader        = isHeader
                , moduleExportTypes     = []
                , moduleExportValues    = [(n, s) | ExportValue n s <- exportSpecs]
                , moduleImportTypes     = [(n, s) | ImportType  n s <- importSpecs]
                , moduleImportValues    = [(n, s) | ImportValue n s <- importSpecs]
                , moduleImportDataDefs  = [def    | ImportData  def <- importSpecs]
                , moduleDataDefsLocal   = defsLocal
                , moduleBody            = body }


-- | Wrapper for a declaration that can appear in the module header.
data HeadDecl n
        = HeadImportSpecs  [ImportSpec  n]
        | HeadExportSpecs  [ExportSpec  n]
        | HeadDataDef      (DataDef     n)

        -- | Number of type and value parameters for some super.
        | HeadPragmaArity  n Int Int


-- | Parse one of the declarations that can appear in a module header.
pHeadDecl :: (Ord n, Pretty n)
          => Context n -> Parser n (HeadDecl n)
pHeadDecl ctx
 = P.choice 
        [ do    def     <- pDataDef ctx
                return  $ HeadDataDef def

        , do    imports <- pImportSpecs ctx
                return  $ HeadImportSpecs imports

        , do    exports <- pExportSpecs ctx
                return  $ HeadExportSpecs exports 

        , do    pHeadPragma ctx ]


-- | Parse one of the pragmas that can appear in the module header.
pHeadPragma :: Context n -> Parser n (HeadDecl n)
pHeadPragma ctx
 = do   (txt, sp)      <- pPragmaSP
        case words $ T.unpack txt of

         -- The type and value arity of a super.
         ["ARITY", name, strTypes, strValues]
          |  all isDigit strTypes
          ,  all isDigit strValues
          , Just makeStringName <- contextMakeStringName ctx
          -> return $ HeadPragmaArity
                (makeStringName sp (T.pack name))
                (read strTypes) (read strValues)

         _ -> P.unexpected $ "pragma " ++ "{-# " ++ T.unpack txt ++ "#-}"

