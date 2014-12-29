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
import Control.Monad
import qualified DDC.Base.Parser        as P


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

