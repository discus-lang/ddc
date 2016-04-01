
module DDC.Core.Parser.ExportSpec
        ( ExportSpec    (..)
        , pExportSpecs)
where
import DDC.Core.Module
import DDC.Core.Parser.Type
import DDC.Core.Parser.Context
import DDC.Core.Parser.Base
import DDC.Core.Lexer.Tokens
import DDC.Base.Pretty
import Control.Monad
import qualified DDC.Base.Parser        as P


-- An exported thing.
data ExportSpec n
        = ExportValue   n (ExportSource n)


-- | Parse some export specifications.
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


-- | Parse an export specification.
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

