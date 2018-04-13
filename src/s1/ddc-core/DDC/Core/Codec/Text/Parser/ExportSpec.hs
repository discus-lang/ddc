
module DDC.Core.Codec.Text.Parser.ExportSpec
        ( ExportSpec    (..)
        , pExportSpecs)
where
import DDC.Core.Codec.Text.Parser.Type
import DDC.Core.Codec.Text.Parser.Context
import DDC.Core.Codec.Text.Parser.Base
import DDC.Core.Codec.Text.Lexer.Tokens

import DDC.Core.Module
import DDC.Type.Exp.Simple
import DDC.Data.Pretty
import qualified DDC.Control.Parser     as P
import qualified Data.Text              as T


-- An exported thing.
data ExportSpec n
        = ExportValue   n (ExportValue n (Type n))


-- | Parse some export specifications.
pExportSpecs
        :: (Ord n, Pretty n)
        => Context n -> Parser n [ExportSpec n]

pExportSpecs c
 = do   pTok (KKeyword EExport)

        P.choice
         [      -- export value { (NAME :: TYPE)+ }
           do   P.choice [ do   pKey EValue
                                return ()
                         ,      return () ]

                pSym    SBraceBra
                specs   <- P.sepEndBy1 (pExportValue c)
                                       (pSym SSemiColon)
                pSym    SBraceKet
                return specs

                -- export foreign X value { (NAME :: TYPE)+ }
         , do   pKey    EForeign
                dst     <- fmap (renderIndent . ppr) pName
                pKey    EValue
                pSym    SBraceBra
                specs   <- P.sepEndBy1 (pExportForeignValue c dst)
                                       (pSym SSemiColon)
                pSym    SBraceKet
                return specs
         ]


-- | Parse an export specification.
pExportValue
        :: (Ord n, Pretty n)
        => Context n -> Parser n (ExportSpec n)

pExportValue c
 = do
        QualName mn n <- pQualName
        pTokSP (KOp ":")
        t       <- pType c
        return  (ExportValue n (ExportValueLocal mn n t Nothing))


-- | Parse a foreign value export spec.
pExportForeignValue
        :: (Ord n, Pretty n)
        => Context n -> String -> Parser n (ExportSpec n)

pExportForeignValue c dst
        | "c"           <- dst
        = do    n       <- pName
                pTokSP (KOp ":")
                t       <- pType c

                let nTxt = renderIndent $ ppr n

                -- ISSUE #327: Allow external symbol to be specified
                --             with foreign C imports and exports.
                return  (ExportValue n (ExportValueSea n (T.pack nTxt) t))

        | otherwise
        = P.unexpected "export mode for foreign value."

