
module DDC.Core.Codec.Text.Parser.DataDef
        ( DataDef    (..)
        , pDataDef)
where
import DDC.Core.Codec.Text.Parser.Type
import DDC.Core.Codec.Text.Parser.Context
import DDC.Core.Codec.Text.Parser.Base
import DDC.Core.Codec.Text.Lexer.Tokens

import DDC.Core.Module.Name
import DDC.Core.Exp.Annot
import DDC.Type.DataDef
import DDC.Data.Pretty
import qualified DDC.Control.Parser     as P


pDataDef :: (Ord n, Pretty n)
         => Context n -> Maybe ModuleName -> Parser n (DataDef n)
pDataDef c mModName
 = do
        pTokSP (KKeyword EData)

        (modName, nData)
         <- case mModName of
                -- We're parsing a local data type declaration
                --   and have been given the name of the enclosing module.
                Just modName'
                 -> do  nData  <- pName
                        return (modName', nData)

                -- We're parsing an import data type declaration
                --  and take the name of the defining module from the name
                --  of the data type.
                Nothing
                 -> do  QualName modName nData <- pQualName
                        return  (modName, nData)


        bsParam <- fmap concat $ P.many (pDataParam c)

        P.choice
         [ -- Data declaration with constructors that have explicit types.
           do   pKey EWhere
                pSym SBraceBra
                ctors      <- P.sepEndBy1 (pDataCtor c modName nData bsParam)
                                          (pSym SSemiColon)
                let ctors' = [ ctor { dataCtorTag = tag }
                                | ctor <- ctors
                                | tag  <- [0..] ]
                pSym SBraceKet
                return  $ DataDef
                        { dataDefModuleName     = modName
                        , dataDefTypeName       = nData
                        , dataDefParams         = bsParam
                        , dataDefCtors          = Just ctors'
                        , dataDefIsAlgebraic    = True }

           -- Data declaration with no data constructors.
         , do   return  $ DataDef
                        { dataDefModuleName     = modName
                        , dataDefTypeName       = nData
                        , dataDefParams         = bsParam
                        , dataDefCtors          = Just []
                        , dataDefIsAlgebraic    = True }

         ]


-- | Parse a type parameter to a data type.
pDataParam :: (Ord n, Pretty n)
           => Context n -> Parser n [Bind n]
pDataParam c
 = do   pSym SRoundBra
        ns      <- P.many1 pName
        pTokSP (KOp ":")
        k       <- pKind c
        pSym SRoundKet
        return  [BName n k | n <- ns]


-- | Parse a data constructor declaration.
pDataCtor
        :: (Ord n, Pretty n)
        => Context n
        -> ModuleName   -- ^ Name of enclosing module.
        -> n            -- ^ Name of data type constructor.
        -> [Bind n]     -- ^ Type parameters of data type constructor.
        -> Parser n (DataCtor n)

pDataCtor c modName nData bsParam
 = do   n       <- pName
        pTokSP (KOp ":")
        t       <- pType c
        let (tsArg, tResult)
                = takeTFunArgResult t

        return  $ DataCtor
                { dataCtorModuleName    = modName
                , dataCtorName          = n

                -- Set tag to 0 for now. We fix this up in pDataDef above.
                , dataCtorTag           = 0

                , dataCtorFieldTypes    = tsArg
                , dataCtorResultType    = tResult
                , dataCtorTypeName      = nData
                , dataCtorTypeParams    = bsParam }

