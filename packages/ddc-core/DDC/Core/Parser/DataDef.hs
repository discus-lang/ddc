
module DDC.Core.Parser.DataDef
        ( DataDef    (..)
        , pDataDef)
where
import DDC.Core.Exp.Annot
import DDC.Core.Parser.Type
import DDC.Core.Parser.Context
import DDC.Core.Parser.Base
import DDC.Core.Lexer.Tokens
import DDC.Type.DataDef
import Control.Monad
import qualified DDC.Control.Parser     as P


pDataDef :: Ord n => Context n -> Parser n (DataDef n)
pDataDef c
 = do   pTokSP (KKeyword EData)
        nData   <- pName 
        bsParam <- liftM concat $ P.many (pDataParam c)

        P.choice
         [ -- Data declaration with constructors that have explicit types.
           do   pKey EWhere
                pSym SBraceBra
                ctors      <- P.sepEndBy1 (pDataCtor c nData bsParam) 
                                          (pSym SSemiColon)
                let ctors' = [ ctor { dataCtorTag = tag }
                                | ctor <- ctors
                                | tag  <- [0..] ]
                pSym SBraceKet
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
 = do   pSym SRoundBra
        ns      <- P.many1 pName
        pTokSP (KOp ":")
        k       <- pType c
        pSym SRoundKet
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

