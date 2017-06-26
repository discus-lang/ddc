
module DDC.Source.Tetra.Parser.Param
        ( -- * Parameters
          pTypeParams
        , pTermParams
        , pDeclTermParamsSP
        , funTypeOfParams

          -- * Patterns
        , pPat
        , pPatSimple)
where
import DDC.Source.Tetra.Parser.Type
import DDC.Source.Tetra.Parser.Base
import DDC.Source.Tetra.Exp
import DDC.Source.Tetra.Prim            as S
import DDC.Core.Lexer.Tokens
import qualified DDC.Control.Parser     as P
import qualified Data.Text              as Text
import Data.Maybe

-- Params ---------------------------------------------------------------------
-- | Parse some type parameters.
pTypeParams :: Parser [(Bind, Maybe Type)]
pTypeParams
 = P.choice
 [ fmap concat
    $ P.many $ do
        pSym SRoundBra
        bs'     <- P.many1 pBind
        pTok (KOp ":")
        t       <- pType
        pSym SRoundKet
        return  [(b, Just t)  | b <- bs']

 , do   bs'     <- P.many1 pBind
        return  [(b, Nothing) | b <- bs']
 ]


-- | Parse some term parameters.
pTermParams :: Parser [(ParamSort, Pat, Maybe Type)]
pTermParams
 = P.choice
 [ P.try $ do
         pSym SRoundBra
         ps   <- P.many1 pPatSimple
         pTok (KOp ":")
         t    <- pType
         pSym SRoundKet
         return  [(MSTerm, p, Just t) | p <- ps]

 , P.try $ do
         pSym SBraceBra
         ps   <- P.many1 pPatSimple
         pTok (KOp ":")
         t    <- pType
         pSym SBraceKet
         return  [(MSImplicit, p, Just t) | p <- ps]

 , do    ps      <- P.many1 pPatSimple
         return  [(MSTerm, p, Nothing) | p <- ps]
 ]


-- Function parameters.
pDeclTermParamsSP :: Parser [Param]
pDeclTermParamsSP
 = P.choice
        -- Type parameter
        --   [BIND1 BIND2 .. BINDN : TYPE]
 [ do   pSym SSquareBra
        bs      <- P.many1 pBind
        pTok (KOp ":")
        t       <- pType
        pSym SSquareKet
        return  [ MType b (Just t) | b <- bs]


        -- Implicit term parameter
        --   {BIND : TYPE}
 , do   pSym  SBraceBra
        ps      <- P.choice
                [  P.try $ do
                        ps      <- P.many1 pPatSimple
                        pTok (KOp ":")
                        t       <- pType
                        return  [ MImplicit p (Just t) | p <- ps ]

                , do    t       <- pType
                        return  [ MImplicit PDefault (Just t) ]
                ]
        pSym  SBraceKet
        return  ps


        -- Value parameter without a type annotation.
        --   This needs to come before the case for value patterns with type
        --   annotations because both records and the unit data constructor
        --   pattern start with a '('.
 , do   p       <- pPatSimple
        return  [MTerm p Nothing]


        -- Term pattern with type annotations.
        -- (BIND1 BIND2 .. BINDN : TYPE)
 , do   pSym    SRoundBra
        ps      <- P.choice
                [  P.try $ do
                        ps      <- P.many1 pPatSimple
                        pTok (KOp ":")
                        t       <- pType
                        return  [ MTerm p (Just t) | p <- ps ]

                , do    p       <- pPat
                        return  [ MTerm p Nothing ]
                ]

        pSym    SRoundKet
        return ps
 ]
 <?> "a function parameter"


--   and the type of the body.
funTypeOfParams
        :: [Param]      -- ^ Spec of parameters.
        -> Type         -- ^ Type of body.
        -> Type         -- ^ Type of whole function.

funTypeOfParams [] tBody
 = tBody

funTypeOfParams (p:ps) tBody
 = case p of
        MType     b mt
         -> let k       = fromMaybe (TBot S.KData) mt
            in  TApp (TCon (TyConForall k)) (TAbs b k $ funTypeOfParams ps tBody)

        MTerm     _ mt
         -> let k       = fromMaybe (TBot S.KData) mt
            in  TFunExplicit k  $ funTypeOfParams ps tBody

        MImplicit  _ mt
         -> let k       = fromMaybe (TBot S.KData) mt
            in  TFunImplicit k  $ funTypeOfParams ps tBody


-- Patterns -------------------------------------------------------------------
-- | Pattern.
pPat :: Parser Pat
pPat
 = P.choice
 [  -- Con Bind Bind ...
    do  nCon    <- pDaConBoundName
        ps      <- P.many pPatSimple
        return  $ PData (DaConBound nCon) ps

    -- Base pattern.
 ,  do  p       <- pPatSimple
        return  p
 ]
 <?> "a pattern"


-- | Simple pattern.
pPatSimple :: Parser Pat
pPatSimple
 = P.choice
 [
        -- Tuple pattern.
   P.try $ do
        _sp       <- pSym SRoundBra
        pField1   <- pPat
        _         <- pSym SComma
        psField'  <- P.sepBy1 pPat (pSym SComma)
        _         <- pSym SRoundKet
        let ps    =  pField1 : psField'
        let arity =  length ps
        let nCtor =  Text.pack ("T" ++ show arity)
        return    $  PData (DaConBound (DaConBoundName nCtor)) ps

        -- Sugared record pattern.
        -- like (x = a, y = b, z = c)
 , P.try $ do
        _       <- pSym SRoundBra

        (nsField, psField)
         <- fmap unzip
          $ P.sepBy
                (do (n, _)  <- pVarNameSP
                    _       <- pSym SEquals
                    p       <- pPatSimple
                    return (n, p))
                (pSym SComma)

        pSym SRoundKet
        return  $ PData (DaConRecord nsField) psField

        -- Primitive record pattern.
        -- like (x,y,z)# a b c
  , P.try $ do
        _       <- pSym SRoundBra
        nsField <- fmap (map fst) $ P.sepBy pVarNameSP (pSym SComma)
        pSym SRoundKet
        pSym SHash
        psField <- P.many pPatSimple
        return  $ PData (DaConRecord nsField) psField

        -- ( PAT )
 , P.try $ do
        pSym SRoundBra
        p       <- pPat
        pSym SRoundKet
        return  $ p

        -- Wildcard
        --   Try this case before the following one for binders
        --   so that '_' is parsed as the default pattern,
        --   rather than a wildcard binder.
 , do   pSym SUnderscore
        return  $ PDefault

        -- Var
 , do   b       <- pBind
        P.choice
         [ do   _       <- pSym SAt
                p       <- pPatSimple
                return  $  PAt b p

         , do   return  $  PVar b
         ]

        -- Lit
 , do   nLit    <- pDaConBoundLit
        return  $ PData (DaConPrim nLit (TBot S.KData)) []

        -- Named algebraic constructors.
 , do   nCon    <- pDaConBoundName
        return  $ PData (DaConBound nCon) []

        -- 'Unit'
 , do   pTok    (KBuiltin BDaConUnit)
        return  $ PData  dcUnit []
 ]
 <?> "a pattern"


