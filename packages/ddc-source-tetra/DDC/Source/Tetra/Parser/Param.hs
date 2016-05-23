
-- | Desugaring of function parameter syntax in Source Tetra.
module DDC.Source.Tetra.Parser.Param
        ( ParamSpec     (..)
        , expOfParams
        , funTypeOfParams
        , pBindParamSpec
        , pBindParamSpecAnnot)
where
import DDC.Source.Tetra.Exp.Annot
import DDC.Core.Lexer.Tokens
import qualified DDC.Base.Parser        as P
import qualified DDC.Type.Exp.Simple    as T

import DDC.Core.Parser
        ( Parser
        , Context (..)
        , pTok
        , pType
        , pBinder)

-- | Specification of a function parameter.
--   We can determine the contribution to the type of the function, 
--   as well as its expression based on the parameter.
data ParamSpec
        = ParamType    Bind (Maybe (T.Type Name))
        | ParamWitness Bind (Maybe (T.Type Name))
        | ParamValue   Bind (Maybe (T.Type Name))


-- | Build the expression of a function from specifications of its parameters,
--   and the expression for the body.
expOfParams 
        :: [ParamSpec]          -- ^ Spec of parameters.
        -> Exp a                -- ^ Body of function.
        -> Exp a                -- ^ Expression of whole function.

expOfParams [] xBody            = xBody
expOfParams (p:ps) xBody
 = case p of
        ParamType    b mt
         -> XLAM (BindMT b mt) $ expOfParams ps xBody
        
        ParamWitness b mt
         -> XLam (BindMT b mt) $ expOfParams ps xBody

        ParamValue   b mt
         -> XLam (BindMT b mt) $ expOfParams ps xBody


-- | Build the type of a function from specifications of its parameters,
--   and the type of the body.
funTypeOfParams 
        :: Context Name
        -> [ParamSpec]          -- ^ Spec of parameters.
        -> T.Type Name          -- ^ Type of body.
        -> T.Type Name          -- ^ Type of whole function.

funTypeOfParams _ [] tBody        
 = tBody

funTypeOfParams c (p:ps) tBody
 = case p of
        ParamType     b _  
         -> T.TForall b
          $ funTypeOfParams c ps tBody

        ParamWitness  b _
         -> T.tImpl (T.typeOfBind b)
          $ funTypeOfParams c ps tBody

        ParamValue    b _
         -> T.tFun (T.typeOfBind b)
          $ funTypeOfParams c ps tBody

-- | Parse a function parameter specification,
--   with an optional type (or kind) annotation.
pBindParamSpec
        :: Context Name -> Parser Name [ParamSpec]

pBindParamSpec c
 = P.choice
 [      -- Value (or type) binder with a type (or kind) annotation.
        pBindParamSpecAnnot c

        -- Value binder without type annotations.
  , do  b       <- pBinder
        return  $  [ ParamValue (T.makeBindFromBinder b (T.tBot T.kData)) Nothing ]
 ]


-- | Parse a function parameter specification,
--   requiring a full type (or kind) annotation.
---
--       [BIND1 BIND2 .. BINDN : TYPE]
--   or  (BIND : TYPE)
--   or  (BIND : TYPE) { EFFECT | CLOSURE }
--
pBindParamSpecAnnot 
        :: Context Name -> Parser Name [ParamSpec]

pBindParamSpecAnnot c
 = P.choice
        -- Type parameter
        -- [BIND1 BIND2 .. BINDN : TYPE]
 [ do   pTok KSquareBra
        bs      <- P.many1 pBinder
        pTok (KOp ":")
        t       <- pType c
        pTok KSquareKet
        return  [ ParamType   (T.makeBindFromBinder b t) (Just t)
                | b <- bs]

        -- Witness parameter
        -- {BIND : TYPE}
 , do   pTok KBraceBra
        b       <- pBinder
        pTok (KOp ":")
        t       <- pType c
        pTok KBraceKet
        return  [ ParamWitness (T.makeBindFromBinder b t) (Just t)]

        -- Value parameter with type annotations.
        -- (BIND1 BIND2 .. BINDN : TYPE) 
        -- (BIND1 BIND2 .. BINDN : TYPE) { TYPE | TYPE }
 , do   pTok KRoundBra
        bs      <- P.many1 pBinder
        pTok (KOp ":")
        t       <- pType c
        pTok KRoundKet

        return  $  [ ParamValue (T.makeBindFromBinder b t) (Just t)
                   | b <- bs ]
 ]
