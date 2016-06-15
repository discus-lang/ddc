
-- | Desugaring of function parameter syntax in Source Tetra.
module DDC.Source.Tetra.Parser.Param
        ( ParamSpec     (..)
        , expOfParams
        , funTypeOfParams
        , pBindParamSpec
        , pBindParamSpecAnnot)
where
import DDC.Source.Tetra.Parser.Base
import DDC.Source.Tetra.Parser.Type
import DDC.Source.Tetra.Exp.Source
import Data.Maybe
import qualified DDC.Core.Lexer.Tokens  as K
import qualified DDC.Base.Parser        as P


-- | Specification of a function parameter.
--   We can determine the contribution to the type of the function, 
--   as well as its expression based on the parameter.
data ParamSpec
        = ParamType    Bind (Maybe Type)
        | ParamWitness Bind (Maybe Type)
        | ParamValue   Bind (Maybe Type)


-- | Build the expression of a function from specifications of its parameters,
--   and the expression for the body.
expOfParams 
        :: [ParamSpec]  -- ^ Spec of parameters.
        -> Exp          -- ^ Body of function.
        -> Exp          -- ^ Expression of whole function.

expOfParams [] xBody            = xBody
expOfParams (p:ps) xBody
 = case p of
        ParamType    b mt
         -> XLAM (XBindVarMT b mt) $ expOfParams ps xBody
        
        ParamWitness b mt
         -> XLam (XBindVarMT b mt) $ expOfParams ps xBody

        ParamValue   b mt
         -> XLam (XBindVarMT b mt) $ expOfParams ps xBody


-- | Build the type of a function from specifications of its parameters,
--   and the type of the body.
funTypeOfParams 
        :: [ParamSpec]          -- ^ Spec of parameters.
        -> Type                 -- ^ Type of body.
        -> Type                 -- ^ Type of whole function.

funTypeOfParams [] tBody        
 = tBody

funTypeOfParams (p:ps) tBody
 = case p of
        ParamType     b mt
         -> TForall (fromMaybe (TBot KData) mt) b
          $ funTypeOfParams ps tBody

        ParamWitness  _ mt
         -> TImpl (fromMaybe (TBot KData) mt)
          $ funTypeOfParams ps tBody

        ParamValue    _ mt
         -> TFun  (fromMaybe (TBot KData) mt)
          $ funTypeOfParams ps tBody


-- | Parse a function parameter specification,
--   with an optional type (or kind) annotation.
pBindParamSpec :: Parser [ParamSpec]
pBindParamSpec
 = P.choice
 [      -- Value (or type) binder with a type (or kind) annotation.
        pBindParamSpecAnnot

        -- Value binder without type annotations.
  , do  b       <- pBind
        return  $  [ ParamValue b Nothing ]
 ]


-- | Parse a function parameter specification,
--   requiring a full type (or kind) annotation.
---
--       [BIND1 BIND2 .. BINDN : TYPE]
--   or  (BIND : TYPE)
--   or  (BIND : TYPE) { EFFECT | CLOSURE }
--
pBindParamSpecAnnot :: Parser [ParamSpec]
pBindParamSpecAnnot 
 = P.choice
        -- Type parameter
        -- [BIND1 BIND2 .. BINDN : TYPE]
 [ do   pTok K.KSquareBra
        bs      <- P.many1 pBind
        pTok (K.KOp ":")
        t       <- pType
        pTok K.KSquareKet
        return  [ ParamType b (Just t) | b <- bs]

        -- Witness parameter
        -- {BIND : TYPE}
 , do   pTok K.KBraceBra
        b       <- pBind
        pTok (K.KOp ":")
        t       <- pType
        pTok K.KBraceKet
        return  [ ParamWitness b (Just t) ]

        -- Value parameter with type annotations.
        -- (BIND1 BIND2 .. BINDN : TYPE) 
        -- (BIND1 BIND2 .. BINDN : TYPE) { TYPE | TYPE }
 , do   pTok K.KRoundBra
        bs      <- P.many1 pBind
        pTok (K.KOp ":")
        t       <- pType
        pTok K.KRoundKet

        return  $  [ ParamValue b (Just t) | b <- bs ]
 ]


