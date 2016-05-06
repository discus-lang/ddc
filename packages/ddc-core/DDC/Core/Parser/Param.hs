
module DDC.Core.Parser.Param
        ( ParamSpec     (..)
        , funTypeOfParams
        , expOfParams
        , pBindParamSpecAnnot
        , pBindParamSpec )
where
import DDC.Core.Exp
import DDC.Core.Parser.Type
import DDC.Core.Parser.Context
import DDC.Core.Parser.Base             (Parser)
import DDC.Core.Lexer.Tokens
import qualified DDC.Base.Parser        as P
import qualified DDC.Type.Exp.Simple    as T


-- | Specification of a function parameter.
--   We can determine the contribution to the type of the function, 
--   as well as its expression based on the parameter.
data ParamSpec n
        = ParamType    (Bind n)
        | ParamWitness (Bind n)
        | ParamValue   (Bind n) (Type n) (Type n)


-- | Build the expression of a function from specifications of its parameters,
--   and the expression for the body.
expOfParams 
        :: a
        -> [ParamSpec n]        -- ^ Spec of parameters.
        -> Exp a n              -- ^ Body of function.
        -> Exp a n              -- ^ Expression of whole function.

expOfParams _ [] xBody            = xBody
expOfParams a (p:ps) xBody
 = case p of
        ParamType b     
         -> XLAM a b $ expOfParams a ps xBody
        
        ParamWitness b
         -> XLam a b $ expOfParams a ps xBody

        ParamValue b _ _
         -> XLam a b $ expOfParams a ps xBody


-- | Build the type of a function from specifications of its parameters,
--   and the type of the body.
funTypeOfParams 
        :: Context n
        -> [ParamSpec n]        -- ^ Spec of parameters.
        -> Type n               -- ^ Type of body.
        -> Type n               -- ^ Type of whole function.

funTypeOfParams _ [] tBody        
 = tBody

funTypeOfParams c (p:ps) tBody
 = case p of
        ParamType  b    
         -> TForall b 
                $ funTypeOfParams c ps tBody

        ParamWitness b
         -> T.tImpl (T.typeOfBind b)
                $ funTypeOfParams c ps tBody

        ParamValue b _eff _clo
         -> T.tFun (T.typeOfBind b)
                $ funTypeOfParams c ps tBody


-- | Parse a function parameter specification,
--   with an optional type (or kind) annotation.
pBindParamSpec
        :: Ord n
        => Context n -> Parser n [ParamSpec n]

pBindParamSpec c
 = P.choice
 [      -- Value (or type) binder with a type (or kind) annotation.
        pBindParamSpecAnnot c

        -- Value binder without type annotations.
  , do  b       <- pBinder
        return  $  [ ParamValue (T.makeBindFromBinder b (T.tBot T.kData))
                                (T.tBot T.kEffect) (T.tBot T.kClosure) ]
 ]


-- | Parse a function parameter specification,
--   requiring a full type (or kind) annotation.
---
--       [BIND1 BIND2 .. BINDN : TYPE]
--   or  (BIND : TYPE)
--   or  (BIND : TYPE) { EFFECT | CLOSURE }
--
pBindParamSpecAnnot 
        :: Ord n 
        => Context n -> Parser n [ParamSpec n]

pBindParamSpecAnnot c
 = P.choice
        -- Type parameter
        -- [BIND1 BIND2 .. BINDN : TYPE]
 [ do   pTok KSquareBra
        bs      <- P.many1 pBinder
        pTok (KOp ":")
        t       <- pType c
        pTok KSquareKet
        return  [ ParamType b 
                | b <- zipWith T.makeBindFromBinder bs (repeat t)]

        -- Witness parameter
        -- {BIND : TYPE}
 , do   pTok KBraceBra
        b       <- pBinder
        pTok (KOp ":")
        t       <- pType c
        pTok KBraceKet
        return  [ ParamWitness $ T.makeBindFromBinder b t]

        -- Value parameter with type annotations.
        -- (BIND1 BIND2 .. BINDN : TYPE) 
        -- (BIND1 BIND2 .. BINDN : TYPE) { TYPE | TYPE }
 , do   pTok KRoundBra
        bs      <- P.many1 pBinder
        pTok (KOp ":")
        t       <- pType c
        pTok KRoundKet

        (eff, clo) 
         <- P.choice
                [ do    pTok KBraceBra
                        eff'    <- pType c
                        pTok KBar
                        clo'    <- pType c
                        pTok KBraceKet
                        return  (eff', clo')
                
                , do    return  (T.tBot T.kEffect, T.tBot T.kClosure) ]
        
        let bLast : bsInit 
                = reverse bs

        return  $  [ ParamValue (T.makeBindFromBinder b     t) 
                                (T.tBot T.kEffect) (T.tBot T.kClosure)
                        | b <- reverse bsInit]
                ++ [ ParamValue (T.makeBindFromBinder bLast t) eff clo]
 ]

