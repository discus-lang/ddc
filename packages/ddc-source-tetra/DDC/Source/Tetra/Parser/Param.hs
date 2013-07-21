
module DDC.Source.Tetra.Parser.Param
        ( ParamSpec     (..)
        , funTypeOfParams
        , pBindParamSpec
        , expOfParams)
where
import DDC.Source.Tetra.Exp
import DDC.Core.Parser.Param    
        ( ParamSpec(..)
        , funTypeOfParams
        , pBindParamSpec)


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
