
-- | Desugaring of function parameter syntax in Source Tetra.
module DDC.Source.Tetra.Parser.Param
        ( ParamSpec     (..)
        , funTypeOfParams
        , pBindParamSpec
        , expOfParams)
where
import DDC.Source.Tetra.Exp.Annot
import DDC.Core.Parser
        ( ParamSpec(..)
        , funTypeOfParams
        , pBindParamSpec)


-- | Build the expression of a function from specifications of its parameters,
--   and the expression for the body.
expOfParams 
        :: [ParamSpec Name]     -- ^ Spec of parameters.
        -> Exp a                -- ^ Body of function.
        -> Exp a                -- ^ Expression of whole function.

expOfParams [] xBody            = xBody
expOfParams (p:ps) xBody
 = case p of
        ParamType b     
         -> XLAM b $ expOfParams ps xBody
        
        ParamWitness b
         -> XLam b $ expOfParams ps xBody

        ParamValue b _ _
         -> XLam b $ expOfParams ps xBody
