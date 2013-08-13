
module DDC.Source.Tetra.Desugar.Defix.Error
        (Error (..))
where
import DDC.Source.Tetra.Exp


-- | Things that can go wrong when defixing code.
data Error a n
        -- | Infix operator symbol has no infix definition.
        = ErrorNoInfixDef
        { errorSymbol           :: String }

        -- | Two non-associative operators with the same precedence.
        | ErrorDefixNonAssoc
        { errorOp1              :: String
        , errorAnnot1           :: a
        , errorOp2              :: String
        , errorAnnot2           :: a }

        -- | Two operators of different associativies with same precedence.
        | ErrorDefixMixedAssoc 
        { errorAnnot            :: a
        , errorOps              :: [String] }

        -- | Infix expression is malformed.
        --   Eg "+ 3" or "2 + + 2"
        | ErrorMalformed
        { errorExp              :: Exp a n }
        deriving Show

