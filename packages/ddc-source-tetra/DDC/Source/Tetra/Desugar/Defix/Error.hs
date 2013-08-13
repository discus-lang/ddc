
module DDC.Source.Tetra.Desugar.Defix.Error
        (Error (..))
where
import DDC.Source.Tetra.Exp
import DDC.Base.Pretty
import qualified DDC.Data.SourcePos     as BP


-- | Things that can go wrong when defixing code.
data Error a n
        -- | Infix operator symbol has no infix definition.
        = ErrorNoInfixDef
        { errorAnnot            :: a
        , errorSymbol           :: String }

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
        { errorAnnot            :: a
        , errorExp              :: Exp a n }
        deriving Show


-- Pretty ---------------------------------------------------------------------
instance (Pretty n) 
       => Pretty (Error BP.SourcePos n) where
 ppr err
  = case err of
        ErrorNoInfixDef{}
         -> vcat [ ppr $ errorAnnot err
                 , text "No infix definition for symbol: " <> ppr (errorSymbol err) ]

        ErrorDefixNonAssoc{}
         -> vcat [ ppr $ errorAnnot1 err
                 , text "Ambiguous infix expression."
                 , text " Operator  '"  <> text (errorOp1 err) 
                                        <> text "' at " <> ppr (errorAnnot1 err)
                                        <+> text "is non associative,"
                 , text " but the same precedence as"
                 , text "  operator '"  <> text (errorOp2 err)
                                        <> text "' at " <> ppr (errorAnnot2 err) 
                                        <> text "."]

        ErrorDefixMixedAssoc{}
         -> vcat [ ppr $ errorAnnot err
                 , text "Ambiguous infix expression."
                 , text " operators "   <> hsep (map ppr (errorOps err))
                        <> text "have different associativities but same precedence." ]

        ErrorMalformed{}
         -> vcat [ ppr $ errorAnnot err
                 , text "Malformed infix expression." ]
