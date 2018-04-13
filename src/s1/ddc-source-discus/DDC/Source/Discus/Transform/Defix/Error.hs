{-# OPTIONS_HADDOCK hide #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Things that can go wrong when resolving infix expressions.
module DDC.Source.Discus.Transform.Defix.Error
        (Error (..))
where
import DDC.Source.Discus.Exp.Term.Base
import DDC.Source.Discus.Pretty


-- | Things that can go wrong when defixing code.
data Error a
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
        , errorExp              :: GExp a }

deriving instance Show a => Show (Error a)


instance PrettyLanguage a => Pretty (Error a) where
 ppr err
  = case err of
        ErrorNoInfixDef{}
         -> vcat [ ppr $ errorAnnot err
                 , text "No infix definition for symbol: "
                        <> ppr (errorSymbol err) ]

        ErrorDefixNonAssoc{}
         -> vcat [ ppr $ errorAnnot1 err
                 , text "Ambiguous infix expression."
                 , text " Operator  '"
                        <> string (errorOp1 err)
                        <> text "' at " <> ppr (errorAnnot1 err)
                        %% text "is non associative,"
                 , text " but the same precedence as"
                 , text "  operator '"
                        <> string (errorOp2 err)
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
