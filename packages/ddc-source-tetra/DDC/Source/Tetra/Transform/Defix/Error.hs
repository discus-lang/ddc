{-# LANGUAGE TypeFamilies, UndecidableInstances #-}

-- | Things that can go wrong when resolving infix expressions.
module DDC.Source.Tetra.Transform.Defix.Error
        (Error (..))
where
import DDC.Source.Tetra.Exp.Generic
import DDC.Source.Tetra.Pretty


-- | Things that can go wrong when defixing code.
data Error l
        -- | Infix operator symbol has no infix definition.
        = ErrorNoInfixDef
        { errorAnnot            :: GAnnot l
        , errorSymbol           :: String }

        -- | Two non-associative operators with the same precedence.
        | ErrorDefixNonAssoc
        { errorOp1              :: String
        , errorAnnot1           :: GAnnot l
        , errorOp2              :: String
        , errorAnnot2           :: GAnnot l }

        -- | Two operators of different associativies with same precedence.
        | ErrorDefixMixedAssoc 
        { errorAnnot            :: GAnnot l
        , errorOps              :: [String] }

        -- | Infix expression is malformed.
        --   Eg "+ 3" or "2 + + 2"
        | ErrorMalformed
        { errorAnnot            :: GAnnot l
        , errorExp              :: GExp l }

deriving instance ShowLanguage l => Show (Error l)


instance PrettyLanguage l => Pretty (Error l) where
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
                        <> text (errorOp1 err) 
                        <> text "' at " <> ppr (errorAnnot1 err)
                        <+> text "is non associative,"
                 , text " but the same precedence as"
                 , text "  operator '"  
                        <> text (errorOp2 err)
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
