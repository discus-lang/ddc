{-# OPTIONS_HADDOCK hide #-}
module DDC.Core.Check.Context.Mode
        (Mode (..))
where
import DDC.Core.Check.Context.Elem
import DDC.Type.Exp.Simple
import DDC.Data.Pretty


-- | What mode we're performing type checking/inference in.
data Mode n
        -- | Reconstruct the type of the expression, requiring type annotations
        --   on parameters  as well as type applications to already be present.
        = Recon

        -- | The ascending smoke of incense.
        --
        --   Synthesise the type of the expression, producing unification
        --   variables for bidirectional type inference.
        --
        --   Any new unification variables introduced may be used to define the
        --   given existentials, so the need to be declared outside their scopes.
        --   If the list is empty we can add new variables to the inner most scope.
        --
        | Synth [Exists n]

        -- | The descending tongue of flame.
        --   Check the type of an expression against this expected type, and
        --   unify expected types into unification variables for bidirecional
        --   type inference.
        | Check (Type n)
        deriving (Show, Eq)


instance (Eq n, Pretty n) => Pretty (Mode n) where
 ppr mode
  = case mode of
        Recon    -> text "RECON"
        Synth is -> text "SYNTH" <+> ppr is
        Check t  -> text "CHECK" <+> parens (ppr t)


