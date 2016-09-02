
module DDC.Core.Check.Context.Mode
        (Mode (..))
where
import DDC.Type.Exp.Simple
import DDC.Base.Pretty


-- | What mode we're performing type checking/inference in.
data Mode n
        -- | Reconstruct the type of the expression, requiring type annotations
        --   on parameters  as well as type applications to already be present.
        = Recon
        
        -- | The ascending smoke of incense.
        --   Synthesise the type of the expression, producing unification
        --   variables for bidirectional type inference.
        --   
        | Synth

        -- | The descending tongue of flame.
        --   Check the type of an expression against this expected type, and
        --   unify expected types into unification variables for bidirecional
        --   type inference.
        | Check (Type n)
        deriving (Show, Eq)


instance (Eq n, Pretty n) => Pretty (Mode n) where
 ppr mode
  = case mode of
        Recon   -> text "RECON"
        Synth   -> text "SYNTH"
        Check t -> text "CHECK" <+> parens (ppr t)


