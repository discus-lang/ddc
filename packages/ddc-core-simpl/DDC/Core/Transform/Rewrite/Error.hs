module DDC.Core.Transform.Rewrite.Error
        ( Error (..)
        , Side  (..))
where
import DDC.Core.Exp
import DDC.Core.Check                   ()
import DDC.Type.Pretty
import qualified DDC.Core.Check         as C


-- | What can go wrong when checking a rewrite rule.
data Error a n
        -- | Error typechecking one of the expressions
        = ErrorTypeCheck
        { errorSide         :: Side     -- ^ left-hand side or right
        , errorExp          :: Exp a n
        , errorCheckError   :: C.Error a n }

        -- | Error typechecking one of the expressions
        | ErrorBadConstraint
        { errorConstraint   :: Type n }

        -- | Types don't match...
        | ErrorTypeConflict
        { errorTypeLhs      :: (Type n, Effect n, Closure n)
        , errorTypeRhs      :: (Type n, Effect n, Closure n) }

        -- | No binders allowed in left-hand side (right is fine, eg @let@s)
        | ErrorNotFirstOrder
        { errorExp          :: Exp a n }

        -- | All variables must be mentioned in left-hand side,
        --   otherwise they won't get bound.
        | ErrorVarUnmentioned

        -- | I don't want to deal with anonymous variables.
        | ErrorAnonymousBinder
        { errorBinder       :: Bind n }


-- | What side of a rewrite rule we're talking about.
data Side 
        = Lhs | Rhs


instance Pretty Side where
 ppr Lhs = text "lhs"
 ppr Rhs = text "rhs"


instance (Show a, Pretty n, Show n, Eq n) => Pretty (Error a n) where
 ppr err
  = case err of
        ErrorTypeCheck s x e
         -> vcat [ text "Can't typecheck " <> ppr s <> text ":  " <> ppr e
                 , text "While checking "  <> ppr x ]

        ErrorBadConstraint c
         ->        text "Bad constraint: " <> ppr c

        ErrorTypeConflict (tl,el,cl) (tr,er,cr)
         -> vcat [ text "LHS and RHS have different types:"
                 , text "Type L: "      <> ppr tl 
                 , text "Type R: "      <> ppr tr
                 , text "Eff L:  "      <> ppr el
                 , text "Eff R:  "      <> ppr er
                 , text "Clo L:  "      <> ppr cl
                 , text "Clo R:  "      <> ppr cr ]

        ErrorNotFirstOrder x
         -> vcat [ text "No binders allowed in left-hand side."
                 , text "While checking " <> ppr x ]

        ErrorVarUnmentioned 
         ->        text "All variables in rule should be mentioned in left-hand side."

        ErrorAnonymousBinder b
         ->        text "Anonymous binders, just give it a name: " <> ppr b

