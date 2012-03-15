module DDC.Core.Transform.Rewrite.Error
    (Error(..))
where

import DDC.Core.Exp

import qualified DDC.Core.Check.Error as C
import DDC.Core.Check.ErrorMessage()

import DDC.Type.Pretty

-- | What can go wrong when checking a rewrite rule?
data Error a n
    -- | Error typechecking left-hand side
    = ErrorTypeCheckLhs
    { errorChecking	:: Exp a n
    , errorCheckError	:: C.Error a n }

    -- | Error typechecking right-hand side
    | ErrorTypeCheckRhs
    { errorChecking	:: Exp a n
    , errorCheckError	:: C.Error a n }

    -- | Types don't match...
    | ErrorTypeConflict
    { errorTypeLhs	:: (Type n, Effect n, Closure n)
    , errorTypeRhs	:: (Type n, Effect n, Closure n) }

instance (Pretty n, Show n, Eq n) => Pretty (Error a n) where
 ppr err
  = case err of
        ErrorTypeCheckLhs x e
         -> vcat [ text "Can't typecheck lhs:  " <> ppr e
		 , text "While checking " <> ppr x ]
        ErrorTypeCheckRhs x e
         -> vcat [ text "Can't typecheck rhs:  " <> ppr e
		 , text "While checking " <> ppr x ]
        ErrorTypeConflict (tl,el,cl) (tr,er,cr)
         -> vcat [ text "LHS and RHS have different types:"
		 , text "Type L: " <> ppr tl 
		 , text "Type R: " <> ppr tr
		 , text "Eff L:  " <> ppr el
		 , text "Eff R:  " <> ppr er
		 , text "Clo L:  " <> ppr cl
		 , text "Clo R:  " <> ppr cr ]
