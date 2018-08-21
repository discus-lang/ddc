
module DDC.Core.Transform.Resolve.Base
        ( module DDC.Core.Env.EnvT
        , module DDC.Core.Module
        , module DDC.Core.Exp
        , module DDC.Type.Exp.Simple.Equiv
        , module Control.Monad.Trans.Except

        , S
        , Error (..)
        , resultTyConNameOfType)
where
import DDC.Core.Env.EnvT
import DDC.Core.Module
import DDC.Core.Exp
import DDC.Type.Exp.Simple.Equiv
import DDC.Core.Codec.Text.Pretty
import Control.Monad.Trans.Except
import qualified DDC.Core.Exp.Annot     as C


-------------------------------------------------------------------------------
-- | Monad used during resolution.
--     We need IO so that we can search external interface during elaboration.
type S a n b = ExceptT (Error a n) IO b


-------------------------------------------------------------------------------
data Error a n
        = ErrorCannotResolve (Type n)

instance (Eq n, Pretty n) => Pretty (Error a n) where
 ppr (ErrorCannotResolve tWanted)
  = vcat
  [ text "Cannot resolve elaboration"
  , text " of type: " % ppr tWanted ]


-------------------------------------------------------------------------------
-- | Get the name of the tycon in the result of a function type,
--   if there is one.
resultTyConNameOfType :: Type n -> Maybe n
resultTyConNameOfType tt
 = let  tBody   = case C.takeTForalls tt of
                        Nothing         -> tt
                        Just (_bs, t)   -> t

        tExplicit = snd $ C.takeTFunImplicits tBody

   in   case C.takeTyConApps tExplicit of
         Just (TyConBound n, _) -> Just n
         _                      -> Nothing


