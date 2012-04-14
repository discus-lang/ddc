
module DDCI.Core.Language.Base
        ( Language (..)
        , Fragment (..))
where
import DDCI.Core.Mode
import DDC.Core.Language.Profile
import DDC.Core.Lexer
import DDC.Core.Module
import DDC.Core.Exp
import DDC.Base.Pretty
import DDC.Base.Lexer
import DDC.Core.Transform.Namify        (Namifier)
import DDC.Type.Env                     (Env)


-- Language ------------------------------------------------------------------
data Language
        = forall n err. (Ord n, Show n, Pretty n, Pretty (err ()))
        => Language (Fragment n err)

deriving instance Show Language
        

-- Fragment -------------------------------------------------------------------
-- | Language profile wrapper 
data Fragment n (err :: * -> *)
        = forall s. Fragment
        { fragmentProfile       :: Profile n
        , fragmentLexModule     :: Source -> String -> [Token (Tok n)]
        , fragmentLexExp        :: Source -> String -> [Token (Tok n)]
        , fragmentCheckModule   :: forall a. Module a n -> Maybe (err a)
        , fragmentCheckExp      :: forall a. Exp a n    -> Maybe (err a) 

        , fragmentMakeNamifierT :: Env n -> Namifier s n
        , fragmentMakeNamifierX :: Env n -> Namifier s n 
        , fragmentNameZero      :: s }


instance Show (Fragment n err) where
 show frag
  = profileName $ fragmentProfile frag
