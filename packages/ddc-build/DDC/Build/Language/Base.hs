
module DDC.Build.Language.Base
        ( Language (..)
        , Fragment (..))
where
import DDC.Core.Fragment
import DDC.Core.Lexer
import DDC.Core.Module
import DDC.Core.Exp
import DDC.Core.Check
import DDC.Base.Pretty
import DDC.Data.Token
import Control.DeepSeq
import Data.Typeable
import DDC.Core.Transform.Namify        (Namifier)
import DDC.Type.Env                     (Env)


-- Language ------------------------------------------------------------------
-- | Existential container for a language fragment, and the dictionaries
--   we need to work with its type parameters.
data Language
        = forall n err
        . ( Typeable n, Ord n, Show n
          , Pretty n
          , Pretty (err (AnTEC () n))
          , NFData n)
        => Language (Fragment n err)

deriving instance Show Language
        

-- Fragment -------------------------------------------------------------------
-- | Carries all the information we need to work on a particular 
--   fragment of the Disciple Core language.
data Fragment n (err :: * -> *)
        = forall s. Fragment
        { -- | Language profile for this fragment.
          fragmentProfile       :: Profile n

          -- | File extension to use when dumping modules in this fragment.
        , fragmentExtension     :: String

          -- | Lex module source into tokens,
          --   given the source name and starting line number. 
        , fragmentLexModule     :: String -> Int -> String -> [Token (Tok n)]

          -- | Lex expression source into tokens,
          --   given the source name and starting line number.
        , fragmentLexExp        :: String -> Int -> String -> [Token (Tok n)]

          -- | Perform language fragment specific checks on a module.
        , fragmentCheckModule   :: forall a. Module a n -> Maybe (err a)

          -- | Perform language fragment specific checks on an expression.
        , fragmentCheckExp      :: forall a. Exp a n    -> Maybe (err a) 

          -- | Make a namifier for level-1 names.
        , fragmentMakeNamifierT :: Env n -> Namifier s n

          -- | Make a namifier for level-0 names.
        , fragmentMakeNamifierX :: Env n -> Namifier s n 

          -- | Initial state for above namifiers.
        , fragmentNameZero      :: s }


instance Show (Fragment n err) where
 show frag
  = profileName $ fragmentProfile frag

