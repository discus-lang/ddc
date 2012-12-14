
-- | The ambient Disciple Core language is specialised to concrete languages
--   by adding primitive operations and optionally restricting the set of 
--   available language features. This specialisation results in user-facing
--   language fragments such as @Disciple Core Lite@ and @Disciple Core Salt@.
module DDC.Core.Fragment
        ( -- * Langauge fragments
          Fragment      (..)
        , Profile       (..)
        , zeroProfile

          -- * Fragment features
        , Feature       (..)
        , Features      (..)
        , zeroFeatures

        -- * Compliance
        , complies
        , compliesWithEnvs
        , Complies
        , Error         (..))
where
import DDC.Core.Fragment.Feature
import DDC.Core.Fragment.Compliance
import DDC.Core.Fragment.Error
import DDC.Core.Fragment.Profile
import DDC.Core.Module
import DDC.Core.Exp
import DDC.Core.Lexer
import DDC.Data.Token


-- Fragment -------------------------------------------------------------------
-- | Carries all the information we need to work on a particular 
--   fragment of the Disciple Core language.
data Fragment n (err :: * -> *)
        = Fragment
        { -- | Language profile for this fragment.
          fragmentProfile       :: Profile n

          -- | File extension to use when dumping modules in this fragment.
        , fragmentExtension     :: String

          -- | Read a name.
        , fragmentReadName      :: String -> Maybe n
        
          -- | Lex module source into tokens,
          --   given the source name and starting line number. 
        , fragmentLexModule     :: String -> Int -> String -> [Token (Tok n)]

          -- | Lex expression source into tokens,
          --   given the source name and starting line number.
        , fragmentLexExp        :: String -> Int -> String -> [Token (Tok n)]

          -- | Perform language fragment specific checks on a module.
        , fragmentCheckModule   :: forall a. Module a n -> Maybe (err a)

          -- | Perform language fragment specific checks on an expression.
        , fragmentCheckExp      :: forall a. Exp a n    -> Maybe (err a) }


instance Show (Fragment n err) where
 show frag
  = profileName $ fragmentProfile frag

