
-- | The ambient Disciple Core language is specialised to concrete languages
--   by adding primitive operations and optionally restricting the set of 
--   available language features. This specialisation results in user-facing
--   language fragments such as @Disciple Core Tetra@ and @Disciple Core Salt@.
module DDC.Core.Fragment
        ( -- * Langauge fragments
          Fragment      (..)
        , mapProfileOfFragment

        , Profile       (..)
        , mapFeaturesOfProfile
        , zeroProfile

          -- * Fragment features
        , Feature       (..)
        , Features      (..)
        , zeroFeatures
        , setFeature

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
        , fragmentLexModule     :: String -> Int -> String -> [Located (Tok n)]

          -- | Lex expression source into tokens,
          --   given the source name and starting line number.
        , fragmentLexExp        :: String -> Int -> String -> [Located (Tok n)]

          -- | Perform language fragment specific checks on a module.
        , fragmentCheckModule   :: forall a. Module a n -> Maybe (err a)

          -- | Perform language fragment specific checks on an expression.
        , fragmentCheckExp      :: forall a. Exp a n    -> Maybe (err a) }


instance Show (Fragment n err) where
 show frag
  = profileName $ fragmentProfile frag


-- | Apply a function to the profile in a fragment.
mapProfileOfFragment 
        :: (Profile n -> Profile n) 
        -> Fragment n err -> Fragment n err

mapProfileOfFragment f fragment
        = fragment
        { fragmentProfile       = f (fragmentProfile fragment) }