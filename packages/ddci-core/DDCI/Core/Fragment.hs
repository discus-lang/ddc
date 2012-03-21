
module DDCI.Core.Fragment
        ( Fragment      (..)
        , StateProfile  (..)
        , stateProfiles)
where
import DDC.Core.Language.Profile
import DDC.Core.Parser.Tokens
import DDC.Core.Module
import DDC.Core.Exp
import DDC.Base.Pretty
import DDC.Base.Lexer
import qualified DDC.Core.Parser.Lexer          as Core
import qualified DDC.Core.Eval.Profile          as Eval
import qualified DDC.Core.Eval.Name             as Eval
import qualified DDC.Core.Eval.Check            as Eval
import qualified DDC.Core.Sea.Output.Profile    as SeaOutput
import qualified DDC.Core.Sea.Output.Name       as SeaOutput


-- | Language profile wrapper 
data StateProfile
        = forall n err. Fragment n err
        => StateProfile (Profile n)

-- | Supported language profiles.
--   
--   One of @Zero@, @Eval@.
stateProfiles :: [(String, StateProfile)]
stateProfiles
 =      [ ("Zero",      StateProfile (zeroProfile :: Profile ZeroName))
        , ("Eval",      StateProfile Eval.evalProfile)
        , ("Sea",       StateProfile SeaOutput.outputProfile) ]


-- | Defines the functions we need for each language fragment.
class (Ord n, Show n, Pretty n, Pretty err) 
        => Fragment n err | n -> err where
 fragmentLex           :: Int         -> String -> [Token (Tok n)] 
 fragmentCheckModule   :: Module () n -> Maybe err
 fragmentCheckExp      :: Exp    () n -> Maybe err 


-- Zero -----------------------------------------------------------------------
-- | No features, no primops.
instance Fragment ZeroName String where
 fragmentLex            = lexStringZero
 fragmentCheckModule    = const Nothing
 fragmentCheckExp       = const Nothing

-- Wrap the names we use for the zero fragment, 
-- so they get pretty printed properly.
data ZeroName 
        = ZeroName String
        deriving (Eq, Ord, Show)

instance Pretty ZeroName where
 ppr (ZeroName str) = text str

-- | Lex a string to tokens, using primitive names.
--
--   The first argument gives the starting source line number.
lexStringZero :: Int -> String -> [Token (Tok ZeroName)]
lexStringZero lineStart str
 = map rn $ Core.lexExp lineStart str
 where rn (Token t sp) = Token (renameTok ZeroName t) sp


-- Eval -----------------------------------------------------------------------
-- | Fragment accepted by the evaluator.
instance Fragment Eval.Name Eval.Error where
 fragmentLex          = Eval.lexString
 fragmentCheckModule  = error "fragmentCheckModule[Eval]: finish me"
 fragmentCheckExp     = Eval.checkCapsX


-- SeaOutput ------------------------------------------------------------------
-- | Fragment that maps directly onto the C language.
instance Fragment SeaOutput.Name String where
 fragmentLex            = SeaOutput.lexString
 fragmentCheckModule    = const Nothing
 fragmentCheckExp       = const Nothing




