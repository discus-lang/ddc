
module DDCI.Core.Language
        ( Fragment      (..)
        , CoreFragment  (..)
        , Language      (..)
        , languages)
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
import qualified DDC.Core.Sea.Lite.Profile      as Lite
import qualified DDC.Core.Sea.Lite.Name         as Lite
import qualified DDC.Core.Sea.Output            as Output


-- | Language profile wrapper 
data Language
        = forall n err. Fragment n err
        => Language (Profile n)


data CoreFragment
        = CoreFragmentZero
        | CoreFragmentEval
        | CoreFragmentSea
        deriving (Show, Eq)


-- | Supported language profiles.
--   
--   One of @Zero@, @Eval@, @Sea@.
languages :: [(String, Language)]
languages
 =      [ ("Zero",      Language (zeroProfile :: Profile ZeroName))
        , ("Eval",      Language Eval.evalProfile)
        , ("Lite",      Language Lite.profile)
        , ("Output",    Language Output.profile)  ]


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
 fragmentLex            = Eval.lexString
 fragmentCheckModule    = error "fragmentCheckModule[Eval]: finish me"
 fragmentCheckExp       = Eval.checkCapsX


-- Lite -----------------------------------------------------------------------
-- | Core langauge with some builtin data types.
instance Fragment Lite.Name String where
 fragmentLex            = Lite.lexString
 fragmentCheckModule    = const Nothing
 fragmentCheckExp       = const Nothing


-- Output ------------------------------------------------------------------
-- | Fragment that maps directly onto the C language.
instance Fragment Output.Name String where
 fragmentLex            = Output.lexString
 fragmentCheckModule    = const Nothing
 fragmentCheckExp       = const Nothing




