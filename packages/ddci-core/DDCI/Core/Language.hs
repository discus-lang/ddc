
module DDCI.Core.Language
        ( Language      (..)
        , Fragment      (..)
        , languages
        , fragmentZero
        , fragmentEval
        , fragmentSea)
where
import DDCI.Core.Mode
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
-- import qualified DDC.Core.Sea.Lite.Profile      as Lite
-- import qualified DDC.Core.Sea.Lite.Name         as Lite
import qualified DDC.Core.Sea.Output            as Output


data Language
        = forall n err. (Ord n, Show n, Pretty n, Pretty (err ()))
        => Language (Fragment n err)

deriving instance Show Language


-- | Supported language profiles.
--   
--   One of @Zero@, @Eval@, @Sea@.
languages :: [(String, Language)]
languages
 =      [ ( "Zero",     Language fragmentZero)
        , ( "Eval",     Language fragmentEval)
        , ( "Sea",      Language fragmentSea) ]


fragmentZero :: Fragment ZeroName ZeroError
fragmentZero 
        = Fragment
        { fragmentProfile       = (zeroProfile :: Profile ZeroName)
        , fragmentLex           = lexStringZero 
        , fragmentCheckModule   = const Nothing
        , fragmentCheckExp      = const Nothing }


fragmentEval :: Fragment Eval.Name Eval.Error
fragmentEval
        = Fragment
        { fragmentProfile       = Eval.evalProfile
        , fragmentLex           = \s str -> Eval.lexString (nameOfSource s) (lineStartOfSource s) str
        , fragmentCheckModule   = error "languages: finish me"
        , fragmentCheckExp      = Eval.checkCapsX }


fragmentSea :: Fragment  Output.Name Output.Error
fragmentSea 
        = Fragment
        { fragmentProfile       = Output.profile 
        , fragmentLex           = \s str -> Output.lexString (nameOfSource s) (lineStartOfSource s) str
        , fragmentCheckModule   = const Nothing
        , fragmentCheckExp      = const Nothing }


-- Fragment -------------------------------------------------------------------
-- | Language profile wrapper 
data Fragment n (err :: * -> *)
        = Fragment
        { fragmentProfile      :: Profile n
        , fragmentLex          :: Source -> String -> [Token (Tok n)]
        , fragmentCheckModule  :: forall a. Module a n -> Maybe (err a)
        , fragmentCheckExp     :: forall a. Exp a n    -> Maybe (err a) }

instance Show (Fragment n err) where
 show frag
  = profileName $ fragmentProfile frag


data ZeroError a
        = ZeroError
        deriving Show

instance Pretty (ZeroError a) where
 ppr ZeroError  = text (show ZeroError)

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
lexStringZero :: Source -> String -> [Token (Tok ZeroName)]
lexStringZero source str
 = map rn $ Core.lexExp (nameOfSource source) (lineStartOfSource source) str
 where rn (Token t sp) = Token (renameTok ZeroName t) sp

