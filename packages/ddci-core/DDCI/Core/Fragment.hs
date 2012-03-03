
module DDCI.Core.Fragment
        (Fragment (..))
where
import DDC.Core.Parser.Tokens
import DDC.Core.Exp
import DDC.Base.Pretty
import DDC.Base.Lexer
import qualified DDC.Core.Eval.Name               as Eval
import qualified DDC.Core.Eval.Check              as Eval


class (Ord n, Show n, Pretty n, Pretty err) 
        => Fragment n err | n -> err where
 fragmentLex     :: Int     -> String -> [Token (Tok n)] 
 fragmentCheck   :: Exp a n -> Maybe err 


instance Fragment Eval.Name Eval.Error where
 fragmentLex      = Eval.lexString
 fragmentCheck    = Eval.checkCapsX

