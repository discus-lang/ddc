
module DDCI.Core.Token
        ( Token(..)
        , tokenOfString
        , stringOfToken
        , posOfToken)
where
import DDC.Base.Parser
import DDC.Base.Pretty

data Token
        = Token String

tokenOfString :: Token -> String -> Token
tokenOfString _ s = Token s

stringOfToken :: Token -> String
stringOfToken (Token str) = str

posOfToken :: Token -> SourcePos
posOfToken = const (newPos "foo" 0 0)

instance Eq Token where
 (==) (Token s1) (Token s2)
        = s1 == s2

instance Ord Token where
 compare (Token s1) (Token s2)
        = compare s1 s2
        
instance Show Token where
 show (Token s1) = s1
 
instance Pretty Token where
 ppr (Token s1)  = text s1
