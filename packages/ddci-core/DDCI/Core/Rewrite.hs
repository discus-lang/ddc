
module DDCI.Core.Rewrite
        ( SetRuleCommand(..)
        , parseFirstWord
	, parseRewrite
	, showRule )
where
import DDC.Base.Pretty
import qualified DDC.Base.Parser        as BP
import DDC.Core.Eval.Name
import DDC.Core.Parser.Tokens
import DDC.Core.Transform.Rewrite.Rule
import DDC.Core.Transform.Rewrite.Parser
import DDCI.Core.State
import DDCI.Core.IO
import Data.Char


-- | :set rule command
data SetRuleCommand
        = SetAdd String (RewriteRule () Name)
        | SetRemove String
	| SetList
        deriving (Eq, Show)
type Error = String

-- | Return first word and remainder of string
parseFirstWord :: String -> (String,String)
parseFirstWord s = break isSpace $ dropWhile isSpace s

-- | Parse a :set rule command
-- +name rule, name rule: add rewrite rule
-- -name: remove rule
parseRewrite :: String -> Either Error SetRuleCommand
parseRewrite str
 = case dropWhile isSpace str of
	[]		-> Right SetList
	('+':rest)	-> parseAdd rest
	('-':rest)	-> let (name,_) = parseFirstWord rest in
			   Right $ SetRemove name
	rest		-> parseAdd rest

-- | Parse add rule
parseAdd :: String -> Either Error SetRuleCommand
parseAdd str
 =  let (name,rest) = parseFirstWord str in
    goParse name (lexString 0 rest)
 where
        -- Lex and parse the string.
        goParse name toks                
         = case BP.runTokenParser describeTok "<interactive>" pRule toks of
                Left err -> Left $ renderIndent $ ppr err
                Right x  -> Right $ SetAdd name x

-- | Display rule
showRule :: State -> Int -> String -> RewriteRule () Name -> IO ()
showRule state indentBy name rule
 = do	putStr $ (take indentBy $ repeat '\t') ++ name ++ " "
	outDocLn state
	 $  ppr rule
