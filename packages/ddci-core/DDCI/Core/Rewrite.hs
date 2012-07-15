
module DDCI.Core.Rewrite
        ( SetRuleCommand(..)
        , parseFirstWord
	, parseRewrite
	, showRule )
where
import DDC.Base.Pretty
import DDC.Build.Language
import DDC.Core.Lexer
import DDC.Core.Fragment.Profile
import DDC.Core.Transform.Rewrite.Rule
import DDC.Core.Transform.Rewrite.Parser
import DDCI.Core.State
import DDCI.Core.Output
import Data.Char
import qualified DDC.Core.Check         as C
import qualified DDC.Base.Parser        as BP


-- | :set rule command
data SetRuleCommand a n
        = SetAdd    
                { setRuleName   :: String 
                , setRuleRule   :: RewriteRule a n }

        | SetRemove 
                { setRuleName   :: String }

	| SetList
        deriving (Eq, Show)

type Error = String


-- | Return first word and remainder of string
parseFirstWord :: String -> (String,String)
parseFirstWord s = break isSpace $ dropWhile isSpace s


-- | Parse a :set rule command
--   +name rule, name rule:  add rewrite rule
--   -name                   remove rule
parseRewrite 
        :: (Ord n, Show n, Pretty n)
        => Fragment n err 
        -> String 
        -> Either Error (SetRuleCommand (C.AnTEC () n) n)

parseRewrite fragment str
 = case dropWhile isSpace str of
	[]		-> Right SetList
	('+':rest)	-> parseAdd fragment rest

	('-':rest)	
         -> let (name,_) = parseFirstWord rest 
            in Right $ SetRemove name

	rest            -> parseAdd fragment rest


-- | Parse add rule
parseAdd
        :: (Ord n, Show n, Pretty n)
        => Fragment n err
        -> String 
        -> Either Error (SetRuleCommand (C.AnTEC () n) n)

parseAdd fragment str
 | Fragment profile _ _ _ _ _ _ _ _     <- fragment
 , (name, rest)                         <- parseFirstWord str
 = case BP.runTokenParser describeTok "<interactive>" pRule
          (fragmentLexExp fragment "interactive" 0 rest) of
                Left err -> Left $ renderIndent $ ppr err
                Right rule ->
                  case checkRewriteRule 
                        (profilePrimDataDefs profile)
                        (profilePrimKinds    profile)
                        (profilePrimTypes    profile) rule of
                    Left err    -> Left  $ renderIndent $ ppr err
                    Right rule' -> Right $ SetAdd name rule'


-- | Display rule
showRule :: (Eq n, Pretty n)
         => State -> Int -> String -> RewriteRule a n -> IO ()

showRule state indentBy name rule
 = do	putStr $ (take indentBy $ repeat '\t') ++ name ++ " "
	outDocLn state
	 $  ppr rule

