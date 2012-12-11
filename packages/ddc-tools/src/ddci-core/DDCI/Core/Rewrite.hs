
module DDCI.Core.Rewrite
        ( SetRuleCommand(..)
        , parseFirstWord
	, parseRewrite
	, showRule )
where
import DDC.Base.Pretty
import DDC.Build.Language
import DDC.Core.Lexer
import DDC.Core.Fragment                hiding (Error)
import DDC.Core.Transform.Rewrite.Rule  hiding (Error)
import DDC.Core.Transform.Rewrite.Parser
import DDCI.Core.State
import DDCI.Core.Output
import Data.Char
import qualified DDC.Core.Check         as C
import qualified DDC.Base.Parser        as BP

import DDC.Core.Module
import Data.Map                         (Map)


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
	-> Map ModuleName (Module (C.AnTEC () n) n)
        -> String 
        -> Either Error (SetRuleCommand (C.AnTEC () n) n)

parseRewrite fragment modules str
 = case dropWhile isSpace str of
	[]		-> Right SetList
	('+':rest)	-> parseAdd fragment modules rest

	('-':rest)	
         -> let (name,_) = parseFirstWord rest 
            in Right $ SetRemove name

	rest            -> parseAdd fragment modules rest


-- | Parse add rule
parseAdd
        :: (Ord n, Show n, Pretty n)
        => Fragment n err
	-> Map ModuleName (Module (C.AnTEC () n) n)
        -> String 
        -> Either Error (SetRuleCommand (C.AnTEC () n) n)

parseAdd fragment modules str
 | (name, rest) <- parseFirstWord str
 = case BP.runTokenParser describeTok "<interactive>" pRule
          (fragmentLexExp fragment "interactive" 0 rest) of
                Left err -> Left $ renderIndent $ ppr err
                Right rule ->
                  case checkRewriteRule config kinds' types' rule of
                    Left err    -> Left  $ renderIndent $ ppr err
                    Right rule' -> Right $ SetAdd name rule'
 where
        config   = C.configOfProfile (fragmentProfile fragment)
	kinds	 = profilePrimKinds  (fragmentProfile fragment)
	types	 = profilePrimTypes  (fragmentProfile fragment)

	kinds'	 = modulesExportKinds modules kinds
	types'	 = modulesExportTypes modules types


-- | Display rule
showRule :: (Eq n, Pretty n)
         => State -> Int -> String -> RewriteRule a n -> IO ()

showRule state indentBy name rule
 = do	putStr $ (take indentBy $ repeat '\t') ++ name ++ " "
	outDocLn state
	 $  ppr rule

