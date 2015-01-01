
-- | Lexer for Source Tetra tokens.
module DDC.Source.Tetra.Lexer
        (lexModuleString)
where
import DDC.Source.Tetra.Prim
import DDC.Core.Lexer
import DDC.Data.Token


-- | Lex a string to tokens, using primitive names.
--
--   The first argument gives the starting source line number.
--
--   We're currently re-using the lexer for the core language, which has
--   *mostly* the same lexical structure as Source Tetra.
--   There are a few tokens accepted by one language but not the other,
--   but it'll do for now.
--
lexModuleString :: String -> Int -> String -> [Token (Tok Name)]
lexModuleString sourceName lineStart str
 = map rn $ lexModuleWithOffside sourceName lineStart str
 where rn (Token strTok sp) 
        = case renameTok readName strTok of
                Just t' -> Token t' sp
                Nothing -> Token (KErrorJunk "lexical error") sp
