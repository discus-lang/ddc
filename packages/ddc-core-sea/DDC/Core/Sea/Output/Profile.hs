
-- | Language profile for the Sea Output fragment of Disciple Core.
module DDC.Core.Sea.Output.Profile
        ( profile
        , lexString)
where
import DDC.Core.Parser.Lexer
import DDC.Core.Language.Profile
import DDC.Core.Sea.Output.Env
import DDC.Core.Sea.Output.Name
import DDC.Base.Lexer
import DDC.Core.Parser.Tokens


-- | Language profile for the Sea Output fragment of Disciple Core.
profile :: Profile Name 
profile
        = Profile
        { profileName           = "Sea"
        , profileFeatures       = features
        , profilePrimDataDefs   = primDataDefs
        , profilePrimKinds      = primKindEnv
        , profilePrimTypes      = primTypeEnv }


-- | The Sea Output fragment doesn't support many features.
--   No nested functions, no partial application and so on.
features :: Features
features = zeroFeatures


-- | Lex a string to tokens, using primitive names.
lexString
         :: String      -- ^ Source file name.
         -> Int         -- ^ Starting line number.
         -> String      -- ^ String to parse.
         -> [Token (Tok Name)]
lexString sourceName lineStart str
 = map rn $ lexExp sourceName lineStart str
 where rn (Token strTok sp) 
        = case renameTok readName strTok of
                Just t' -> Token t' sp
                Nothing -> Token (KJunk "lexical error") sp
