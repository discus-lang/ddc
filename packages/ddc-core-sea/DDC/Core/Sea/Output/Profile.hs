
-- | Language profile for the Sea Output fragment of Disciple Core.
module DDC.Core.Sea.Output.Profile
        ( profile
        , lexModuleString
        , lexExpString)
where
import DDC.Core.Sea.Output.Env
import DDC.Core.Sea.Output.Name
import DDC.Core.Language.Profile
import DDC.Core.Lexer
import DDC.Base.Lexer


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
        { featuresUnusedBindings = True }


-- | Lex a string to tokens, using primitive names.
lexModuleString
         :: String      -- ^ Source file name.
         -> Int         -- ^ Starting line number.
         -> String      -- ^ String to parse.
         -> [Token (Tok Name)]
lexModuleString sourceName lineStart str
 = map rn $ lexModuleWithOffside sourceName lineStart str
 where rn (Token strTok sp) 
        = case renameTok readName strTok of
                Just t' -> Token t' sp
                Nothing -> Token (KJunk "lexical error") sp


-- | Lex a string to tokens, using primitive names.
lexExpString
         :: String      -- ^ Source file name.
         -> Int         -- ^ Starting line number.
         -> String      -- ^ String to parse.
         -> [Token (Tok Name)]
lexExpString sourceName lineStart str
 = map rn $ lexExp sourceName lineStart str
 where rn (Token strTok sp) 
        = case renameTok readName strTok of
                Just t' -> Token t' sp
                Nothing -> Token (KJunk "lexical error") sp
