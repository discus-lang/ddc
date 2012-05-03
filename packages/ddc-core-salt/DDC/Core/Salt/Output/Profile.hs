
-- | Language profile for Disciple Core Salt.
module DDC.Core.Salt.Output.Profile
        ( profile
        , lexModuleString
        , lexExpString)
where
import DDC.Core.Salt.Output.Env
import DDC.Core.Salt.Output.Name
import DDC.Core.Fragment.Profile
import DDC.Core.Lexer
import DDC.Base.Lexer


-- | Language profile for Disciple Core Salt.
profile :: Profile Name 
profile
        = Profile
        { profileName           = "Salt"
        , profileFeatures       = features
        , profilePrimDataDefs   = primDataDefs
        , profilePrimKinds      = primKindEnv
        , profilePrimTypes      = primTypeEnv }


-- | The Salt fragment doesn't support many features.
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