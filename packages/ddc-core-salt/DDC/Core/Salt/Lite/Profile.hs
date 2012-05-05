
-- | Language profile for Disciple Core Lite.
module DDC.Core.Salt.Lite.Profile
        ( profile
        , lexModuleString
        , lexExpString)
where
import DDC.Core.Salt.Lite.Env
import DDC.Core.Salt.Lite.Name
import DDC.Core.Fragment.Profile
import DDC.Core.Lexer
import DDC.Base.Lexer


-- | Profile for Disciple Core Lite.
profile :: Profile Name 
profile
        = Profile
        { profileName           = "Lite"
        , profileFeatures       = features
        , profilePrimDataDefs   = primDataDefs
        , profilePrimKinds      = primKindEnv
        , profilePrimTypes      = primTypeEnv }


features :: Features
features 
        = Features
        { featuresPartialPrims          = False
        , featuresGeneralApplication    = True
        , featuresNestedFunctions       = True
        , featuresLazyBindings          = True
        , featuresDebruijnBinders       = True
        , featuresUnboundLevel0Vars     = False
        , featuresNameShadowing         = False
        , featuresUnusedBindings        = True          -- TODO: need to fix compliance checker to enable this
                                                        -- type vars are not being marked as used.
        , featuresUnusedMatches         = False }


-- | Lex a string to tokens, using primitive names.
--
--   The first argument gives the starting source line number.
lexModuleString :: String -> Int -> String -> [Token (Tok Name)]
lexModuleString sourceName lineStart str
 = map rn $ lexModuleWithOffside sourceName lineStart str
 where rn (Token strTok sp) 
        = case renameTok readName strTok of
                Just t' -> Token t' sp
                Nothing -> Token (KJunk "lexical error") sp


-- | Lex a string to tokens, using primitive names.
--
--   The first argument gives the starting source line number.
lexExpString :: String -> Int -> String -> [Token (Tok Name)]
lexExpString sourceName lineStart str
 = map rn $ lexExp sourceName lineStart str
 where rn (Token strTok sp) 
        = case renameTok readName strTok of
                Just t' -> Token t' sp
                Nothing -> Token (KJunk "lexical error") sp


