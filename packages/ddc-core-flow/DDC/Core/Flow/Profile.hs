
-- | Language profile for Disciple Core Flow.
module DDC.Core.Flow.Profile
        ( profile
        , lexModuleString
        , lexExpString)
where
import DDC.Core.Flow.Name
import DDC.Core.Flow.Env
import DDC.Core.Fragment
import DDC.Core.Lexer
import DDC.Data.Token


-- | Profile for Disciple Core Flow.
profile :: Profile Name 
profile
        = Profile
        { profileName                   = "Flow"
        , profileFeatures               = features
        , profilePrimDataDefs           = primDataDefs
        , profilePrimKinds              = primKindEnv
        , profilePrimTypes              = primTypeEnv

          -- We don't need to distinguish been boxed and unboxed
          -- because we allow unboxed instantiation.
        , profileTypeIsUnboxed          = const False }


features :: Features
features 
        = Features
        { featuresUntrackedEffects      = True
        , featuresUntrackedClosures     = True
        , featuresPartialPrims          = True
        , featuresPartialApplication    = True
        , featuresGeneralApplication    = False
        , featuresNestedFunctions       = True
        , featuresLazyBindings          = False
        , featuresDebruijnBinders       = True
        , featuresUnboundLevel0Vars     = False
        , featuresUnboxedInstantiation  = True
        , featuresNameShadowing         = False
        , featuresUnusedBindings        = True
        , featuresUnusedMatches         = True }


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


