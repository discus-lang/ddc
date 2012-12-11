
-- | Language profile for Disciple Core Lite.
module DDC.Core.Lite.Profile
        ( profile
        , lexModuleString
        , lexExpString)
where
import DDC.Core.Lite.Env
import DDC.Core.Lite.Name
import DDC.Core.Fragment
import DDC.Core.Lexer
import DDC.Data.Token


-- | Profile for Disciple Core Lite.
profile :: Profile Name 
profile
        = Profile
        { profileName                   = "Lite"
        , profileFeatures               = features
        , profilePrimDataDefs           = primDataDefs
        , profilePrimKinds              = primKindEnv
        , profilePrimTypes              = primTypeEnv

          -- As we allow unboxed instantiation,
          -- this isn't needed by the compliance check.
        , profileTypeIsUnboxed          = const False }


features :: Features
features 
        = Features
        { featuresUntrackedEffects      = False
        , featuresUntrackedClosures     = False
        , featuresPartialPrims          = False
        , featuresPartialApplication    = True
        , featuresGeneralApplication    = True
        , featuresNestedFunctions       = True
        , featuresLazyBindings          = True
        , featuresDebruijnBinders       = True
        , featuresUnboundLevel0Vars     = False

          -- We allow unboxed instantiation in Lite, though all all
          -- polymorphic functions applied to unboxed types will need
          -- to be specialised before Salt will accept the code.
        , featuresUnboxedInstantiation  = True

        , featuresNameShadowing         = False
        , featuresUnusedBindings        = True
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


