
-- | Language profile for Disciple Core Lite.
module DDC.Core.Lite.Profile
        ( profile
        , lexModuleString
        , lexExpString)
where
import DDC.Core.Lite.Env
import DDC.Core.Lite.Name
import DDC.Core.Fragment.Profile
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
        { featuresClosureTerms          = True
        , featuresPartialPrims          = False
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


