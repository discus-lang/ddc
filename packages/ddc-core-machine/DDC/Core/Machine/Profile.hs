
-- | Language profile for Disciple Core Machine.
module DDC.Core.Machine.Profile
        ( profile
        , lexModuleString
        , lexExpString)
where
import DDC.Core.Machine.Prim
import DDC.Core.Machine.Env
import DDC.Core.Fragment
import DDC.Core.Lexer


-- | Language profile for Disciple Core Machine.
profile :: Profile Name 
profile
        = Profile
        { profileName                   = "Machine"
        , profileFeatures               = features
        , profilePrimDataDefs           = primDataDefs
        , profilePrimKinds              = primKindEnv
        , profilePrimTypes              = primTypeEnv
        , profileTypeIsUnboxed          = const False 
        , profileNameIsHole             = Nothing 
        , profileMakeLiteralName        = Nothing }


features :: Features
features 
        = Features
        { featuresTrackedEffects        = False
        , featuresTrackedClosures       = False
        , featuresFunctionalEffects     = False
        , featuresFunctionalClosures    = False
        , featuresEffectCapabilities    = False
        , featuresImplicitRun           = False
        , featuresImplicitBox           = False
        , featuresMetaVariables         = False
        , featuresPartialPrims          = False
        , featuresPartialApplication    = False
        , featuresGeneralApplication    = False
        , featuresNestedFunctions       = True
        , featuresGeneralLetRec         = True
        , featuresDebruijnBinders       = True
        , featuresUnboundLevel0Vars     = False
        , featuresUnboxedInstantiation  = True
        , featuresNameShadowing         = False
        , featuresUnusedBindings        = True
        , featuresUnusedMatches         = True }


-- | Lex a string to tokens, using primitive names.
--
--   The first argument gives the starting source line number.
lexModuleString :: String -> Int -> String -> [Located (Token Name)]
lexModuleString sourceName lineStart str
 = map rn $ lexModuleWithOffside sourceName lineStart str
 where 
        rn (Located sp strTok) 
         = case renameToken readName strTok of
                Just t' -> Located sp t'
                Nothing -> Located sp (KErrorJunk "lexical error")


-- | Lex a string to tokens, using primitive names.
--
--   The first argument gives the starting source line number.
lexExpString :: String -> Int -> String -> [Located (Token Name)]
lexExpString sourceName lineStart str
 = map rn $ lexExp sourceName lineStart str
 where
        rn (Located sp strTok) 
         = case renameToken readName strTok of
                Just t' -> Located sp t'
                Nothing -> Located sp (KErrorJunk "lexical error")


