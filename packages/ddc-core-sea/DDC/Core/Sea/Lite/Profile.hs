
-- | Language profile for the Lite fragment of Disciple Core.
module DDC.Core.Sea.Lite.Profile
        ( profile
        , lexString)
where
import DDC.Core.Sea.Lite.Env
import DDC.Core.Sea.Lite.Name
import DDC.Core.Language.Profile
import DDC.Core.Lexer
import DDC.Base.Lexer


-- | Profile for the Lite fragment of Disciple Core.
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
        { featuresRecursion             = True
        , featuresPartialApplication    = True
        , featuresPartialPrims          = False
        , featuresGeneralApplication    = True
        , featuresNestedFunctions       = True
        , featuresLazyBindings          = True
        , featuresDataCtors             = True
        , featuresDebruijnBinders       = True
        , featuresLetRegion             = True
        , featuresMutableRegions        = True
        , featuresLocalRegions          = True
        , featuresGlobalRegions         = True
        , featuresImports               = True
        , featuresNameShadowing         = True
        , featuresUnusedBindings        = True
        , featuresUnusedMatches         = True
        , featuresUnusedImports         = True }


-- | Lex a string to tokens, using primitive names.
--
--   The first argument gives the starting source line number.
lexString :: String -> Int -> String -> [Token (Tok Name)]
lexString sourceName lineStart str
 = map rn $ lexExp sourceName lineStart str
 where rn (Token strTok sp) 
        = case renameTok readName strTok of
                Just t' -> Token t' sp
                Nothing -> Token (KJunk "lexical error") sp


