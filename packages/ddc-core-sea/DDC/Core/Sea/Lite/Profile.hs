
-- | Language profile for the Lite fragment of Disciple Core.
module DDC.Core.Sea.Lite.Profile
        ( profile
        , lexString)
where
import DDC.Core.Parser.Lexer
import DDC.Core.Language.Profile
import DDC.Core.Sea.Lite.Env
import DDC.Core.Sea.Lite.Name
import Data.Maybe
import DDC.Base.Lexer
import DDC.Core.Parser.Tokens


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
 where rn (Token t sp) = Token (renameTok readName_ t) sp

       readName_ str'
        = fromMaybe (error $ "DDC.Core.Sea.Lite.Profile: unknown name " ++ show str')
        $ readName str'


