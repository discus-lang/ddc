
module DDC.Build.Language
        ( Language      (..)
        , Fragment      (..)
        , languages
        , languageOfExtension
        , fragmentLite
        , fragmentSalt
        , fragmentEval
        , fragmentZero)
where
import DDC.Build.Language.Base
import DDC.Build.Language.Lite
import DDC.Build.Language.Salt
import DDC.Build.Language.Eval
import DDC.Build.Language.Zero


-- | Supported language profiles.
--   
--   One of @Zero@, @Eval@, @Lite@m, @Salt@.
languages :: [(String, Language)]
languages
 =      [ ( "Lite",     Language fragmentLite) 
        , ( "Salt",     Language fragmentSalt)
        , ( "Eval",     Language fragmentEval)
        , ( "Zero",     Language fragmentZero) ]


-- | Return the language fragment definition corresponding to the given 
--   file extension. eg "dcl" gives the definition of the Lite language.
languageOfExtension :: String -> Maybe Language
languageOfExtension ext
 = case ext of
        "dcl"   -> Just $ Language fragmentLite
        "dce"   -> Just $ Language fragmentSalt
        "dcv"   -> Just $ Language fragmentEval
        "dcz"   -> Just $ Language fragmentZero
        _       -> Nothing
