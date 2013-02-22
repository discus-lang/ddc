
module DDC.Build.Language
        ( Language      (..)
        , Bundle        (..)
        , Fragment      (..)
        , languages
        , languageOfExtension)
where
import DDC.Core.Fragment
import DDC.Build.Language.Base
import DDC.Build.Language.Lite  as Lite
import DDC.Build.Language.Salt  as Salt
import DDC.Build.Language.Eval  as Eval
import DDC.Build.Language.Flow  as Flow
import DDC.Build.Language.Zero  as Zero


-- | Supported language profiles.
--   
--   One of @Lite@, @Salt@, @Eval@, @Zero@.
languages :: [(String, Language)]
languages
 =      [ ( "Lite", Lite.language)
        , ( "Salt", Salt.language)
        , ( "Eval", Eval.language)
        , ( "Flow", Flow.language)
        , ( "Zero", Zero.language) ]


-- | Return the language fragment definition corresponding to the given 
--   file extension. eg @dcl@ gives the definition of the Lite language.
languageOfExtension :: String -> Maybe Language
languageOfExtension ext
 = let  -- Strip of dots at the front.
        -- the 'takeExtension' function from System.FilePath
        -- doens't do this itself.
        ext'     = case ext of 
                        '.' : rest      -> rest
                        _               -> ext
   in case ext' of
        "dcl"   -> Just Lite.language
        "dcs"   -> Just Salt.language
        "dcv"   -> Just Eval.language
        "dcf"   -> Just Flow.language
        "dcz"   -> Just Zero.language
        _       -> Nothing
