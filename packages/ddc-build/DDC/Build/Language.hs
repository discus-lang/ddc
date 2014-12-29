
module DDC.Build.Language
        ( Language      (..)
        , Bundle        (..)
        , Fragment      (..)
        , languages
        , languageOfExtension)
where
import DDC.Core.Fragment
import DDC.Build.Language.Base
import DDC.Build.Language.Salt  as Salt
import DDC.Build.Language.Eval  as Eval
import DDC.Build.Language.Flow  as Flow
import DDC.Build.Language.Zero  as Zero
import DDC.Build.Language.Tetra as Tetra


-- | Supported language profiles.
--   
--   One of @Tetra@, @Salt@, @Eval@, @Flow@, @Zero@.
languages :: [(String, Language)]
languages
 =      [ ( "Tetra", Tetra.language) 
        , ( "Salt",  Salt.language)
        , ( "Eval",  Eval.language)
        , ( "Flow",  Flow.language)
        , ( "Zero",  Zero.language) ]


-- | Return the language fragment definition corresponding to the given 
--   file extension. eg @dct@ gives the definition of the Tetra language.
languageOfExtension :: String -> Maybe Language
languageOfExtension ext
 = let  -- Strip of dots at the front.
        -- the 'takeExtension' function from System.FilePath
        -- doens't do this itself.
        ext'     = case ext of 
                        '.' : rest      -> rest
                        _               -> ext
   in case ext' of
        "dct"   -> Just Tetra.language
        "dcs"   -> Just Salt.language
        "dcv"   -> Just Eval.language
        "dcf"   -> Just Flow.language
        "dcz"   -> Just Zero.language
        _       -> Nothing

