
module DDC.Build.Language
        ( Language      (..)
        , Fragment      (..)
        , languages
        , fragmentZero
        , fragmentEval
        , fragmentLite
        , fragmentSalt)
where
import DDC.Build.Language.Base
import DDC.Build.Language.Zero
import DDC.Build.Language.Eval
import DDC.Build.Language.Lite
import DDC.Build.Language.Salt


-- | Supported language profiles.
--   
--   One of @Zero@, @Eval@, @Lite@m, @Salt@.
languages :: [(String, Language)]
languages
 =      [ ( "Zero",     Language fragmentZero)
        , ( "Eval",     Language fragmentEval)
        , ( "Lite",     Language fragmentLite) 
        , ( "Salt",     Language fragmentSalt) ]






