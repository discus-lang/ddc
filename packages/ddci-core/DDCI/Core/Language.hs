
module DDCI.Core.Language
        ( Language      (..)
        , Fragment      (..)
        , languages
        , fragmentZero
        , fragmentEval
        , fragmentLite
        , fragmentSalt)
where
import DDCI.Core.Language.Base
import DDCI.Core.Language.Zero
import DDCI.Core.Language.Eval
import DDCI.Core.Language.Lite
import DDCI.Core.Language.Salt


-- | Supported language profiles.
--   
--   One of @Zero@, @Eval@, @Lite@m, @Salt@.
languages :: [(String, Language)]
languages
 =      [ ( "Zero",     Language fragmentZero)
        , ( "Eval",     Language fragmentEval)
        , ( "Lite",     Language fragmentLite) 
        , ( "Salt",     Language fragmentSalt) ]






