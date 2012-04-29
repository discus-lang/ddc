
module DDCI.Core.Language
        ( Language      (..)
        , Fragment      (..)
        , languages
        , fragmentZero
        , fragmentEval
        , fragmentBrine
        , fragmentLite)
where
import DDCI.Core.Language.Base
import DDCI.Core.Language.Zero
import DDCI.Core.Language.Eval
import DDCI.Core.Language.Brine
import DDCI.Core.Language.Lite


-- | Supported language profiles.
--   
--   One of @Zero@, @Eval@, @Sea@.
languages :: [(String, Language)]
languages
 =      [ ( "Zero",     Language fragmentZero)
        , ( "Eval",     Language fragmentEval)
        , ( "Brine",    Language fragmentBrine) 
        , ( "Lite",     Language fragmentLite) ]






