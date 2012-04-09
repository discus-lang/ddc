
module DDCI.Core.Language
        ( Language      (..)
        , Fragment      (..)
        , languages
        , fragmentZero
        , fragmentEval
        , fragmentSea)
where
import DDCI.Core.Language.Base
import DDCI.Core.Language.Zero
import DDCI.Core.Language.Eval
import DDCI.Core.Language.Sea


-- | Supported language profiles.
--   
--   One of @Zero@, @Eval@, @Sea@.
languages :: [(String, Language)]
languages
 =      [ ( "Zero",     Language fragmentZero)
        , ( "Eval",     Language fragmentEval)
        , ( "Sea",      Language fragmentSea) ]






