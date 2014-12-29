
-- | Language profile for Disciple Core Salt.
module DDC.Core.Salt.Profile
        ( profile
        , lexModuleString
        , lexExpString)
where
import DDC.Core.Salt.Env
import DDC.Core.Salt.Name
import DDC.Core.Fragment
import DDC.Core.Lexer
import DDC.Data.Token


-- | Language profile for Disciple Core Salt.
profile :: Profile Name 
profile
        = Profile
        { profileName                   = "Salt"
        , profileFeatures               = features
        , profilePrimDataDefs           = primDataDefs
        , profilePrimKinds              = primKindEnv
        , profilePrimTypes              = primTypeEnv 
        , profileTypeIsUnboxed          = typeIsUnboxed 
        , profileNameIsHole             = Nothing 
        , profileMakeStringName         = Nothing }


-- | The Salt fragment doesn't support many features.
--   No nested functions, no partial application and so on.
features :: Features
features = zeroFeatures
        { featuresFunctionalEffects     = True
        , featuresFunctionalClosures    = True
        , featuresDebruijnBinders       = True
        , featuresUnusedBindings        = True 
        , featuresEffectCapabilities    = True

          -- ISSUE #340: Check for partial application of supers in Salt
          -- fragment check. This is enabled to support the reify# primitive,
          -- which takes the address of a top-level super. However, the Salt
          -- language itself doesn't support general partial application.
          -- The fragment compliance checker should distinguish between these
          -- two cases.
        , featuresPartialApplication    = True }


-- | Lex a string to tokens, using primitive names.
lexModuleString
         :: String      -- ^ Source file name.
         -> Int         -- ^ Starting line number.
         -> String      -- ^ String to parse.
         -> [Token (Tok Name)]
lexModuleString sourceName lineStart str
 = map rn $ lexModuleWithOffside sourceName lineStart str
 where rn (Token strTok sp) 
        = case renameTok readName strTok of
                Just t' -> Token t' sp
                Nothing -> Token (KErrorJunk "lexical error") sp


-- | Lex a string to tokens, using primitive names.
lexExpString
         :: String      -- ^ Source file name.
         -> Int         -- ^ Starting line number.
         -> String      -- ^ String to parse.
         -> [Token (Tok Name)]
lexExpString sourceName lineStart str
 = map rn $ lexExp sourceName lineStart str
 where rn (Token strTok sp) 
        = case renameTok readName strTok of
                Just t' -> Token t' sp
                Nothing -> Token (KErrorJunk "lexical error") sp
