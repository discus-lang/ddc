
-- | Language profile for Disciple Core Salt.
module DDC.Core.Salt.Profile
        ( profile
        , lexModuleString
        , lexExpString)
where
import DDC.Core.Salt.Env
import DDC.Core.Salt.Name
import DDC.Core.Salt.Compounds
import DDC.Core.Fragment.Profile
import DDC.Core.Lexer
import DDC.Type.Exp
import DDC.Type.Predicates
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
        , profileTypeIsUnboxed          = typeIsUnboxed }


-- | The Salt fragment doesn't support many features.
--   No nested functions, no partial application and so on.
features :: Features
features = zeroFeatures
        { featuresUnusedBindings        = True }


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
                Nothing -> Token (KJunk "lexical error") sp


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
                Nothing -> Token (KJunk "lexical error") sp


-- | Check if a type is an unboxed data type.
typeIsUnboxed :: Type Name -> Bool
typeIsUnboxed tt
 = case tt of
        TVar{}          -> False

        -- All plain constructors are unboxed.
        TCon (TyConBound _ k)
         | isDataKind k -> True

        TCon _          -> False

        TForall _ t     -> typeIsUnboxed t

        -- Pointers to objects are boxed.
        TApp{}
         | Just (_tR, tTarget)  <- takeTPtr tt
         , tTarget == tObj
         -> False

        TApp t1 t2      
         -> typeIsUnboxed t1 || typeIsUnboxed t2

        TSum{}          -> False

