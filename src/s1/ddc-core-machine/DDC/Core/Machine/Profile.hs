
-- | Language profile for Disciple Core Machine.
module DDC.Core.Machine.Profile
        ( profile
        , lexModuleString
        , lexExpString

        , freshT
        , freshX)
where
import DDC.Core.Machine.Prim
import DDC.Core.Machine.Env
import DDC.Core.Fragment
import DDC.Core.Lexer
import DDC.Type.Exp
import Control.Monad.State.Strict
import DDC.Type.Env             (Env)
import qualified DDC.Type.Env   as Env


-- | Language profile for Disciple Core Machine.
profile :: Profile Name
profile
        = Profile
        { profileName                   = "Machine"
        , profileFeatures               = features
        , profilePrimDataDefs           = primDataDefs
        , profilePrimKinds              = primKindEnv
        , profilePrimTypes              = primTypeEnv
        , profileTypeIsUnboxed          = const False
        , profileNameIsHole             = Nothing
        , profileMakeLiteralName        = Nothing }


features :: Features
features
        = Features
        { featuresTrackedEffects        = False
        , featuresTrackedClosures       = False
        , featuresFunctionalEffects     = False
        , featuresFunctionalClosures    = False
        , featuresEffectCapabilities    = False
        , featuresImplicitRun           = False
        , featuresImplicitBox           = False
        , featuresMetaVariables         = False
        , featuresPartialPrims          = True
        -- NOTE: not sure yet which of these we want.
        -- Looks like partial application allows
        -- (if f is 3 argument function)
        -- > foo (f x)
        -- but not
        -- > let zz = f x
        -- > in foo
        , featuresPartialApplication    = True
        , featuresGeneralApplication    = True
        , featuresNestedFunctions       = True
        , featuresGeneralLetRec         = True
        , featuresDebruijnBinders       = True
        , featuresUnboundLevel0Vars     = False
        , featuresNameShadowing         = False
        , featuresUnusedBindings        = True
        , featuresUnusedMatches         = True }


-- | Lex a string to tokens, using primitive names.
--
--   The first argument gives the starting source line number.
lexModuleString :: String -> Int -> String -> [Located (Token Name)]
lexModuleString sourceName lineStart str
 = map rn $ lexModuleWithOffside sourceName lineStart str
 where
        rn (Located sp strTok)
         = case renameToken readName strTok of
                Just t' -> Located sp t'
                Nothing -> Located sp (KErrorJunk "lexical error")


-- | Lex a string to tokens, using primitive names.
--
--   The first argument gives the starting source line number.
lexExpString :: String -> Int -> String -> [Located (Token Name)]
lexExpString sourceName lineStart str
 = map rn $ lexExp sourceName lineStart str
 where
        rn (Located sp strTok)
         = case renameToken readName strTok of
                Just t' -> Located sp t'
                Nothing -> Located sp (KErrorJunk "lexical error")


-- | Create a new type variable name that is not in the given environment.
freshT :: Env Name -> Bind Name -> State Int Name
freshT env bb
 = do   i       <- get
        put (i + 1)
        let n =  NameVar ("t" ++ show i)
        case Env.lookupName n env of
         Nothing -> return n
         _       -> freshT env bb


-- | Create a new value variable name that is not in the given environment.
freshX :: Env Name -> Bind Name -> State Int Name
freshX env bb
 = do   i       <- get
        put (i + 1)
        let n = NameVar ("x" ++ show i)
        case Env.lookupName n env of
         Nothing -> return n
         _       -> freshX env bb

