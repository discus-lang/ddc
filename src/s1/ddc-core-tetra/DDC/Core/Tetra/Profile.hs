
-- | Language profile for Disciple Core Tetra
module DDC.Core.Tetra.Profile
        ( profile
        , lexModuleString
        , lexExpString
        , freshT
        , freshX)
where
import DDC.Core.Tetra.Prim
import DDC.Core.Tetra.Env
import DDC.Core.Fragment
import DDC.Core.Lexer
import DDC.Type.Exp
import Control.Monad.State.Strict
import DDC.Type.Env             (Env)
import DDC.Data.SourcePos
import qualified DDC.Type.Env   as Env


-- | Language profile for Disciple Core Tetra.
profile :: Profile Name
profile
        = Profile
        { profileName                   = "Tetra"
        , profileFeatures               = features
        , profilePrimDataDefs           = primDataDefs
        , profilePrimKinds              = primKindEnv
        , profilePrimTypes              = primTypeEnv
        , profileTypeIsUnboxed          = const False
        , profileNameIsHole             = Just isNameHole
        , profileMakeLiteralName        = Just makeLiteralName }


-- | Convert a literal to a Tetra name.
makeLiteralName :: SourcePos -> Literal -> Bool -> Maybe Name
makeLiteralName _ lit True
 = case lit of
        LNat    n               -> Just $ NameLitNat     n
        LInt    i               -> Just $ NameLitInt     i
        LSize   s               -> Just $ NameLitSize    s
        LWord   i b             -> Just $ NameLitWord    i b
        LFloat  f (Just 32)     -> Just $ NameLitFloat   f 32
        LFloat  f (Just 64)     -> Just $ NameLitFloat   f 64
        LChar   c               -> Just $ NameLitChar    c
        LString tx              -> Just $ NameLitTextLit tx
        _                       -> Nothing

makeLiteralName _ _ _
 = Nothing


features :: Features
features
        = Features
        { featuresTrackedEffects        = True
        , featuresTrackedClosures       = False
        , featuresFunctionalEffects     = False
        , featuresFunctionalClosures    = False
        , featuresEffectCapabilities    = True

        -- We don't want to insert implicit casts when type checking
        -- the core code during transformation, but we do insert them
        -- the first time the source
        , featuresImplicitRun           = False
        , featuresImplicitBox           = False

        , featuresMetaVariables         = False

        , featuresPartialPrims          = True
        , featuresPartialApplication    = True
        , featuresGeneralApplication    = True
        , featuresNestedFunctions       = True
        , featuresGeneralLetRec         = True
        , featuresDebruijnBinders       = True
        , featuresUnboundLevel0Vars     = False
        , featuresNameShadowing         = True
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
freshT :: String -> Env Name -> Bind Name -> State Int Name
freshT prefix env bb
 = do   i       <- get
        put (i + 1)
        let n =  NameVar (prefix ++ "t" ++ show i)
        case Env.lookupName n env of
         Nothing -> return n
         _       -> freshT prefix env bb


-- | Create a new value variable name that is not in the given environment.
freshX :: String -> Env Name -> Bind Name -> State Int Name
freshX prefix env bb
 = do   i       <- get
        put (i + 1)
        let n = NameVar (prefix ++ "x" ++ show i)
        case Env.lookupName n env of
         Nothing -> return n
         _       -> freshX prefix env bb
