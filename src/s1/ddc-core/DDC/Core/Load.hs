{-# LANGUAGE UndecidableInstances #-}

-- | \"Loading\" refers to the combination of parsing and type checking.
--   This is the easiest way to turn source tokens into a type-checked
--   abstract syntax tree.
module DDC.Core.Load
        ( C.AnTEC       (..)
        , Error         (..)
        , Mode          (..)
        , CheckTrace    (..)

        -- * Loading modules
        , loadModuleFromFile
        , loadModuleFromString
        , loadModuleFromTokens

        , parseModuleFromString
        , parseModuleFromTokens

        -- * Loading expressions
        , loadExpFromString
        , loadExpFromTokens

        -- * Loading types
        , loadTypeFromString
        , loadTypeFromTokens

        -- * Loading witnesses
        , loadWitnessFromString
        , loadWitnessFromTokens)
where
import DDC.Core.Transform.SpreadX
import DDC.Core.Fragment.Profile
import DDC.Core.Lexer.Tokens
import DDC.Core.Check                           (Mode(..), CheckTrace)
import DDC.Core.Exp
import DDC.Core.Exp.Annot.AnT                   (AnT)
import DDC.Type.Transform.SpreadT
import qualified DDC.Core.Transform.Reannotate  as Reannotate
import DDC.Type.Universe
import DDC.Core.Module
import DDC.Data.Pretty
import DDC.Core.Fragment                        (Fragment)
import qualified DDC.Core.Env.EnvX              as EnvX
import qualified DDC.Core.Fragment              as F
import qualified DDC.Core.Parser                as C
import qualified DDC.Core.Check                 as C
import qualified DDC.Control.Parser             as BP
import qualified Data.Map.Strict                as Map
import Data.Map.Strict                          (Map)
import System.Directory


-- Error ------------------------------------------------------------------------------------------
-- | Things that can go wrong when loading a core thing.
data Error n err
        = ErrorRead       !String
        | ErrorParser     !BP.ParseError
        | ErrorCheckType  !(C.Error BP.SourcePos n)
        | ErrorCheckExp   !(C.Error BP.SourcePos n)
        | ErrorCompliance !(F.Error (C.AnTEC BP.SourcePos n) n)
        | ErrorFragment   !(err (C.AnTEC BP.SourcePos n))


instance ( Eq n, Show n, Pretty n
         , Pretty (err (C.AnTEC BP.SourcePos n)))
        => Pretty (Error n err) where
 ppr err
  = case err of
        ErrorRead str
         -> vcat [ text "While reading."
                 , indent 2 $ text str ]

        ErrorParser     err'
         -> vcat [ text "While parsing."
                 , indent 2 $ ppr err' ]

        ErrorCheckType  err'
         -> vcat [ text "When checking type."
                 , indent 2 $ ppr err' ]

        ErrorCheckExp   err'
         -> vcat [ text "When checking expression."
                 , indent 2 $ ppr err' ]

        ErrorCompliance err'
         -> vcat [ text "During fragment compliance check."
                 , indent 2 $ ppr err' ]

        ErrorFragment err'
         -> vcat [ text "During fragment specific check."
                 , indent 2 $ ppr err' ]


-- Module -----------------------------------------------------------------------------------------
-- | Parse and type check a core module from a file.
loadModuleFromFile
        :: (Eq n, Ord n, Show n, Pretty n)
        => Fragment n err               -- ^ Language fragment definition.
        -> FilePath                     -- ^ File containing source code.
        -> Mode n                       -- ^ Type checker mode.
        -> IO ( Either (Error n err)
                       (Module (C.AnTEC BP.SourcePos n) n)
              , Maybe CheckTrace)

loadModuleFromFile fragment filePath mode
 = do
        -- Check whether the file exists.
        exists  <- doesFileExist filePath
        if not exists
         then return ( Left $ ErrorRead $ "No such file '" ++ filePath ++ "'"
                     , Nothing)
         else do
                -- Read the source file.
                src     <- readFile filePath

                -- Lex the source.
                let toks = (F.fragmentLexModule fragment) filePath 1 src

                return $ loadModuleFromTokens fragment filePath mode toks


-- | Parse and type check a core module from a string.
loadModuleFromString
        :: (Eq n, Ord n, Show n, Pretty n)
        => Fragment n err               -- ^ Language fragment definition.
        -> FilePath                     -- ^ Path to source file for error messages.
        -> Int                          -- ^ Starting line number for error messages.
        -> Mode n                       -- ^ Type checker mode.
        -> String                       -- ^ Program text.
        -> ( Either (Error n err)
                    (Module (C.AnTEC BP.SourcePos n) n)
           , Maybe CheckTrace)

loadModuleFromString fragment filePath lineStart mode src
 = do   let toks = F.fragmentLexModule fragment filePath lineStart src
        loadModuleFromTokens fragment filePath mode toks


-- | Parse and type check a core module from some tokens.
loadModuleFromTokens
        :: (Eq n, Ord n, Show n, Pretty n)
        => Fragment n err               -- ^ Language fragment definition.
        -> FilePath                     -- ^ Path to source file for error messages.
        -> Mode n                       -- ^ Type checker mode.
        -> [Located (Token n)]          -- ^ Source tokens.
        -> ( Either (Error n err)
                    (Module (C.AnTEC BP.SourcePos n) n)
           , Maybe CheckTrace)

loadModuleFromTokens fragment sourceName mode toks'
 = goParse toks'
 where
        -- Type checker config kind and type environments.
        profile = F.fragmentProfile fragment
        config  = C.configOfProfile profile
        kenv    = profilePrimKinds  profile
        tenv    = profilePrimTypes  profile

        -- Parse the tokens.
        goParse toks
         = case BP.runTokenParser describeToken sourceName
                        (C.pModule (C.contextOfProfile profile))
                        toks of
                Left err        -> (Left (ErrorParser err),     Nothing)
                Right mm        -> goCheckType (spreadX kenv tenv mm)

        -- Check that the module is type well-typed.
        goCheckType mm
         = case C.checkModule config mm mode of
                (Left err,  ct) -> (Left (ErrorCheckExp err),   Just ct)
                (Right mm', ct) -> goCheckCompliance ct mm'

        -- Check that the module compiles with the language fragment.
        goCheckCompliance ct mm
         = case F.complies profile mm of
                Just err        -> (Left (ErrorCompliance err), Just ct)
                Nothing         -> goCheckFragment ct mm

        -- Perform fragment specific checks.
        goCheckFragment ct mm
         = case F.fragmentCheckModule fragment mm of
                Just err        -> (Left (ErrorFragment err),   Just ct)
                Nothing         -> (Right mm,                   Just ct)


-- | Parse and type check a core module from a string.
parseModuleFromString
        :: (Eq n, Ord n, Show n, Pretty n)
        => Fragment n err               -- ^ Language fragment definition.
        -> FilePath                     -- ^ Path to source file for error messages.
        -> Int                          -- ^ Starting line number for error messages.
        -> String                       -- ^ Program text.
        -> Either (Error n err) (Module () n)

parseModuleFromString fragment filePath lineStart src
 = do   let toks = F.fragmentLexModule fragment filePath lineStart src
        parseModuleFromTokens fragment filePath toks


parseModuleFromTokens
        :: (Eq n, Ord n, Show n, Pretty n)
        => Fragment n err
        -> FilePath                     -- ^ Path to source file for error messages.
        -> [Located (Token n)]          -- ^ Source tokens.
        -> Either (Error n err) (Module () n)

parseModuleFromTokens fragment sourceName toks'
 = goParse toks'
 where
        profile = F.fragmentProfile fragment
        kenv    = profilePrimKinds  profile
        tenv    = profilePrimTypes  profile

        -- Parse the tokens.
        goParse toks
         = case BP.runTokenParser describeToken sourceName
                        (C.pModule (C.contextOfProfile profile))
                        toks of
                Left err        -> Left  $ ErrorParser err
                Right mm        -> Right $ Reannotate.reannotate (const ()) $ spreadX kenv tenv mm



-- Exp --------------------------------------------------------------------------------------------
-- | Parse and type-check and expression from a string.
loadExpFromString
        :: (Eq n, Ord n, Show n, Pretty n)
        => Fragment n err       -- ^ Language fragment definition.
        -> Map ModuleName (Module (C.AnTEC () n) n)
                                -- ^ Other modules currently in scope.
                                --   We add their exports to the environment.
        -> FilePath             -- ^ Path to source file for error messages.
        -> Mode n               -- ^ Type checker mode.
        -> String               -- ^ Source string.
        -> ( Either (Error n err)
                    (Exp (C.AnTEC BP.SourcePos n) n)
           , Maybe CheckTrace)

loadExpFromString fragment modules sourceName mode src
 = do  let toks = F.fragmentLexExp fragment sourceName 1 src
       loadExpFromTokens fragment modules sourceName mode toks


-- | Parse and check an expression
--   returning it along with its spec, effect and closure
loadExpFromTokens
        :: (Eq n, Ord n, Show n, Pretty n)
        => Fragment n err       -- ^ Language fragment definition.
        -> Map ModuleName (Module (C.AnTEC () n) n)
                                -- ^ Other modules currently in scope.
                                --   We add their exports to the environment.
        -> FilePath             -- ^ Path to source file for error messages.
        -> Mode n               -- ^ Type checker mode.
        -> [Located (Token n)]  -- ^ Source tokens.
        -> ( Either (Error n err)
                    (Exp (C.AnTEC BP.SourcePos n) n)
           , Maybe CheckTrace)

loadExpFromTokens fragment modules sourceName mode toks'
 = goParse toks'
 where
        -- Type checker profile, kind and type environments.
        profile = F.fragmentProfile fragment
        config  = C.configOfProfile  profile

        envx    = modulesEnvX
                        (profilePrimKinds    profile)
                        (profilePrimTypes    profile)
                        (profilePrimDataDefs profile)
                        (Map.elems modules)

        kenv    = EnvX.kindEnvOfEnvX envx
        tenv    = EnvX.typeEnvOfEnvX envx

        -- Parse the tokens.
        goParse toks
         = case BP.runTokenParser describeToken sourceName
                        (C.pExp (C.contextOfProfile profile))
                        toks of
                Left err              -> (Left (ErrorParser err),     Nothing)
                Right t               -> goCheckType (spreadX kenv tenv t)

        -- Check the kind of the type.
        goCheckType x
         = case C.checkExp config envx mode C.DemandNone x of
            (Left err, ct)            -> (Left  (ErrorCheckExp err),  Just ct)
            (Right (x', _, _), ct)    -> goCheckCompliance ct x'

        -- Check that the module compiles with the language fragment.
        goCheckCompliance ct x
         = case F.compliesWithEnvs profile kenv tenv x of
            Just err                  -> (Left (ErrorCompliance err), Just ct)
            Nothing                   -> goCheckFragment ct x

        -- Perform fragment specific checks.
        goCheckFragment ct x
         = case F.fragmentCheckExp fragment x of
            Just err                  -> (Left (ErrorFragment err),   Just ct)
            Nothing                   -> (Right x,                    Just ct)


-- Type -------------------------------------------------------------------------------------------
-- | Parse and check a type from a string, returning it along with its kind.
loadTypeFromString
        :: (Eq n, Ord n, Show n, Pretty n)
        => Fragment n err       -- ^ Language fragment definition.
        -> Universe             -- ^ Universe this type is supposed to be in.
        -> FilePath             -- ^ Path to source file for error messages.
        -> String               -- ^ Source string.
        -> Either (Error n err)
                  (Type n, Kind n)

loadTypeFromString fragment uni sourceName str
 = do  let toks = F.fragmentLexExp fragment sourceName 1 str
       loadTypeFromTokens fragment uni sourceName toks


-- | Parse and check a type from some tokens, returning it along with its kind.
loadTypeFromTokens
        :: (Eq n, Ord n, Show n, Pretty n)
        => Fragment n err       -- ^ Language fragment definition.
        -> Universe             -- ^ Universe this type is supposed to be in.
        -> FilePath             -- ^ Path to source file for error messages.
        -> [Located (Token n)]  -- ^ Source tokens.
        -> Either (Error n err)
                  (Type n, Kind n)

loadTypeFromTokens fragment uni sourceName toks'
 = goParse toks'
 where
        profile = F.fragmentProfile fragment

        -- Parse the tokens.
        goParse toks
         = case BP.runTokenParser describeToken sourceName
                        (C.pType (C.contextOfProfile profile))
                        toks of
                Left err  -> Left (ErrorParser err)
                Right t   -> goCheckType (spreadT (profilePrimKinds profile) t)

        -- Check the kind of the type.
        goCheckType t
         = case C.checkType (C.configOfProfile profile) uni t of
                Left err      -> Left (ErrorCheckType err)
                Right (t', k) -> Right (t', k)


-- Witness ----------------------------------------------------------------------------------------
-- | Parse and check a witness from a string, returning it along with its kind.
loadWitnessFromString
        :: (Eq n, Ord n, Show n, Pretty n)
        => Fragment n err       -- ^ Language fragment profile.
        -> FilePath             -- ^ Path to source file for error messages.
        -> String               -- ^ Source string.
        -> Either (Error n err)
                  (Witness (AnT BP.SourcePos n) n, Type n)

loadWitnessFromString fragment sourceName str
 = do  let toks = F.fragmentLexExp fragment sourceName 1 str
       loadWitnessFromTokens fragment sourceName toks


-- | Parse and check a witness from some tokens, returning it along with its type.
loadWitnessFromTokens
        :: (Eq n, Ord n, Show n, Pretty n)
        => Fragment n err       -- ^ Language fragment profile.
        -> FilePath             -- ^ Path to source file for error messages.
        -> [Located (Token n)]  -- ^ Source tokens.
        -> Either (Error n err)
                  (Witness (AnT BP.SourcePos n) n, Type n)

loadWitnessFromTokens fragment sourceName toks'
 = goParse toks'
 where  -- Type checker config, kind and type environments.
        profile = F.fragmentProfile fragment
        config  = C.configOfProfile profile

        env     = EnvX.fromPrimEnvs
                        (profilePrimKinds    profile)
                        (profilePrimTypes    profile)
                        (profilePrimDataDefs profile)

        kenv    = profilePrimKinds profile
        tenv    = profilePrimTypes profile

        -- Parse the tokens.
        goParse toks
         = case BP.runTokenParser describeToken sourceName
                (C.pWitness (C.contextOfProfile profile))
                toks of
                Left err  -> Left (ErrorParser err)
                Right t   -> goCheckType (spreadX kenv tenv t)

        -- Check the kind of the type.
        goCheckType w
         = case C.checkWitness config env w of
                Left err      -> Left (ErrorCheckExp err)
                Right (w', t) -> Right (w', t)

