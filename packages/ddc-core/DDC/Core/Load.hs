
-- | \"Loading\" refers to the combination of parsing and type checking.
--   This is the easiest way to turn source tokens into a type-checked 
--   abstract syntax tree.
module DDC.Core.Load
        ( C.AnTEC (..)
        , Error (..)
        , loadModuleFromFile
        , loadModuleFromString
        , loadModuleFromTokens
        , loadExp
        , loadType
        , loadWitness)
where
import DDC.Core.Transform.SpreadX
import DDC.Core.Fragment.Profile
import DDC.Core.Lexer.Tokens
import DDC.Core.Exp
import DDC.Core.Annot.AnT                       (AnT)
import DDC.Type.Transform.SpreadT
import DDC.Core.Module
import DDC.Base.Pretty
import DDC.Data.Token
import qualified DDC.Core.Fragment              as I
import qualified DDC.Core.Parser                as C
import qualified DDC.Core.Check                 as C
import qualified DDC.Type.Check                 as T
import qualified DDC.Type.Env                   as Env
import qualified DDC.Base.Parser                as BP
import Data.Map.Strict                          (Map)
import System.Directory


-- | Things that can go wrong when loading a core thing.
data Error n
        = ErrorRead       !String
        | ErrorParser     !BP.ParseError
        | ErrorCheckType  !(T.Error n)      
        | ErrorCheckExp   !(C.Error BP.SourcePos n)
        | ErrorCompliance !(I.Error (C.AnTEC BP.SourcePos n) n)
        deriving Show


instance (Eq n, Show n, Pretty n) => Pretty (Error n) where
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


-- Module ---------------------------------------------------------------------
-- | Parse and type check a core module from a file.
loadModuleFromFile 
        :: (Eq n, Ord n, Show n, Pretty n)
        => Profile n                    -- ^ Language fragment profile.
        -> (String -> [Token (Tok n)])  -- ^ Function to lex the source file.
        -> FilePath                     -- ^ File containing source code.
        -> IO (Either (Error n)
                      (Module (C.AnTEC BP.SourcePos n) n))

loadModuleFromFile profile lexSource filePath
 = do   
        -- Check whether the file exists.
        exists  <- doesFileExist filePath
        if not exists 
         then return $ Left $ ErrorRead "Cannot read file."
         else do
                -- Read the source file.
                src     <- readFile filePath

                -- Lex the source.
                let toks = lexSource src

                return $ loadModuleFromTokens profile filePath toks


-- | Parse and type check a core module from a string.
loadModuleFromString
        :: (Eq n, Ord n, Show n, Pretty n)
        => Profile n                    -- ^ Language fragment profile.
        -> (String -> [Token (Tok n)])  -- ^ Function to lex the source file.
        -> FilePath                     -- ^ Path to source file for error messages.
        -> String                       -- ^ Program text.
        -> Either (Error n) 
                  (Module (C.AnTEC BP.SourcePos n) n)

loadModuleFromString profile lexSource filePath src
        = loadModuleFromTokens profile filePath (lexSource src)


-- | Parse and type check a core module.
loadModuleFromTokens
        :: (Eq n, Ord n, Show n, Pretty n)
        => Profile n                    -- ^ Language fragment profile.
        -> FilePath                     -- ^ Path to source file for error messages.
        -> [Token (Tok n)]              -- ^ Source tokens.
        -> Either (Error n) 
                  (Module (C.AnTEC BP.SourcePos n) n)

loadModuleFromTokens profile sourceName toks'
 = goParse toks'
 where  
        -- Type checker config kind and type environments.
        config  = C.configOfProfile  profile
        kenv    = profilePrimKinds profile
        tenv    = profilePrimTypes profile

        -- Parse the tokens.
        goParse toks                
         = case BP.runTokenParser describeTok sourceName C.pModule toks of
                Left err  -> Left (ErrorParser err)
                Right mm  -> goCheckType (spreadX kenv tenv mm)

        -- Check that the module is type sound.
        goCheckType mm
         = case C.checkModule config mm of
                Left err  -> Left (ErrorCheckExp err)
                Right mm' -> goCheckCompliance mm'

        -- Check that the module compiles with the language fragment.
        goCheckCompliance mm
         = case I.complies profile mm of
                Just err  -> Left (ErrorCompliance err)
                Nothing   -> Right mm


-- Exp ------------------------------------------------------------------------
-- | Parse and check an expression
--   returning it along with its spec, effect and closure
loadExp
        :: (Eq n, Ord n, Show n, Pretty n)
        => Profile n            -- ^ Language fragment profile.
        -> Map ModuleName (Module (C.AnTEC () n) n)
                                -- ^ Other modules currently in scope.
                                --   We add their exports to the environment.
        -> FilePath             -- ^ Path to source file for error messages.
        -> [Token (Tok n)]      -- ^ Source tokens.
        -> Either (Error n) 
                  (Exp (C.AnTEC BP.SourcePos n) n)

loadExp profile modules sourceName toks'
 = goParse toks'
 where  
        -- Type checker profile, kind and type environments.
        config  = C.configOfProfile  profile
        kenv    = modulesExportKinds modules $ profilePrimKinds profile
        tenv    = modulesExportTypes modules $ profilePrimTypes profile

        -- Parse the tokens.
        goParse toks                
         = case BP.runTokenParser describeTok sourceName C.pExp toks of
                Left err  -> Left (ErrorParser err)
                Right t   -> goCheckType (spreadX kenv tenv t)

        -- Check the kind of the type.
        goCheckType x
         = case C.checkExp config kenv tenv x of
                Left err            -> Left  (ErrorCheckExp err)
                Right (x', _, _, _) -> goCheckCompliance x'

        -- Check that the module compiles with the language fragment.
        goCheckCompliance x 
         = case I.compliesWithEnvs profile kenv tenv x of
                Just err  -> Left (ErrorCompliance err)
                Nothing   -> Right x


-- Type -----------------------------------------------------------------------
-- | Parse and check a type,
--   returning it along with its kind.
loadType
        :: (Eq n, Ord n, Show n, Pretty n)
        => Profile n            -- ^ Language fragment profile.
        -> FilePath             -- ^ Path to source file for error messages.
        -> [Token (Tok n)]      -- ^ Source tokens.
        -> Either (Error n) 
                  (Type n, Kind n)

loadType profile sourceName toks'
 = goParse toks'
 where  
        -- Parse the tokens.
        goParse toks                
         = case BP.runTokenParser describeTok sourceName C.pType toks of
                Left err  -> Left (ErrorParser err)
                Right t   -> goCheckType (spreadT (profilePrimKinds profile) t)

        -- Check the kind of the type.
        goCheckType t
         = case T.checkType (T.configOfProfile profile) Env.empty t of
                Left err  -> Left (ErrorCheckType err)
                Right k   -> Right (t, k)
        


-- Witness --------------------------------------------------------------------
-- | Parse and check a witness,
--   returning it along with its type.
loadWitness
        :: (Eq n, Ord n, Show n, Pretty n)
        => Profile n            -- ^ Language fragment profile.
        -> FilePath             -- ^ Path to source file for error messages.
        -> [Token (Tok n)]      -- ^ Source tokens.
        -> Either (Error n) 
                  (Witness (AnT BP.SourcePos n) n, Type n)

loadWitness profile sourceName toks'
 = goParse toks'
 where  -- Type checker config, kind and type environments.
        config  = C.configOfProfile  profile
        kenv    = profilePrimKinds profile
        tenv    = profilePrimTypes profile

        -- Parse the tokens.
        goParse toks                
         = case BP.runTokenParser describeTok sourceName C.pWitness toks of
                Left err  -> Left (ErrorParser err)
                Right t   -> goCheckType (spreadX kenv tenv t)

        -- Check the kind of the type.
        goCheckType w
         = case C.checkWitness config kenv tenv w of
                Left err      -> Left (ErrorCheckExp err)
                Right (w', t) -> Right (w', t)

