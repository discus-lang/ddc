
module DDC.Driver.Command.RewriteRules
        ( cmdTryReadRules )
where
import DDC.Driver.Source
import DDC.Build.Language
import DDC.Core.Fragment
import DDC.Core.Simplifier
import DDC.Core.Module
import DDC.Core.Lexer
import DDC.Core.Pretty
import DDC.Core.Transform.Reannotate
import DDC.Core.Transform.Rewrite.Rule  hiding (Error)
import DDC.Core.Transform.Rewrite.Parser
import Control.DeepSeq
import System.Directory
import System.IO
import qualified DDC.Base.Parser        as BP
import qualified DDC.Core.Check         as C
import qualified DDC.Core.Parser        as C
import qualified DDC.Type.Env           as Env


-- Read rewrite rules ---------------------------------------------------------
-- | Load and typecheck a module's rewrite rules, using exported and imported
--   definitions from module
cmdTryReadRules 
        :: (Ord n, Show n, Pretty n, NFData n)
        => Fragment n err       -- ^ Language fragment.
        -> FilePath             -- ^ Path to the module.
        -> Module () n          -- ^ Module with types of imports and exports
        -> IO (NamedRewriteRules () n)

cmdTryReadRules frag filePath modu
 = do
        exists  <- doesFileExist filePath
        -- Silently return an empty list if there is no rules file
        case exists of
         False -> return []
         True  -> do
            -- Read the source file
            src        <- readFile filePath
            let source =  SourceFile filePath
            -- Parse and typecheck
            cmdReadRules_parse filePath frag modu source src


cmdReadRules_parse filePath frag modu source src
 = case parse frag modu source src of
    Left err -> do
        putStrLn $ "When reading " ++ filePath
        hPutStrLn stderr err
        return []
    Right rules -> return rules


parse fragment modu source str
 = case BP.runTokenParser describeTok source' 
        (pRuleMany (C.contextOfProfile (fragmentProfile fragment)))
          (fragmentLexExp fragment source' 0 str) of
                Left err -> Left $ renderIndent $ ppr err
                Right rules ->
                  case mapM check' rules of
                    Left err     -> Left  $ renderIndent $ ppr err
                    Right rules' -> Right $ rules'
 where
    -- Typecheck, then clear annotations
    check' (n,r)
     = do r' <- checkRewriteRule config kinds' types' r
          return (show n, reannotate (const ()) r')

    config   = C.configOfProfile (fragmentProfile fragment)
    kinds    = profilePrimKinds  (fragmentProfile fragment)
    types    = profilePrimTypes  (fragmentProfile fragment)

    kindsImp = moduleKindEnv modu
    typesImp = moduleTypeEnv modu

    kindsExp = modulesGetBinds $ moduleExportKinds modu
    typesExp = modulesGetBinds $ moduleExportTypes modu

    -- Final kind and type environments
    kinds'	 = kinds `Env.union` kindsImp `Env.union` kindsExp
    types'	 = types `Env.union` typesImp `Env.union` typesExp

    source'  = nameOfSource source
