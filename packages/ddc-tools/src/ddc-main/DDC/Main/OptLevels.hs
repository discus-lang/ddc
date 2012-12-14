
-- | Define the default optimisation levels.
module DDC.Main.OptLevels 
        ( getSimplLiteOfConfig
        , getSimplSaltOfConfig)
where
import DDC.Main.Config
import DDC.Driver.Command.Load
import DDC.Driver.Command.RewriteRules
import DDC.Build.Builder
import DDC.Build.Platform
import DDC.Core.Simplifier                      (Simplifier)
import DDC.Core.Transform.Inline
import DDC.Core.Transform.Namify
import DDC.Core.Transform.Reannotate
import System.FilePath
import Control.Monad
import Data.Monoid
import Data.Maybe
import qualified DDC.Core.Simplifier            as S
import qualified DDC.Core.Simplifier.Recipe     as S
import qualified DDC.Core.Lite                  as Lite
import qualified DDC.Core.Salt                  as Salt
import qualified DDC.Build.Language.Salt        as Salt
import qualified DDC.Build.Language.Lite        as Lite
import qualified Data.Map                       as Map


-- | Get the simplifier for Lite code from the config.
--   This also reads up all the modules we use for inliner templates.
--
--   We don't want to delay this until all arguments are parsed, 
--   because the simplifier spec also contains the list of modules used
--   as inliner templates, so we need to wait until they're all specified.
--
getSimplLiteOfConfig 
        :: Config -> Builder 
        -> IO (Simplifier Int () Lite.Name)

getSimplLiteOfConfig config builder
 = case configOptLevelLite config of
        OptLevel0       -> opt0_lite config
        OptLevel1       -> opt1_lite config builder


-- | Get the simplifier for Salt code from the config.
--
getSimplSaltOfConfig 
        :: Config -> Builder
        -> IO (Simplifier Int () Salt.Name)

getSimplSaltOfConfig config builder
 = case configOptLevelSalt config of
        OptLevel0       -> opt0_salt config
        OptLevel1       -> opt1_salt config builder


-- Level 0 --------------------------------------------------------------------
-- This just passes the code through unharmed.

-- | Level 0 optimiser for Core Lite code.
opt0_lite :: Config -> IO (Simplifier Int () Lite.Name)
opt0_lite _
        = return $ S.Trans S.Id


-- | Level 0 optimiser for Core Salt code.
opt0_salt :: Config -> IO (Simplifier Int () Salt.Name)
opt0_salt _
        = return $ S.Trans S.Id


-- Level 1 --------------------------------------------------------------------
-- Do full optimsiations.

-- | Level 1 optimiser for Core Lite code.
opt1_lite 
        :: Config -> Builder
        -> IO (Simplifier Int () Lite.Name)

opt1_lite config _builder
 = do
        -- Auto-inline basic numeric code.
        let inlineModulePaths
                =  [ configLibraryPath config </> "lite/base/Data/Numeric/Int.dcl"
                   , configLibraryPath config </> "lite/base/Data/Numeric/Nat.dcl" ]
                ++ (configWithLite config)

        -- Load all the modules that we're using for inliner templates.
        --  If any of these don't load then the 'cmdReadModule' function 
        --  will display the errors.
        minlineModules
                <- liftM sequence
                $  mapM (cmdReadModule Lite.fragment)
                        inlineModulePaths

        let inlineModules
                = map (reannotate (const ()))
                $ fromMaybe (error "Imported modules do not parse.")
                            minlineModules

        -- Optionally load the rewrite rules for each 'with' module
        rules <- mapM (\(m,file) -> cmdTryReadRules Lite.fragment (file ++ ".rules") m)
              $  inlineModules `zip` inlineModulePaths
        let rules' = concat rules

        -- Simplifier to convert to a-normal form.
        let normalizeLite
                = S.anormalize
                        (makeNamifier Lite.freshT)      
                        (makeNamifier Lite.freshX)

        return  $ (S.Trans $ S.Inline 
                           $ lookupTemplateFromModules inlineModules)        
                <> S.Fix 5 (S.beta 
                                <> S.bubble      <> S.flatten 
                                <> normalizeLite <> S.forward
                                <> (S.Trans $ S.Rewrite rules'))


-- | Level 1 optimiser for Core Salt code.
opt1_salt 
        :: Config -> Builder
        -> IO (Simplifier Int () Salt.Name)

opt1_salt config builder
 = do   
        -- Auto-inline the low-level code from the runtime system
        --   that constructs and destructs objects.
        let targetWidth
                = archPointerWidth $ platformArch $ buildTarget builder

        -- The runtime system code comes in different versions, 
        --  depending on the pointer width of the target architecture.
        let inlineModulePaths
                =  [ configLibraryPath config 
                        </> "salt/runtime" ++ show targetWidth </> "Object.dcs"]
                ++ configWithSalt config

        -- Load all the modues that we're using for inliner templates.
        --  If any of these don't load then the 'cmdReadModule' function 
        --  will display the errors.
        minlineModules
                <- liftM sequence
                $  mapM (cmdReadModule Salt.fragment)
                        inlineModulePaths

        let inlineModules
                = map (reannotate (const ()))
                $ fromMaybe (error "Imported modules do not parse.")
                            minlineModules

        -- Optionally load the rewrite rules for each 'with' module
        rules <- mapM (\(m,file) -> cmdTryReadRules Salt.fragment (file ++ ".rules") m)
              $  inlineModules `zip` inlineModulePaths
        let rules' = concat rules


        -- Simplifier to convert to a-normal form.
        let normalizeSalt
                = S.anormalize
                        (makeNamifier Salt.freshT)      
                        (makeNamifier Salt.freshX)
        
        -- Perform rewrites before inlining
        return  $  (S.Trans $ S.Rewrite rules')
                <> (S.Trans $ S.Inline 
                            $ lookupTemplateFromModules Map.empty inlineModules)
                <> S.Fix 5 (S.beta 
                                <> S.bubble      <> S.flatten 
                                <> normalizeSalt <> S.forward
                                <> (S.Trans $ S.Rewrite rules'))

