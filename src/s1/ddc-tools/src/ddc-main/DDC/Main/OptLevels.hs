
-- | Define the default optimisation levels.
module DDC.Main.OptLevels
        ( getSimplSaltOfConfig)
where
import DDC.Main.Config
import DDC.Driver.Command.Read
import DDC.Driver.Command.RewriteRules
import DDC.Build.Builder
import DDC.Build.Platform
import DDC.Core.Module
import DDC.Core.Transform.Inline
import DDC.Core.Transform.Namify
import DDC.Core.Transform.Reannotate
import DDC.Core.Simplifier                      (Simplifier)
import System.FilePath
import Control.Monad
import Data.List
import Data.Monoid
import Data.Maybe
import qualified DDC.Driver.Config              as D
import qualified DDC.Core.Simplifier            as S
import qualified DDC.Core.Simplifier.Recipe     as S
import qualified DDC.Core.Salt                  as Salt
import qualified DDC.Core.Salt.Runtime          as Salt
import qualified DDC.Build.Language.Salt        as Salt
import qualified Data.Map                       as Map
import qualified Data.Set                       as Set

-- | Get the simplifier for Salt code from the config.
--
getSimplSaltOfConfig
        :: Config  -> D.Config
        -> Builder
        -> Salt.Config
        -> Maybe FilePath -- ^ path of current module
        -> IO (Simplifier Int () Salt.Name)

getSimplSaltOfConfig config dconfig builder runtimeConfig filePath
 = case configOptLevelSalt config of
        OptLevel0       -> opt0_salt config
        OptLevel1       -> opt1_salt config dconfig builder runtimeConfig filePath


-- Level 0 --------------------------------------------------------------------
-- This just passes the code through unharmed.

-- | Level 0 optimiser for Core Salt code.
opt0_salt :: Config -> IO (Simplifier Int () Salt.Name)
opt0_salt _
        = return $ S.Trans S.Id


-- Level 1 --------------------------------------------------------------------
-- Do full optimsiations.

-- | Level 1 optimiser for Core Salt code.
opt1_salt
        :: Config -> D.Config
        -> Builder
        -> Salt.Config
        -> Maybe FilePath -- ^ path of current module
        -> IO (Simplifier Int () Salt.Name)

opt1_salt config dconfig builder runtimeConfig filePath
 = do
        -- Auto-inline the low-level code from the runtime system
        --   that constructs and destructs objects.
        let targetWidth
                = archPointerWidth $ platformArch $ buildTarget builder

        -- The runtime system code comes in different versions,
        --  depending on the pointer width of the target architecture.
        let inlineModulePaths
                =  [ configBaseDir config
                        </> "ddc-runtime/salt/runtime" ++ show targetWidth </> "Object.dcs"]
                ++ configWithSalt config

        -- Load all the modues that we're using for inliner templates.
        --  If any of these don't load then the 'cmdReadModule' function
        --  will display the errors.
        minlineModules
                <- liftM sequence
                $  mapM (cmdReadModule dconfig Salt.fragment)
                        inlineModulePaths

        let inlineModules
                = map (reannotate (const ()))
                $ fromMaybe (error "Imported modules do not parse.")
                            minlineModules


        -- Inline simple leaf functions from the runtime system.
        let inlineSpec
                = Map.fromList
                [ ( ModuleName ["Runtime", "Object"]
                  , InlineSpecNone (ModuleName ["Runtime", "Object"])
                        $ Set.fromList
                        $ map Salt.NameVar
                        [ "funThunk", "paramsThunk", "boxesThunk", "argsThunk", "runThunk"
                        , "setThunk", "getThunk"
                        , "getBoxed", "setBoxed"
                        , "getMixed", "payloadMixed"
                        , "payloadRaw",  "payloadSizeRaw"
                        , "payloadSmall"])]


        -- Optionally load the rewrite rules for each 'with' module
        rules <- mapM (\(m,file) -> cmdTryReadRules Salt.fragment (file ++ ".rules") m)
              $  inlineModules `zip` inlineModulePaths

        -- Load rules for target module as well
        modrules <- loadSaltRules dconfig builder runtimeConfig filePath

        let rules' = concat rules ++ modrules


        -- Simplifier to convert to a-normal form.
        let normalizeSalt
                = S.anormalize
                        (makeNamifier Salt.freshT)
                        (makeNamifier Salt.freshX)

        -- Perform rewrites before inlining
        return  $  (S.Trans $ S.Rewrite rules')
                <> (S.Trans $ S.Inline
                            $ lookupTemplateFromModules inlineSpec inlineModules)
                <> S.Fix 5 (S.beta
                                <> S.bubble      <> S.flatten
                                <> normalizeSalt <> S.forward
                                <> (S.Trans $ S.Rewrite rules'))


-- | Load rules for main module
loadSaltRules
    :: D.Config
    -> Builder
    -> Salt.Config
    -> Maybe FilePath
    -> IO (S.NamedRewriteRules () Salt.Name)

loadSaltRules dconfig _builder _runtimeConfig (Just filePath)
 | isSuffixOf ".dcs" filePath
 = do modu      <- cmdReadModule' False dconfig Salt.fragment filePath
      case modu of
       Just mod' -> cmdTryReadRules Salt.fragment (filePath ++ ".rules") (reannotate (const ()) mod')
       Nothing   -> return []

loadSaltRules _ _ _ _
 = return []


