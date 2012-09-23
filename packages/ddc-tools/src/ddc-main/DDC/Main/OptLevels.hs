
-- | Define the default optimisation levels.
module DDC.Main.OptLevels 
        ( getSimplLiteOfConfig
        , getSimplSaltOfConfig)
where
import DDC.Main.Config
import DDC.Driver.Command.Load
import DDC.Build.Builder
import DDC.Build.Platform
import DDC.Core.Check                           (AnTEC)
import DDC.Core.Simplifier                      (Simplifier)
import DDC.Core.Transform.Inline.Templates
import DDC.Core.Transform.Namify
import qualified DDC.Core.Simplifier            as S
import qualified DDC.Core.Simplifier.Recipe     as S
import qualified DDC.Core.Lite.Name             as Lite
import qualified DDC.Core.Salt.Name             as Salt
import qualified DDC.Build.Language.Salt        as Salt
import qualified DDC.Build.Language.Lite        as Lite
import Data.Monoid
import Data.Maybe
import Control.Monad

-- | Get the simplifier for Lite code from the config.
--   This also reads up all the modules we use for inliner templates.
--
--   We don't want to delay this until all arguments are parsed, 
--   because the simplifier spec also contains the list of modules used
--   as inliner templates, so we need to wait until their all specified.
--
getSimplLiteOfConfig 
        :: Config -> Builder 
        -> IO (Simplifier Int (AnTEC () Lite.Name) Lite.Name)

getSimplLiteOfConfig config builder
 = case configOptLevelLite config of
        OptLevel0       -> opt0_lite config
        OptLevel1       -> opt1_lite config builder
        OptCustom _str  -> error "OptLevel: custom optimsations not parsed yet" 


-- | Get the simplifier for Salt code from the config.
--
getSimplSaltOfConfig 
        :: Config -> Builder
        -> IO (Simplifier Int (AnTEC () Salt.Name) Salt.Name)

getSimplSaltOfConfig config builder
 = case configOptLevelSalt config of
        OptLevel0       -> opt0_salt config
        OptLevel1       -> opt1_salt config builder
        OptCustom _str  -> error "OptLevel: custom optimsations not parsed yet" 


-- Level 0 --------------------------------------------------------------------
-- This just passes the code through unharmed.

-- | Level 0 optimiser for Core Lite code.
opt0_lite :: Config -> IO (Simplifier Int (AnTEC () Lite.Name) Lite.Name)
opt0_lite _
        = return $ S.Trans S.Id


-- | Level 0 optimiser for Core Salt code.
opt0_salt :: Config -> IO (Simplifier Int (AnTEC () Salt.Name) Salt.Name)
opt0_salt _
        = return $ S.Trans S.Id


-- Level 1 --------------------------------------------------------------------
-- Do full optimsiations.

-- | Level 1 optimiser for Core Lite code.
opt1_lite 
        :: Config -> Builder
        -> IO (Simplifier Int (AnTEC () Lite.Name) Lite.Name)

opt1_lite config _builder
 = do
        -- Auto-inline basic numeric code.
        -- TODO: we should be able to specify where the this code is.
        let inlineModulePaths
                =  [ "code/lite/base/Data/Numeric/Int.dcl"
                   , "code/lite/base/Data/Numeric/Nat.dcl" ]
                ++ (configWithSalt config)


        -- Load all the modues that we're using for inliner templates.
        --  If any of these don't load then the 'cmdReadModule' function 
        --  will display the errors.
        minlineModules
                <- liftM sequence
                $  mapM (cmdReadModule Lite.fragmentLite)
                        inlineModulePaths

        let inlineModules
                = fromMaybe (error "Imported modules do not parse.")
                            minlineModules

        -- Simplifier to convert to a-normal form.
        let normalizeLite
                = S.anormalize
                        (makeNamifier Lite.freshT)      
                        (makeNamifier Lite.freshX)

        return  $ (S.Trans $ S.Inline 
                           $ lookupTemplateFromModules inlineModules)        
                <> S.Fix 5 (S.beta 
                                <> S.bubble      <> S.flatten 
                                <> normalizeLite <> S.forward)


-- | Level 1 optimiser for Core Salt code.
opt1_salt 
        :: Config -> Builder
        -> IO (Simplifier Int (AnTEC () Salt.Name) Salt.Name)

opt1_salt config builder
 = do   
        -- Auto-inline the low-level code from the runtime system
        --   that constructs and destructs objects.
        let targetWidth
                = archPointerWidth $ platformArch $ buildTarget builder

        -- TODO: we should be able to specify where the RTS code is.
        let inlineModulePaths
                =  [ "code/salt/runtime" ++ show targetWidth ++ "/Object.dce"]
                ++ (configWithSalt config)

        -- Load all the modues that we're using for inliner templates.
        --  If any of these don't load then the 'cmdReadModule' function 
        --  will display the errors.
        minlineModules
                <- liftM sequence
                $  mapM (cmdReadModule Salt.fragmentSalt)
                        inlineModulePaths

        let inlineModules
                = fromMaybe (error "Imported modules do not parse.")
                            minlineModules

        -- Simplifier to convert to a-normal form.
        let normalizeSalt
                = S.anormalize
                        (makeNamifier Salt.freshT)      
                        (makeNamifier Salt.freshX)
        
        return  $ (S.Trans $ S.Inline 
                           $ lookupTemplateFromModules inlineModules)
                <> S.Fix 5 (S.beta 
                                <> S.bubble      <> S.flatten 
                                <> normalizeSalt <> S.forward)



