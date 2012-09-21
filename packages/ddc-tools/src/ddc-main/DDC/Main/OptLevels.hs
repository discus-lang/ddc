
-- | Define the default optimisation levels.
module DDC.Main.OptLevels 
        ( simplLiteOfConfig
        , simplSaltOfConfig)
where
import DDC.Main.Config
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
import qualified Data.Map                       as Map
import Data.Monoid


-- | Get the simplifier for Lite code from the config.
--
--   We don't want to delay this until all arguments are parsed, 
--   because the simplifier spec also contains the list of modules used
--   as inliner templates, so we need to wait until their all specified.
--
simplLiteOfConfig :: Config -> Simplifier Int (AnTEC () Lite.Name) Lite.Name
simplLiteOfConfig config
 = case configOptLevelLite config of
        OptLevel0       -> opt0_lite config
        OptLevel1       -> opt1_lite config
        OptCustom _str  -> error "OptLevel: custom optimsations not parsed yet" 


-- | Get the simplifier for Salt code from the config.
--
simplSaltOfConfig :: Config -> Simplifier Int (AnTEC () Salt.Name) Salt.Name
simplSaltOfConfig config
 = case configOptLevelSalt config of
        OptLevel0       -> opt0_salt config
        OptLevel1       -> opt1_salt config
        OptCustom _str  -> error "OptLevel: custom optimsations not parsed yet" 


-- Level 0 --------------------------------------------------------------------
-- This just passes the code through unharmed.

-- | Level 0 optimiser for Core Lite code.
opt0_lite :: Config -> Simplifier Int (AnTEC () Lite.Name) Lite.Name
opt0_lite _
        = S.Trans S.Id


-- | Level 0 optimiser for Core Salt code.
opt0_salt :: Config -> Simplifier Int (AnTEC () Salt.Name) Salt.Name
opt0_salt _
        = S.Trans S.Id


-- Level 1 --------------------------------------------------------------------
-- Do full optimsiations.

-- | Level 1 optimiser for Core Lite code.
opt1_lite :: Config -> Simplifier Int (AnTEC () Lite.Name) Lite.Name
opt1_lite config
 = let  normalizeLite
         = S.anormalize
                (makeNamifier Lite.freshT)      
                (makeNamifier Lite.freshX)

   in   (S.Trans $ S.Inline 
                 $ lookupTemplateFromModules
                        (Map.elems (configWithLite config)))
        
         -- TODO: want to do a fixpoint.
         <> S.beta <> S.bubble <> S.flatten <> normalizeLite <> S.forward 
         <> S.beta <> S.bubble <> S.flatten <> normalizeLite <> S.forward 
         <> S.beta <> S.bubble <> S.flatten <> normalizeLite <> S.forward 
         <> S.beta <> S.bubble <> S.flatten <> normalizeLite <> S.forward 


-- | Level 1 optimiser for Core Salt code.
opt1_salt :: Config -> Simplifier Int (AnTEC () Salt.Name) Salt.Name
opt1_salt config
 = let  normalizeSalt
         = S.anormalize
                (makeNamifier Salt.freshT)      
                (makeNamifier Salt.freshX)

   in   (S.Trans $ S.Inline 
                 $ lookupTemplateFromModules
                        (Map.elems (configWithSalt config)))

         -- hrm. Want a fixpoint here.
         <> S.beta <> S.bubble <> S.flatten <> normalizeSalt <> S.forward
         <> S.beta <> S.bubble <> S.flatten <> normalizeSalt <> S.forward
         <> S.beta <> S.bubble <> S.flatten <> normalizeSalt <> S.forward
         <> S.beta <> S.bubble <> S.flatten <> normalizeSalt <> S.forward
