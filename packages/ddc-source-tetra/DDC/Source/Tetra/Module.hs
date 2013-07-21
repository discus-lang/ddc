
module DDC.Source.Tetra.Module
        ( -- * Modules
          Module (..)
        , isMainModule

          -- * Module Names
        , QualName      (..)
        , ModuleName    (..)
        , isMainModuleName)
where
import DDC.Source.Tetra.Exp
import Control.DeepSeq

import DDC.Core.Module          
        ( QualName      (..)
        , ModuleName    (..)
        , isMainModuleName)
        

-- Module ---------------------------------------------------------------------
data Module a n
        = Module
        { -- | Name of this module
          moduleName            :: !ModuleName

          -- Exports ----------------------------
          -- | Names of exported types  (level-1).
        , moduleExportedTypes   :: [n]

          -- | Names of exported values (level-0).
        , moduleExportedValues  :: [n]

          -- Imports ----------------------------
          -- | Imported modules.
        , moduleImportedModules :: [ModuleName]

          -- Local ------------------------------
          -- | Top-level bindings.
        , moduleBindings        :: [Lets a n] }
        deriving Show


instance (NFData a, NFData n) => NFData (Module a n) where
 rnf !mm
        =     rnf (moduleName mm)
        `seq` rnf (moduleExportedTypes   mm)
        `seq` rnf (moduleExportedValues  mm)
        `seq` rnf (moduleImportedModules mm)
        `seq` rnf (moduleBindings        mm)
        

-- | Check if this is the `Main` module.
isMainModule :: Module a n -> Bool
isMainModule mm
        = isMainModuleName
        $ moduleName mm



