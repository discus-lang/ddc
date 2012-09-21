
module DDCI.Core.Interface.Suppress
        ( eraseImports)
where
import DDC.Core.Module
import qualified Data.Map       as Map


-- | Erase the import list of a module.
eraseImports :: Module a n -> Module a n
eraseImports mm
 = mm   { moduleImportKinds     = Map.empty
        , moduleImportTypes     = Map.empty }
