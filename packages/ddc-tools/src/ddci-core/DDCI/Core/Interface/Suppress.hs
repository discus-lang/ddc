
module DDCI.Core.Interface.Suppress
        (suppressModule)
where
import DDCI.Core.State
import DDC.Core.Module
import qualified Data.Set       as Set
import qualified Data.Map       as Map


-- | Apply suppression flags to a module to erase information that the user isn't
--   interested in. The module probably won't be type-correct afterwards.
suppressModule :: State -> Module a n -> IO (Module a n)
suppressModule state mm
 = do   let mm2 = if Set.member SuppressImports $ stateModes state
                        then eraseImports mm
                        else mm

        return mm2


-- | Erase the import list of a module.
eraseImports :: Module a n -> Module a n
eraseImports mm
 = mm   { moduleImportKinds     = Map.empty
        , moduleImportTypes     = Map.empty }
