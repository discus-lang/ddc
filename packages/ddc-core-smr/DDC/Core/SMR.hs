
module DDC.Core.SMR
        (smrOfTetraModule)
where
import qualified DDC.Core.Check         as E
import qualified DDC.Core.Module        as E
import qualified DDC.Core.Tetra.Prim    as E
import qualified DDC.Type.DataDef       as E
import qualified DDC.Type.Env           as E

import qualified DDC.Core.SMR.Core.Exp  as S

-- | Convert a Core Tetra module to Shimmer code.
smrOfTetraModule
        :: E.DataDefs E.Name                    -- ^ Data type declarations.
        -> E.KindEnv  E.Name                    -- ^ Kind environment.
        -> E.TypeEnv  E.Name                    -- ^ Type environment
        -> E.Module  (E.AnTEC a E.Name) E.Name  -- ^ Tetra module.
        -> S.Module   S.Name S.Name             -- ^ Shimmer module.

smrOfTetraModule _defs _kenv _tenv _mm
 = S.Module []