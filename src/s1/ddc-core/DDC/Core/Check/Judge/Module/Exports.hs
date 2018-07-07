
module DDC.Core.Check.Judge.Module.Exports where
import DDC.Core.Check.Judge.Type.Base           (checkTypeM)
import DDC.Core.Check.Base
import DDC.Core.Module
import DDC.Core.Env.EnvX                        (EnvX)
import DDC.Control.CheckIO                      (throw)


---------------------------------------------------------------------------------------------------
-- | Check exported types.
checkExportTypes
        :: (Show n, Pretty n, Ord n)
        => Config  n
        -> EnvT  n
        -> [(n, ExportType n (Type n))]
        -> CheckM a n [(n, ExportType n (Type n))]

checkExportTypes config env nesrcs
 = let
        ctx     = contextOfEnvT env

        check (n, esrc)
         | Just k          <- takeKindOfExportType esrc
         = do   (k', _, _) <- checkTypeM config ctx UniverseKind k Recon
                return  $ (n, mapKindOfExportType (const k') esrc)

         | otherwise
         = return (n, esrc)
   in do
        -- Check for duplicate exports.
        let dups = findDuplicates $ map fst nesrcs
        (case takeHead dups of
          Just n -> throw $ ErrorExportDuplicate n
          _      -> return ())


        -- Check the kinds of the export specs.
        mapM check nesrcs


---------------------------------------------------------------------------------------------------
-- | Check exported types.
checkExportValues
        :: (Show n, Pretty n, Ord n)
        => Config n
        -> EnvX   n
        -> [(n, ExportValue n (Type n))]
        -> CheckM a n [(n, ExportValue n (Type n))]

checkExportValues config envX nesrcs
 = let
        ctx     = contextOfEnvX envX

        check (n, esrc)
         | Just t          <- takeTypeOfExportValue esrc
         = do   (t', _, _) <- checkTypeM config ctx UniverseSpec t Recon
                return  $ (n, mapTypeOfExportValue (const t') esrc)

         | otherwise
         = return (n, esrc)

   in do
        -- Check for duplicate exports.
        let dups = findDuplicates $ map fst nesrcs
        (case takeHead dups of
          Just n -> throw $ ErrorExportDuplicate n
          _      -> return ())

        -- Check the types of the exported values.
        mapM check nesrcs


