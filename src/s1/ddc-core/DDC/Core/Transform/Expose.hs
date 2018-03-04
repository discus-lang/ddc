
-- | Attach arity of exported top level supers to export declarations.
--
--   We do this so that a client that imports the module has all the
--   information it needs to call the supers, and does not need to
--   look at the definition of the terms themselves.
--
-- TODO: also cut back imported stuff that is not re-exported.
--
module DDC.Core.Transform.Expose
        (exposeModule)
where
import DDC.Core.Module
import DDC.Core.Exp
import DDC.Core.Call
import Data.Maybe
import qualified Data.Map       as Map


-- | Expose information in module meta-data before generation of interface file.
exposeModule :: Ord n => Module a n -> Module a n
exposeModule mm
 = let
        -- Take the arity information for a top-level super.
        takeSuperArity b x
         | BName nSuper _ <- b
         , cs             <- takeCallConsFromExp x
         , Just (csType, csValue, csBox) <- splitStdCallCons cs
         = Just (nSuper, (length csType, length csValue, length csBox))

         | otherwise  = Nothing

        -- Map of names of supers to arities.
        nsLocalArities
         =  Map.fromList $ catMaybes
         $  mapTopBinds takeSuperArity mm

        -- Attach arity information to an export record.
        attachExportArity (n', ex)
         = case ex of
                ExportSourceLocal n t Nothing
                 -> case Map.lookup n nsLocalArities of
                     Just as -> (n', ExportSourceLocal n t (Just as))
                     Nothing -> (n', ex)

                _ -> (n', ex)

   in   mm { moduleExportValues = map attachExportArity
                                $ moduleExportValues mm }

