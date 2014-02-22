
module DDC.Core.Tetra.Check
        (checkModule)
where
import DDC.Core.Tetra.Error
import DDC.Core.Tetra.Prim
import DDC.Core.Module


-- | Perform Core Tetra specific checks on a module.
checkModule :: Module a Name -> Maybe (Error a)
checkModule mm
 | moduleName mm == ModuleName ["Main"]
 , not $ elem (NameVar "main") $ map fst $ moduleExportValues mm 
 = error "Farkness"

 | otherwise
 = Nothing
