
module DDC.Core.Discus.Check
        (checkModule)
where
import DDC.Core.Discus.Compounds
import DDC.Core.Discus.Error
import DDC.Core.Discus.Prim
import DDC.Core.Module


-- | Perform Core Discus specific checks on a module.
checkModule :: Module a Name -> Maybe (Error a)
checkModule mm

 -- Check that the 'Main' module exports a 'main' function.
 | moduleName mm == ModuleName ["Main"]
 = case lookup (NameVar "main") $ moduleExportValues mm of

        -- Main module does not export any main function.
        Nothing
         -> Just ErrorMainMissing

        -- Main function exports a main function with the correct mode.
        Just (ExportValueLocal (ModuleName ["Main"]) (NameVar "main") tMain _)
         -> let -- .. and the type is ok.
                check
                 | Just (t1, t2)                             <- takeTFun tMain
                 , t1 == tUnit
                 , Just (TyConSpec TcConSusp, [_tEff, tRet]) <- takeTyConApps t2
                 , tRet == tUnit
                 = Nothing

                 -- .. but it has an invalid type.
                 | otherwise
                 = Just (ErrorMainInvalidType tMain)
            in check

        -- Main module exports
        Just _  -> Just ErrorMainInvalidMode

 | otherwise
 = Nothing
