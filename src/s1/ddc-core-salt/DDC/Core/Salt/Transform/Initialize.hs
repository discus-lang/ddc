
-- | Adding code to initialise the runtime system.
module DDC.Core.Salt.Transform.Initialize
        (initializeModule)
where
import DDC.Core.Salt.Compounds
import DDC.Core.Salt.Runtime
import DDC.Core.Salt.Name
import DDC.Core.Module
import DDC.Core.Exp.Annot
import Data.List


---------------------------------------------------------------------------------------------------
-- | If this it the Main module, then insert a main function for the posix
--   entry point that initialises the runtime system and calls the real main function.
--
--   Returns Nothing if this is the Main module, but there is no main function.
--
initializeModule
        :: Config
        -> Module a Name
        -> Maybe (Module a Name)

initializeModule config mm@ModuleCore{}
 | isMainModule mm
 = case initRuntimeTopX config (moduleBody mm) of
    Nothing
     -> Nothing

    Just x'
     -> Just    $ mm
                { moduleExportValues    = patchMainExports (moduleExportValues mm)
                , moduleBody            = x'}

 | otherwise
 = Just mm


---------------------------------------------------------------------------------------------------
-- | Patch the list of export definitions to export our wrapper instead
--   of the original main function.
patchMainExports
        :: [(Name, ExportValue Name (Type Name))]
        -> [(Name, ExportValue Name (Type Name))]

patchMainExports xx
 = case xx of
        []      -> []

        (x : xs)
         |  (NameVar "main", ExportValueLocal mn n _ mArity) <- x
         -> (NameVar "main", ExportValueLocal mn n tPosixMain mArity) : xs

         |  otherwise
         -> x : patchMainExports xs


-- | Type of the POSIX main function.
tPosixMain :: Type Name
tPosixMain
        = tNat `tFun` tAddr `tFun` tInt


---------------------------------------------------------------------------------------------------
-- | Takes the top-level let-bindings of amodule
--      and add code to initialise the runtime system.
initRuntimeTopX :: Config -> Exp a Name -> Maybe (Exp a Name)
initRuntimeTopX config xx
 | XLet a (LRec bxs) x2  <- xx
 , Just (bMainOrig, xMainOrig)   <- find   (isMainBind . fst) bxs
 , bxs_cut                       <- filter (not . isMainBind . fst) bxs
 , BName _ tMainOrig             <- bMainOrig
 = let
        -- Rename the old main function to '_main$discus'
        bMainOrig'      = BName (NameVar "_main$discus") $ tMainOrig

        -- The new entry point of the program is called 'main'.
        bMainEntry      = BName (NameVar "main")  $ tPosixMain

        xMainEntry      = makeMainEntryX config a

   in   Just $ XLet a
                (LRec $ bxs_cut
                        ++ [ (bMainOrig', xMainOrig)
                           , (bMainEntry, xMainEntry)])
                x2

 -- This was supposed to be the main Module,
 -- but there was no 'main' function for the program entry point.
 | otherwise
 = Nothing


-- | Check whether this is the bind for the 'main' function.
isMainBind :: Bind Name -> Bool
isMainBind bb
  = case bb of
        (BName (NameVar "main") _)      -> True
        _                               -> False


-- | Make the posix main function,
--   which is the entry point to the executable.
makeMainEntryX :: Config -> a -> Exp a Name
makeMainEntryX config a
 = let xU = xAllocBoxed a rTop 0 (xWord a 0 32) (xNat a 0)
                -- TODO: info table index.

   in  XLam    a  (BName (NameVar "argc") tNat)
        $ XLam a  (BName (NameVar "argv") tAddr)

        -- Initialize the runtime system.
        $ XLLet a (BNone tUnit)
                  (xddcInit a (configHeapSize config)
                               (XVar a (UName (NameVar "argc")))
                               (XVar a (UName (NameVar "argv"))))

        -- Call the user level main function.
        $ XLLet a (BNone (tBot kData))
                  (xApps a (XVar a (UName (NameVar "_main$discus"))) [RTerm xU])

        -- Shut down the runtime system.
        $ XLLet a (BNone tVoid)
                  (xddcExit a  0)

        -- Inner dummy expression.
        $ xInt a 0


