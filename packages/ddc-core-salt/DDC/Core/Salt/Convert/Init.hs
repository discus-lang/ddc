
module DDC.Core.Salt.Convert.Init
        (initRuntime)
where
import DDC.Core.Salt.Compounds
import DDC.Core.Salt.Runtime
import DDC.Core.Salt.Name
import DDC.Core.Module
import DDC.Core.Exp
import Data.List


-- | If this it the Main module, 
--   then add code to the 'main' function to initialise the runtime system.
--
--   Returns Nothing if this is the Main module, 
--      but there is no main function.
initRuntime
        :: Config
        -> Module a Name
        -> Maybe (Module a Name)

initRuntime config mm@ModuleCore{}
        | isMainModule mm
        = case initRuntimeTopX config (moduleBody mm) of
                Nothing -> Nothing
                Just x' -> Just $ mm { moduleBody = x'}

        | otherwise     
        = Just mm


-- | Takes the top-level let-bindings of amodule
--      and add code to initialise the runtime system.
initRuntimeTopX :: Config -> Exp a Name -> Maybe (Exp a Name)
initRuntimeTopX config xx
        | XLet a (LRec bxs) x2  <- xx
        , Just (bMain, xMain)   <- find   (isMainBind . fst) bxs
        , bxs_cut               <- filter (not . isMainBind . fst) bxs
        = let   
                -- Initial size of the heap.
                bytes   = configHeapSize config

                xMain'  = hackBodyX (XLet a (LLet LetStrict (BNone tVoid) 
                                                 (xCreate a bytes))) 
                                    xMain

          in    Just $ XLet a (LRec $ bxs_cut ++ [(bMain, xMain')]) x2

        -- This was supposed to be the main Module,
        -- but there was no 'main' function for the program entry point.
        | otherwise
        = Nothing


-- | Apply a worker to the body of some function.
--   Enters into enclosing lambdas.
hackBodyX :: (Exp a n -> Exp a n) -> Exp a n -> Exp a n
hackBodyX f xx
 = case xx of
        XLAM a b x      -> XLAM a b $ hackBodyX f x
        XLam a b x      -> XLam a b $ hackBodyX f x
        _               -> f xx


-- | Check whether this is the bind for the 'main' function.
isMainBind :: Bind Name -> Bool
isMainBind bb
  = case bb of
        (BName (NameVar "main") _)      -> True
        _                               -> False

