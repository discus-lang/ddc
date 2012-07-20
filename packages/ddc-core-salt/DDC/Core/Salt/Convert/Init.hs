
module DDC.Core.Salt.Convert.Init
        (initRuntime)
where
import DDC.Core.Salt.Runtime
import DDC.Core.Salt.Env
import DDC.Core.Salt.Platform
import DDC.Core.Salt.Name
import DDC.Core.Module
import DDC.Core.Exp
import Data.List


-- | If this it the Main module, 
--    then add code to the 'main' function to initialise the runtime system.
initRuntime
        :: Platform
        -> Module a Name
        -> Module a Name

initRuntime _pp mm@ModuleCore{}
        | moduleName mm == ModuleName ["Main"]
        = mm    
        { moduleBody    = initRuntimeTopX (moduleBody mm)}

        | otherwise     = mm


initRuntimeTopX :: Exp a Name -> Exp a Name
initRuntimeTopX xx
        | XLet a (LRec bxs) x2  <- xx
        , Just (bMain, xMain)   <- find   (isMainB . fst) bxs
        , bxs_cut               <- filter (not . isMainB . fst) bxs
        = let   bytes   = 100000
                                -- TODO: Heap Size parameterise this

                xMain'  = hackBodyX (XLet a (LLet LetStrict (BNone tVoid) (xCreate a bytes))) xMain

          in    XLet a (LRec $ bxs_cut ++ [(bMain, xMain')]) x2

        | otherwise
        = error "convertInitX: no main function"

hackBodyX :: (Exp a n -> Exp a n) -> Exp a n -> Exp a n
hackBodyX f xx
 = case xx of
        XLAM a b x      -> XLAM a b $ hackBodyX f x
        XLam a b x      -> XLam a b $ hackBodyX f x
        _               -> f xx


isMainB :: Bind Name -> Bool
isMainB bb
  = case bb of
        (BName (NameVar "main") _)      -> True
        _                               -> False

