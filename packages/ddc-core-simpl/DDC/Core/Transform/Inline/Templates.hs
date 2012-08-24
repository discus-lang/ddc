
module DDC.Core.Transform.Inline.Templates
	( lookupTemplateFromModules
	, lookupTemplateFromModule )
where
import DDC.Core.Exp
import DDC.Core.Module
import DDC.Core.Transform.AnonymizeX
import Data.List



-------------------------------------------------------------------------------
-- TODO: Rubbish function to load inliner templates from some modules.
--       It just does a linear search, which won't be good enough in the long-term.
lookupTemplateFromModules 
        :: (Eq n, Ord n)
        => [Module a n] -> n -> Maybe (Exp a n)

lookupTemplateFromModules [] _  = Nothing
lookupTemplateFromModules (m:ms) n
 = case lookupTemplateFromModule m n of
        Nothing -> lookupTemplateFromModules ms n
        Just x  -> Just x


lookupTemplateFromModule 
        :: (Eq n, Ord n)
        => Module a n -> n -> Maybe (Exp a n)

lookupTemplateFromModule mm n
        | XLet _ (LRec bxs) _  <- moduleBody mm
        , Just (_,x)	       <- find (\(BName n' _, _) -> n == n') bxs
        = Just $ anonymizeX x

        | otherwise
        = Nothing


