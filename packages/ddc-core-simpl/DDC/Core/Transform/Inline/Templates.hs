
-- | Retrieving inliner templates from a list of modules.
module DDC.Core.Transform.Inline.Templates
	( InlineSpec(..)
        , lookupTemplateFromModules
	, lookupTemplateFromModule )
where
import DDC.Core.Exp
import DDC.Core.Module
import DDC.Core.Transform.AnonymizeX
import Data.List
import Data.Set                 (Set)
import Data.Map                 (Map)
import qualified Data.Map       as Map
import qualified Data.Set       as Set


-- | Inlining specification says what bindings we should inline
--   from a particular module.
data InlineSpec n
        -- | Inline all bindings from a module,
        --   but exclude some particulars.
        = InlineSpecAll
        { inlineSpecModuleName  :: ModuleName
        , inlineSpecExclude     :: Set n }

        -- | Inline no bindings from a module,
        --   but include some particulars.
        | InlineSpecNone
        { inlineSpecModuleName  :: ModuleName
        , inlineSpecInclude     :: Set n }
        deriving Show


-- | Lookup an inliner template from a list of modules.
---
--   This just does a linear search through all the modules.
--   As we only inline functions defined at top level, we don't need to worry
--   about lifting indices in templates when we go under binders.
--
lookupTemplateFromModules 
        :: (Eq n, Ord n, Show n)
        => Map ModuleName (InlineSpec n)
                                -- ^ Inliner specifications for the modules.
        -> [Module a n]         -- ^ Modules to use for inliner templates.
        -> n 
        -> Maybe (Exp a n)

lookupTemplateFromModules specs mm n
 | m : ms <- mm
 = let  -- If there is no inliner spec then don't inline anything.
        spec    = case Map.lookup (moduleName m) specs of
                        Just s  -> s
                        Nothing -> InlineSpecNone (moduleName m) Set.empty

   in   case lookupTemplateFromModule spec m n of
                Nothing -> lookupTemplateFromModules specs ms n
                Just x  -> Just x

 | otherwise
 = Nothing


lookupTemplateFromModule 
        :: (Eq n, Ord n, Show n)
        => InlineSpec n         -- ^ Inliner specification for this module.
        -> Module a n           -- ^ Module to use for inliner templates.
        -> n    
        -> Maybe (Exp a n)

lookupTemplateFromModule spec mm n
        | shouldInline spec n
        , XLet _ (LRec bxs) _  <- moduleBody mm
        , Just (_,x)	       <- find (\(BName n' _, _) -> n == n') bxs
        = Just $ anonymizeX x

        | otherwise
        = Nothing


-- | Decide whether we should inline the binding with this name based on the 
--   provided inliner specification.
shouldInline
        :: (Ord n, Show n)
        => InlineSpec n -> n -> Bool

shouldInline spec n
 = case spec of
        InlineSpecAll _ except
         | Set.member n except  -> False
         | otherwise            -> True

        InlineSpecNone _ include
         | Set.member n include -> True
         | otherwise            -> False

