
-- | Definition of the store.
--
--   This implements the store in terms of the operational semantics of the
--   core language, and isn't intended to be efficient in a practical sense.
--   If we cared about runtime performance we'd want to use an IOArray or
--   some other mutable structure to hold the bindings, instead of a Data.Map.
--
module DDC.Core.Eval.Store
        ( Store  (..)
        , Loc    (..)
        , Rgn    (..)
        , SBind  (..)
        
        -- * Operators
        , empty
        , newLoc,       newLocs
        , newRgn,       newRgns
        , delRgn
        , hasRgn
        , setGlobal
        , addBind
        , allocBind,    allocBinds
        , lookupBind
        , lookupTypeOfLoc
        , lookupRegionTypeBind)
where
import DDC.Core.Exp
import DDC.Core.Eval.Name
import Control.Monad
import DDC.Core.Pretty          hiding (empty)
import Data.Map                 (Map)
import Data.Set                 (Set)
import qualified Data.Map       as Map
import qualified Data.Set       as Set


-- | The store maps locations to store bindings.
data Store
        = Store 
        { -- | Next store location to allocate.
          storeNextLoc          :: Int

          -- | Next region handle to allocate.
        , storeNextRgn          :: Int

          -- | Region handles already allocated.
        , storeRegions          :: Set Rgn

          -- | Regions that are marked as global, and are not
          --   deallocated with a stack discipline.
        , storeGlobal           :: Set Rgn

          -- | Map of locations to store bindings,
          --   and the handles for the regions they're in.
        , storeBinds            :: Map Loc (Rgn, Type Name, SBind) }
        deriving Show
        

-- | Store binding.
--   These are "naked objects" that can be allocated directly into the heap.
data SBind 
        -- Algebraic data object.
        = SObj
        { sbindDataTag          :: Name
        , sbindDataArgs         :: [Loc] }

        -- Lambda abstraction, used for recursive bindings.
        -- The flag indicates whether the is level-1 (True) or level-0 (False)
        | SLams
        { sbindLamBinds         :: [(Bool, Bind Name)]
        , sbindLamBody          :: Exp () Name }

        -- Thunk, used for lazy evaluation.
        | SThunk
        { sbindThunkExp         :: Exp () Name }
        deriving (Eq, Show)


-- Pretty ---------------------------------------------------------------------
instance Pretty Store where
 ppr (Store nextLoc nextRgn regions global binds)
  = vcat
  [ text "* STORE"
  , text "  NextLoc: " <> text (show nextLoc)
  , text "  NextRgn: " <> text (show nextRgn)

  , text "  Regions: " <> braces (sep  $ punctuate comma 
                                      $ map ppr $ Set.toList regions)

  , text "  Global:  " <> braces (sep  $ punctuate comma
                                      $ map ppr $ Set.toList global)
  , text ""
  , text "  Binds:"
  , vcat $ [  text "   " <> ppr l <> colon <> ppr r <> text " -> " <> ppr sbind 
           <> line
           <> text "      :: " <> ppr t
                | (l, (r, t, sbind)) <- Map.toList binds] ]


instance Pretty SBind where
 ppr (SObj tag [])
  = text "OBJ"   <+> ppr tag

 ppr (SObj tag svs)    
  = text "OBJ"   <+> ppr tag
                 <+> (sep $ map ppr svs)
 
 ppr (SLams fbs x)    
  = text "LAMS"  <+> sep (map (parens . ppr) fbs)
                 <>  text "."
                 <>  text (renderPlain $ ppr x)

 ppr (SThunk x)
  = text "THUNK" <+> text (renderPlain $ ppr x)
 

-- Constructors ---------------------------------------------------------------
-- | An empty store, with no bindings or regions.
empty   :: Store
empty   = Store
        { storeNextLoc  = 1
        , storeNextRgn  = 1
        , storeRegions  = Set.empty
        , storeGlobal   = Set.empty
        , storeBinds    = Map.empty }


-- Locations ------------------------------------------------------------------
-- | Create a new location in the store.
newLoc  :: Store -> (Store, Loc)
newLoc store
 = let  loc     = storeNextLoc store
        store'  = store { storeNextLoc  = loc + 1 }
   in   (store', Loc loc)


-- | Create several new locations in the store.
newLocs :: Int -> Store -> (Store, [Loc])
newLocs n store
 = let  lFirst  = storeNextLoc store
        lLast   = lFirst + n
        
        locs    = [lFirst .. lLast]
        store'  = store { storeNextLoc = lLast + 1 }
    in  (store', map Loc locs)


-- Regions  -------------------------------------------------------------------
-- | Create a new region in the store.
newRgn  :: Store -> (Store, Rgn)
newRgn store
 = let  rgn     = storeNextRgn store
        store'  = store { storeNextRgn  = rgn + 1 
                        , storeRegions  = Set.insert (Rgn rgn) (storeRegions store) }
   in   (store', Rgn rgn)


-- | Create several new regions in the store
newRgns :: Int -> Store -> (Store, [Rgn])
newRgns 0     store     = (store, [])
newRgns count store
 = let  rgns    = map Rgn $ [ storeNextRgn store .. storeNextRgn store + count - 1]
        store'  = store { storeNextRgn  = storeNextRgn store + count 
                        , storeRegions  = Set.union (Set.fromList rgns) (storeRegions store) }
   in   (store', rgns)


-- | Delete a region, removing all its bindings.
delRgn :: Rgn -> Store -> Store
delRgn rgn store
 = let  binds'   = [x | x@(_, (r, _, _)) <- Map.toList $ storeBinds store
                      , r /= rgn ]  
   in   store   { storeBinds    = Map.fromList binds' 
                , storeRegions  = Set.delete rgn (storeRegions store)
                , storeGlobal   = Set.delete rgn (storeGlobal  store) }


-- | Check whether a store contains the given region.
hasRgn :: Store -> Rgn -> Bool
hasRgn store rgn
        = Set.member rgn (storeRegions store)
        

-- | Set a region as being global.
setGlobal :: Rgn -> Store -> Store
setGlobal rgn store
        = store
        { storeGlobal   = Set.insert rgn (storeGlobal store) }


-- Bindings -------------------------------------------------------------------
-- | Add a store binding to the store, at the given location.
addBind :: Loc -> Rgn -> Type Name -> SBind -> Store -> Store
addBind loc rgn t sbind store
        = store 
        { storeBinds    = Map.insert loc (rgn, t, sbind) (storeBinds store) }


-- | Allocate a new binding into the given region,
--    returning the new location.
allocBind :: Rgn -> Type Name -> SBind -> Store -> (Store, Loc)
allocBind rgn t sbind store
 = let  (store1, loc)   = newLoc store
        store2          = addBind loc rgn t sbind store1
   in   (store2, loc)


-- | Alloc some recursive bindings into the given region, 
--     returning the new locations.
allocBinds :: ([[Loc] -> (Rgn, Type Name, SBind)]) -> Store -> (Store, [Loc])
allocBinds mkSBinds store
 = let  n               = length mkSBinds
        (store1, locs)  = newLocs n store
        rgnBinds        = map (\mk -> mk locs) mkSBinds
        store2          = foldr (\(l, (r, t, b)) -> addBind l r t b) store1
                        $ zip locs rgnBinds 
   in   (store2, locs)


-- | Lookup a the binding for a location.
lookupBind :: Loc -> Store -> Maybe SBind
lookupBind loc store
        = liftM (\(_, _, sb) -> sb) 
        $ Map.lookup loc (storeBinds store)


-- | Lookup the type of a store location.
lookupTypeOfLoc :: Loc -> Store -> Maybe (Type Name)
lookupTypeOfLoc loc store
 = case Map.lookup loc (storeBinds store) of
        Nothing         -> Nothing
        Just (_, t, _)  -> Just t

-- | Lookup the region handle and binding for a location.
lookupRegionTypeBind :: Loc -> Store -> Maybe (Rgn, Type Name, SBind)
lookupRegionTypeBind loc store
        = Map.lookup loc (storeBinds store)

