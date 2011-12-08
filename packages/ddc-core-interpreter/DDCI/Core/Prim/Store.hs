
module DDCI.Core.Prim.Store
        ( Store  (..)
        , Loc    (..)
        , Rgn    (..)
        , SValue (..)
        , SBind  (..)
        
        -- * Operators
        , empty
        , newLoc
        , newRgn,       newRgns
        , addBind
        , allocBind
        , lookupBind)
where
import DDCI.Core.Prim.Base
import DDCI.Core.Prim.Name
import DDC.Core.Exp
import Data.Map                 (Map)
import Data.Set                 (Set)
import qualified Data.Map       as Map

import qualified Data.Set       as Set
import Control.Monad


-- | The store maps locations to store bindings.
data Store
        = Store 
        { -- | Next store location to allocate.
          storeNextLoc  :: Int

          -- | Next region handle to allocate.
        , storeNextRgn  :: Int

          -- | Region handles already allocated.
        , storeRegions  :: Set Rgn

          -- | Map of locations to store bindings,
          --   and the handles for the regions they're in.
        , storeBinds    :: Map Loc (Rgn, SBind) }


-- | Store value, 
--   these are the things that can be kept directly in store bindings.
data SValue
        = SLoc Int
        | SLam (Bind Name) (Exp () Prim Name)
        deriving (Eq, Show)


-- | Store binding.
data SBind 
        = SObj
        { sbindDataTag          :: Name
        , sbindDataArgs         :: [SValue] }

        | SInt  Integer
        deriving (Eq, Show)


-- Operators ------------------------------------------------------------------
-- | An empty store, with no bindings or regions.
empty   :: Store
empty   = Store
        { storeNextLoc  = 1
        , storeNextRgn  = 1
        , storeRegions  = Set.empty
        , storeBinds    = Map.empty }


-- | Create a new location in the store.
newLoc  :: Store -> (Store, Loc)
newLoc store
 = let  loc     = storeNextLoc store
        store'  = store { storeNextLoc  = loc + 1 }
   in   (store', Loc loc)


-- | Create a new region in the store.
newRgn  :: Store -> (Store, Rgn)
newRgn store
 = let  rgn     = storeNextRgn store
        store'  = store { storeNextRgn  = rgn + 1 }
   in   (store', Rgn rgn)


-- | Create several new regions in the store
newRgns :: Int -> Store -> (Store, [Rgn])
newRgns 0     store     = (store, [])
newRgns count store
 = let  rgns    = [ storeNextRgn store .. storeNextRgn store + count - 1]
        store'  = store { storeNextRgn  = storeNextRgn store + count }
   in   (store', map Rgn rgns)
        

-- | Add a store binding to the store, at the given location.
addBind :: Loc -> Rgn -> SBind -> Store -> Store
addBind loc rgn sbind store
        = store 
        { storeBinds    = Map.insert loc (rgn, sbind) (storeBinds store) }


-- | Allocate a new binding into the given region, returning the new location.
allocBind :: Rgn -> SBind -> Store -> (Store, Loc)
allocBind rgn sbind store
 = let  (store1, loc)   = newLoc store
        store2          = addBind loc rgn sbind store1
   in   (store2, loc)


-- | Lookup a store binding from the store.
lookupBind :: Loc -> Store -> Maybe SBind
lookupBind loc store
        = liftM snd $ Map.lookup loc (storeBinds store)

