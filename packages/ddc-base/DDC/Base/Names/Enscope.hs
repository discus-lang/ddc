
-- | Utils for writing enscopers.
module DDC.Base.Names.Enscope
        ( Scope         (..)
        , EnscopeState  (..))
where
--import qualified Data.Map       as Map
import Data.Map                 (Map)


-- | Name scopes.
data Scope name
        -- | A name scope that may contain several names with a given identifier.
        --   This can happen when importing two modules that define the same name into a third.
        = ScopeMulti (Map String [name])

        -- | A local scope that can only contain a single name with a given identifier.
	| ScopeLocal (Map String name)
	deriving Show


-- | Enscoper state.
data EnscopeState s name space
        = EnscopeState
        { -- | Some client defined state.
          stateState    :: s 

          -- | The current stack of scopes, one for each namespace.
        , stateScopes   :: Map space [Scope name]

          -- | Fresh names that hasn't been used before.
        , stateNameSeed :: Map space name

          -- | Generate a new fresh name based on a previous one.
        , stateNameNext :: Map space (name -> name) }

