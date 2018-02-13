{-# LANGUAGE TypeFamilies, OverloadedStrings #-}
module DDC.Source.Discus.Transform.Freshen.State
        ( type S
        , State (..),   stateZero
        , Env   (..),   envZero
        , evalState
        , newName
        , withModifiedEnvT
        , withModifiedEnvX
        , mapFreshBinds)
where
import DDC.Source.Discus.Exp
import Data.Monoid
import Data.Set                         (Set)
import Data.Map.Strict                  (Map)
import Data.Text                        (Text)
import qualified Control.Monad.State    as S
import qualified Data.Text              as Text
import qualified Data.Set               as Set
import qualified Data.Map.Strict        as Map


-------------------------------------------------------------------------------
-- | State holding a variable name prefix and counter to
--   create fresh variable names.
type S  = S.State State

data State
        = State
        { -- | Prefix for creating fresh variables.
          stateVarPrefix        :: Text

          -- | Current counter for creating fresh variables.
        , stateVarCount         :: Int

          -- | Environment for type level names.
        , stateEnvT             :: Env

          -- | Environment for value level names.
        , stateEnvX             :: Env
        }


-- | Information about a current environemnt.
data Env
        = Env
        { -- | Stack of names of anonymous binders.
          envStack              :: [Name]

          -- | Length of the above sack.
        , envStackLen           :: Int

          -- | Names currently in scope.
        , envNames              :: Set Name

          -- | Names currently being rewritten.
        , envRename             :: Map Name Name }


-- | The empty environmenet.
envZero :: Env
envZero
        = Env
        { envStack              = []
        , envStackLen           = 0
        , envNames              = Set.empty
        , envRename             = Map.empty }


-- | The starting state.
stateZero :: Text -> State
stateZero prefix
        = State
        { stateVarPrefix        = prefix
        , stateVarCount         = 0
        , stateEnvT             = envZero
        , stateEnvX             = envZero }


-- | Evaluate a desguaring computation,
--   using the given prefix for freshly introduced variables.
evalState :: Text -> S a -> a
evalState prefix c
 = S.evalState c (stateZero prefix)


-- | Allocate a new name.
newName :: Text -> S Name
newName pre
 = do   prefix  <- S.gets stateVarPrefix
        count   <- S.gets stateVarCount
        let name = pre <> "$" <> prefix <> Text.pack (show count)
        S.modify $ \s -> s { stateVarCount = count + 1 }
        return  name


-- | Run a computation in a modified EnvT,
--   restoring the original environment after it's done.
withModifiedEnvT :: (Env -> Env) -> S a -> S a
withModifiedEnvT modEnvT cont
 = do
        state     <- S.get
        let envT  =  stateEnvT state
        let envT' =  modEnvT envT
        S.put state { stateEnvT = envT' }

        result  <- cont

        state'  <- S.get
        S.put state' { stateEnvT = envT }
        return result


-- | Run a computation in a modified EnvX,
--   restoring the original environment after it's done.
withModifiedEnvX :: (Env -> Env) -> S a -> S a
withModifiedEnvX modEnvX cont
 = do
        state     <- S.get
        let envX  =  stateEnvX state
        let envX' =  modEnvX envX
        S.put state { stateEnvX = envX' }

        result  <- cont

        state'  <- S.get
        S.put state' { stateEnvX = envX }
        return result


-- | Given a function that binds and freshens a single thing,
--   binds and freshens a list of things in sequence.
mapFreshBinds
        :: (a  -> ( a  -> S b) -> S b)
        -> [a] -> ([a] -> S b) -> S b

mapFreshBinds freshBind as0 cont
 = go [] as0
 where
        go asAcc []
         = cont (reverse asAcc)

        go asAcc (a : as)
         = freshBind a $ \a' -> go (a' : asAcc) as

