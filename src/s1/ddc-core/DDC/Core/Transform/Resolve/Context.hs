
module DDC.Core.Transform.Resolve.Context where
import DDC.Core.Transform.Resolve.Base
import DDC.Core.Fragment                (Profile (..))
import DDC.Core.Interface.Oracle        (Oracle)
import Data.IORef
import Data.Map                         (Map)
import qualified Data.Map               as Map
import Control.Monad.IO.Class


-------------------------------------------------------------------------------
-- | Context of resolve process.
--   TODO: cache resolutions and re-use them.
--         there are often many, eg (Eq Word32) in the same module and we don't
--         want to repeat the resolution process each time.
data Context n
        = Context
        { -- | Interface oracle to access bindings from imported modules.
          contextOracle         :: Oracle n

          -- | Context that holds type synonyms from the current module.
        , contextEnvT           :: EnvT n

          -- | Stack of binding groups in the environment,
          --   Each of of the inner groups are from bindings at the same level.
        , contextBinds          :: [ [(n, Type n)] ]

          -- | Map of imported values that we've used in elaborated expressions.
        , contextImports        :: IORef (Map (ModuleName, n) (ImportValue n (Type n))) }


-------------------------------------------------------------------------------
-- | Create the initial context from the given module.
makeContextOfModule
        :: Ord n
        => Profile n -> Oracle n
        -> Module a n -> S a n (Context n)
makeContextOfModule !profile !oracle !mm
 = do   refImports      <- liftIO $ newIORef Map.empty
        return $ Context
          { contextOracle  = oracle
          , contextEnvT    = moduleEnvT (profilePrimKinds profile) mm
          , contextBinds   = []
          , contextImports = refImports }


-- | Push some bindings onto the context.
--   These can then be used to resolve elaborations.
contextPushBinds :: [Bind n] -> Context n -> Context n
contextPushBinds !bs !ctx
 = let  es   = [ (n, t) | BName n t <- bs ]
   in   ctx { contextBinds = es : contextBinds ctx }


-- | Push some let-bindings onto the contet.
--   These can then be used to resolve elaborations.
contextPushLets :: Lets a n -> Context n -> Context n
contextPushLets !lts !ctx
 = case lts of
        LLet b _        -> contextPushBinds [b]  ctx
        LRec bs         -> contextPushBinds (map fst bs) ctx
        _               -> ctx


-- | Push a parameter onto the context.
--   This can then be used to resolve elaborations.
contextPushParam :: Param n ->  Context n -> Context n
contextPushParam !pp !ctx
 = case pp of
        MTerm (BName n t)
          -> ctx { contextBinds = [(n, t)] : contextBinds ctx }

        MImplicit (BName n t)
          -> ctx { contextBinds = [(n, t)] : contextBinds ctx }

        _ -> ctx


-- | Push bindings of a pattern onto the context.
--   These can then be used to resolve elaborations.
contextPushPat  :: Pat n -> Context n -> Context n
contextPushPat !ww !ctx
 = case ww of
        PDefault        -> ctx
        PData _ bs      -> contextPushBinds bs ctx

