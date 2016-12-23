
module DDC.Core.Transform.Resolve.Context
        ( Context (..)
        , makeContextOfModule
        , contextPushBinds
        , contextPushParam
        , contextPushPat
        , contextResolve)
where
import DDC.Core.Transform.Resolve.Base
import Control.Monad.IO.Class
import Data.IORef


-- | Context of resolve process.
data Context n
        = Context
        { -- | Current type environment.
          contextEnvT           :: EnvT n

          -- | Available top-level context.
          --   These are supers that are available in other modules, 
          --   that might not already be referenced in the one that we're resolving.
          --
          --   TODO: searcing through all of these during resolution will be 
          --         a performance disaster. We should store them as a map by
          --         the outermost tycon.
        , contextTop            :: [ (n, ImportValue n (Type n)) ]

          -- | Extra top-level context that we've used during resolution.
        , contextTopUsed        :: IORef [(n, ImportValue n (Type n))]

          -- | Stack of binding groups in the environment,
          --   Each of of the inner groups are from bindings at the same level.
        , contextBinds          :: [ [(n, Type n)] ] }


-- | Create the initial context from the given module.
makeContextOfModule 
        :: Ord n 
        => Profile n                      -- ^ Language profile.
        -> [(n, ImportValue n (Type n))]  -- ^ Top-level context from imported modules.
        -> Module a n                      -- ^ Module that we're doing resolution in.
        -> S a n (Context n)

makeContextOfModule !profile !ntsTop !mm
 = do   let ntsImport   = [ (n, t)  | (n, ImportValueModule _ _ t _) 
                                    <- moduleImportValues mm ]

        refTopUsed      <- liftIO $ newIORef []

        return $ Context
         { contextEnvT           = moduleEnvT (profilePrimKinds profile) mm
         , contextTop            = ntsTop 
         , contextTopUsed        = refTopUsed
         , contextBinds          = [ntsImport] }


-- | Push some bindings onto the context.
--   These can then be used to resolve elaborations.
contextPushBinds :: [Bind n] -> Context n -> Context n
contextPushBinds !bs !ctx
 = let  es   = [ (n, t) | BName n t <- bs ]
   in   ctx { contextBinds = es : contextBinds ctx }


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


-- | Try to build a term of the given type, 
--   out of the terms available in the context.
contextResolve 
        :: Ord n
        => a 
        -> Context n 
        -> Type n 
        -> S a n (Exp a n)

contextResolve !a !ctx !tWant
 = searchStack (contextBinds ctx)
 where
        -- Search the module-local binding stack.
        --  If that doesn't work then search the top-level namespace.
        searchStack []
         = searchImports (contextTop ctx)

        searchStack (g : gs)
         = case searchGroup g of
                Just x  -> return x
                Nothing -> searchStack gs

        searchGroup []
         = Nothing

        searchGroup ((nBind, tBind) : nts)
         = if equivT (contextEnvT ctx) tBind tWant 
                then Just (XVar a (UName nBind))
                else searchGroup nts


        -- Search the top-level imported things.
        searchImports []
         = throwE $ ErrorCannotResolve tWant

        searchImports ((nBind, i) : nisMore)
         = case i of
                ImportValueModule{}
                 -> let tBind   = importValueModuleType i
                    in  if equivT (contextEnvT ctx) tBind tWant 
                         then do 
                                liftIO $ modifyIORef' (contextTopUsed ctx)
                                 $ \used -> (nBind, i) : used

                                return (XVar a (UName nBind))

                         else searchImports nisMore

                ImportValueSea{}
                 -> let tBind   = importValueSeaType i
                    in  if equivT (contextEnvT ctx) tBind tWant
                         then do
                                liftIO $ modifyIORef' (contextTopUsed ctx)
                                 $ \used -> (nBind, i) : used

                                return (XVar a (UName nBind))

                         else searchImports nisMore

