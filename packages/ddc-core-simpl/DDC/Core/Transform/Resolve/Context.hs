
module DDC.Core.Transform.Resolve.Context
        ( Context (..)
        , contextOfModule
        , contextPushBinds
        , contextPushParam
        , contextPushPat
        , contextResolve)
where
import DDC.Core.Transform.Resolve.Base


data Context n
        = Context
        { -- | Current type environment.
          contextEnvT           :: EnvT n

          -- | Stack of binding groups in the environment,
          --   Each of of the inner groups are from bindings at the same level.
        , contextBinds          :: [ [(n, Type n)] ] }


-- | Create the initial context from the given module.
contextOfModule 
        :: Ord n 
        => Profile n -> Module a n -> Context n

contextOfModule !profile !mm
 = let  ntsImport       = [ (n, t)      | (n, ImportValueModule _ _ t _) 
                                        <- moduleImportValues mm ]
   in   Context
        { contextEnvT           = moduleEnvT (profilePrimKinds profile) mm
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
        searchStack []
         = throwE $ ErrorCannotResolve tWant

        searchStack (g : gs)
         = case searchGroup g of
                Nothing -> searchStack gs
                Just x  -> return x


        searchGroup []
         = Nothing

        searchGroup ((nBind, tBind) : nts)
         = if equivT (contextEnvT ctx) tBind tWant 
                then Just (XVar a (UName nBind))
                else searchGroup nts


