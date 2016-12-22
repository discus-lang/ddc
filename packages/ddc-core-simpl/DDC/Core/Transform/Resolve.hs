
-- | Resolve elaborations in a module.
module DDC.Core.Transform.Resolve
        ( resolveModule
        , Error (..))
where
import DDC.Core.Env.EnvT
import DDC.Core.Exp
import DDC.Core.Module
import DDC.Core.Fragment.Profile
import DDC.Type.Exp.Simple.Equiv
import DDC.Data.Pretty  hiding ((<$>))


---------------------------------------------------------------------------------------------------
-- | Resolve elaborations in a module.
resolveModule 
        :: Ord n
        => Profile n
        -> Module a n
        -> Either (Error a n) (Module a n)

resolveModule profile mm
 = do   xBody'  <- resolveExp (contextOfModule profile mm) 
                              (moduleBody mm)
        return  $  mm { moduleBody = xBody' }


-- | Resolve elaborations in an expression.
resolveExp 
        :: Ord n
        => Context n
        -> Exp a n -> Either (Error a n) (Exp a n)

resolveExp ctx xx
 = case xx of

        -- Try to resolve an elaboration.
        XApp a (XPrim _ PElaborate) (RType tWant)
         -> contextResolve a ctx tWant


        -- Boilerplate traversal.
        XPrim{}         -> return xx
        XCon{}          -> return xx
        XVar{}          -> return xx
        XAbs  a p x     -> XAbs  a p <$> resolveExp (contextPushParam p ctx) x
        XApp  a x1 a2   -> XApp  a   <$> resolveExp ctx x1  <*> resolveArg ctx a2
        XLet  a lts x   -> XLet  a   <$> resolveLts ctx lts <*> resolveExp ctx x
        XCase a x alts  -> XCase a   <$> resolveExp ctx x   <*> mapM (resolveAlt ctx) alts
        XCast a c x     -> XCast a c <$> resolveExp ctx x


-- | Resolve elaborations in an argument.
resolveArg 
        :: Ord n
        => Context n
        -> Arg a n -> Either (Error a n) (Arg a n)

resolveArg ctx arg
 = case arg of
        RType{}         -> return arg
        RTerm x         -> RTerm     <$> resolveExp ctx x
        RWitness{}      -> return arg
        RImplicit arg'  -> RImplicit <$> resolveArg ctx arg'


-- | Resolve elaborations in some let bindings.
resolveLts
        :: Ord n
        => Context n
        -> Lets a n -> Either (Error a n) (Lets a n)

resolveLts ctx lts
 = case lts of
        LLet b x        -> LLet b <$> resolveExp ctx x

        LRec bxs    
         -> do  let (bs, xs)    = unzip bxs
                let ctx'        = contextPushBinds bs ctx
                xs'             <- mapM (resolveExp ctx') xs
                let bxs'        = zip bs xs'
                return $ LRec bxs'

        LPrivate{}      -> return lts


-- | Resolve elaborations in an alternative.
resolveAlt 
        :: Ord n
        => Context n
        -> Alt a n -> Either (Error a n) (Alt a n)

resolveAlt ctx alt
 = case alt of
        AAlt w x        -> AAlt w <$> resolveExp (contextPushPat w ctx) x


---------------------------------------------------------------------------------------------------
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

contextOfModule profile mm
 = let
        ntsImport       = [ (n, t)      | (n, ImportValueModule _ _ t _) 
                                        <- moduleImportValues mm ]
   in   Context
        { contextEnvT           = moduleEnvT (profilePrimKinds profile) mm
        , contextBinds          = [ntsImport] }


-- | Push some bindings onto the context.
--   These can then be used to resolve elaborations.
contextPushBinds :: [Bind n] -> Context n -> Context n
contextPushBinds bs ctx
 = let  es   = [ (n, t) | BName n t <- bs ]
   in   ctx { contextBinds = es : contextBinds ctx }


-- | Push a parameter onto the context.
--   This can then be used to resolve elaborations.
contextPushParam :: Param n ->  Context n -> Context n
contextPushParam pp ctx
 = case pp of
        MTerm (BName n t)      
          -> ctx { contextBinds = [(n, t)] : contextBinds ctx }

        _ -> ctx


-- | Push bindings of a pattern onto the context.
--   These can then be used to resolve elaborations.
contextPushPat  :: Pat n -> Context n -> Context n
contextPushPat ww ctx
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
        -> Either (Error a n) (Exp a n)

contextResolve a ctx tWant
 = searchStack (contextBinds ctx)
 where
        searchStack []
         = Left $ ErrorCannotResolve tWant

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


---------------------------------------------------------------------------------------------------
data Error a n
        = ErrorCannotResolve    (Type n)

instance Pretty (Error a n) where
 ppr (ErrorCannotResolve _)
  = text "Cannot resolve elaboration"

