
-- | Resolve elaborations in a module.
module DDC.Core.Transform.Resolve
        ( resolveModule
        , Error (..))
where
import DDC.Core.Transform.Resolve.Context
import DDC.Core.Transform.Resolve.Base
import DDC.Core.Fragment                        (Profile (..))
import DDC.Core.Pretty                          hiding ((<$>))
import Control.Monad.IO.Class
import Data.IORef
import qualified Data.Map                       as Map


-- | Resolve elaborations in a module.
resolveModule
        :: (Ord n, Pretty n, Show n)
        => Profile n                      -- ^ Language profile.
        -> [(n, ImportValue n (Type n))]  -- ^ Top-level context from imported modules.
        -> Module a n                     -- ^ Module to resolve elaborations in.
        -> IO (Either (Error a n) (Module a n))

resolveModule profile ntsTop mm
 = runExceptT
 $ resolveModuleM profile ntsTop mm


-- | Resolve elaborations in a module.
resolveModuleM
        :: (Ord n, Pretty n, Show n)
        => Profile n                      -- ^ Language profile.
        -> [(n, ImportValue n (Type n))]  -- ^ Top-level context from imported modules.
        -> Module a n                     -- ^ Module to resolve elaborations in.
        -> S a n (Module a n)

resolveModuleM profile ntsTop mm
 = do
        -- Build the initial context,
        --   which also gathers up the set of top-level declarations
        --   available in other modules.
        ctx     <- makeContextOfModule profile ntsTop mm

        -- Decend into the expression.
        xBody'  <- resolveExp ctx (moduleBody mm)

        -- Read back the list of bindings that we've used from other modules.
        --   The module that we've processed may not have import declarations
        --   for all of these bindings, so we add and de-duplicate them here.
        topUsed <- liftIO $ readIORef (contextTopUsed ctx)
        let importValues'
                = Map.toList
                $ Map.union (Map.fromList (moduleImportValues mm))
                            (Map.fromList topUsed)

        -- Return the resolved module.
        return  $ mm
                { moduleBody            = xBody'
                , moduleImportValues    = importValues' }


-- | Resolve elaborations in an expression.
resolveExp
        :: (Ord n, Pretty n, Show n)
        => Context n
        -> Exp a n -> S a n (Exp a n)

resolveExp !ctx xx
 = case xx of
        -- Try to resolve an elaboration.
        XApp a (XPrim _ PElaborate) (RType tWant)
         -> contextResolve a ctx tWant

        -- Boilerplate traversal.
        XPrim{}         -> return xx
        XCon{}          -> return xx
        XVar{}          -> return xx

        XAbs  a p x
         -> XAbs  a p
                <$> resolveExp (contextPushParam p ctx) x

        XApp  a x1 a2
         -> XApp  a
                <$> resolveExp ctx x1
                <*> resolveArg ctx a2

        XLet  a lts x
         -> XLet  a
                <$> resolveLts ctx lts
                <*> resolveExp (contextPushLets lts ctx) x

        XCase a x alts
         -> XCase a
                <$> resolveExp ctx x
                <*> mapM (resolveAlt ctx) alts

        XCast a c x
         -> XCast a c
                <$> resolveExp ctx x


-- | Resolve elaborations in an argument.
resolveArg
        :: (Ord n, Pretty n, Show n)
        => Context n
        -> Arg a n -> S a n (Arg a n)

resolveArg !ctx arg
 = case arg of
        RType{}         -> return arg
        RTerm x         -> RTerm     <$> resolveExp ctx x
        RWitness{}      -> return arg
        RImplicit arg'  -> RImplicit <$> resolveArg ctx arg'


-- | Resolve elaborations in some let bindings.
resolveLts
        :: (Ord n, Pretty n, Show n)
        => Context n
        -> Lets a n -> S a n (Lets a n)

resolveLts !ctx lts
 = case lts of
        LLet b x
         -> LLet b <$> resolveExp ctx x

        LRec bxs
         -> do  let (bs, xs)    = unzip bxs
                let ctx'        = contextPushBinds bs ctx
                xs'             <- mapM (resolveExp ctx') xs
                let bxs'        = zip bs xs'
                return $ LRec bxs'

        LPrivate{}      -> return lts


-- | Resolve elaborations in an alternative.
resolveAlt
        :: (Ord n, Pretty n, Show n)
        => Context n
        -> Alt a n -> S a n (Alt a n)

resolveAlt !ctx alt
 = case alt of
        AAlt w x        -> AAlt w <$> resolveExp (contextPushPat w ctx) x

