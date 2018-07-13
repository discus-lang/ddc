
module DDC.Core.Transform.Resolve.Context
        ( Context (..)
        , makeContextOfModule
        , contextPushBinds
        , contextPushLets
        , contextPushParam
        , contextPushPat
        , contextResolve)
where
import DDC.Core.Transform.Resolve.Base
import DDC.Type.Transform.Instantiate
import DDC.Type.Transform.SubstituteT
import DDC.Type.Transform.Unify
import DDC.Core.Fragment                        (Profile (..))
import DDC.Core.Exp.Annot
import DDC.Core.Codec.Text.Pretty
import Control.Monad.IO.Class
import Data.IORef
import qualified Data.Map.Strict                as Map


-- | Context of resolve process.
--   TODO: cache resolutions and re-use them.
--         there are often many, eg (Eq Word32) in the same module and we don't
--         want to repeat the resolution process each time.
data Context n
        = Context
        { -- | Current type environment.
          contextEnvT           :: EnvT n

          -- | Available top-level context.
          --   These are supers that are available in other modules,
          --   that might not already be referenced in the one that we're resolving.
          --
          -- ISSUE #434: Store resolve dictionary by outermost tycon.
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
        -> [(n, Type n)]                  -- ^ Top-level synonyms from imported modules.
        -> Module a n                     -- ^ Module that we're doing resolution in.
        -> S a n (Context n)

makeContextOfModule !profile !ntsTop !ntsSyn !mm
 = do   let ntsImport   = [ (n, t)  | (n, ImportValueModule _ _ t _)
                                    <- moduleImportValues mm ]

        refTopUsed      <- liftIO $ newIORef []
        let envt        = moduleEnvT (profilePrimKinds profile) mm

        return $ Context
         { contextEnvT
                = envt { envtEquations  = Map.union (envtEquations envt)
                                        $ Map.fromList ntsSyn }

         , contextTop            = ntsTop
         , contextTopUsed        = refTopUsed
         , contextBinds          = [ntsImport] }


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


-- | Try to build a term of the given type,
--   out of the terms available in the context.
contextResolve
        :: (Ord n, Pretty n, Show n)
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
         = do   result <- searchGroup g
                case result of
                 Nothing -> searchStack gs
                 Just x  -> return x


        searchGroup []
         = return Nothing

        searchGroup ((nBind, tBind) : nts)
         = do   let match = matchBind a (contextEnvT ctx) tWant nBind tBind
                case match of
                 Nothing
                  -> searchGroup nts

                 Just (xx', tsArg)
                  -> do
                        xsArg   <- mapM (contextResolve a ctx) tsArg
                        return  $ Just
                                $ xApps a xx'
                                $ map (RImplicit . RTerm) xsArg


        -- Search the top-level imported things.
        searchImports []
         = throwE $ ErrorCannotResolve tWant

        searchImports ((nBind, i) : nisMore)
         = do
                let tBind
                        = case i of
                           ImportValueModule{} -> importValueModuleType i
                           ImportValueSea{}    -> importValueSeaType    i

                let match = matchBind a (contextEnvT ctx) tWant nBind tBind
                case match of
                 Nothing
                  -> searchImports nisMore

                 Just (xx', tsArg)
                  -> do

                        xsArg   <- mapM (contextResolve a ctx) tsArg

                        liftIO $ modifyIORef' (contextTopUsed ctx)
                         $ \used -> (nBind, i) : used

                        return  $ xApps a xx'
                                $ map (RImplicit . RTerm) xsArg


matchBind
        :: Ord n
        => a
        -> EnvT n               -- ^ Type environment.
        -> Type n               -- ^ Wanted type.
        -> n                    -- ^ Name of binding in environment.
        -> Type n               -- ^ Type of binding.
        -> Maybe (Exp a n, [Type n])

matchBind a envt tWant' nBind  tBind'
 = do
        let match = matchScheme envt tWant' tBind'
        case match of
         Nothing
          ->    Nothing

         Just (tsInst, tsArg)
          ->    Just    ( xApps a (XVar a (UName nBind)) $ map RType tsInst
                        , tsArg)


-- | Match a wanted type against an available scheme.
matchScheme
        :: Ord n
        => EnvT n               -- ^ Type environment.
        -> Type n               -- ^ Wanted type.
        -> Type n               -- ^ Available scheme.
        -> Maybe ( [Type n]     --   Type  arguments to instantiate scheme.
                 , [Type n])    --   Types of more implicit term arguments.

matchScheme envt tWanted tBind
 -- The type of this binding is exactly what we want.
 | equivT envt tWanted tBind
 = Just ([], [])

 -- Check if the binding
 | otherwise
 = let
        -- Split off any type parameters.
        (bsParamType, _tBody)
         = case takeTForalls tBind of
                Just (bs, t)    -> (bs, t)
                Nothing         -> ([], tBind)

        -- Instantiate the type with new existentials.
        nArgs   = length bsParamType
        tsArgExists
                = [ TCon (TyConExists i k)
                  | i <- [0..]
                  | k <- map typeOfBind bsParamType]

        Just tBind_inst
                = instantiateTs tBind tsArgExists

        -- Split of any implicit value parameters.
        (tsParamTerm, tResult)
                = takeTFunImplicits tBind_inst

        result  = unifyExistsRight envt tWanted tResult

        -- Try to unify the wanted type with the result we
        -- would get if we applied the function.
   in   case result of
         Just cs
          |  Just tsArgInst
                <- sequence
                $  map (\i -> Prelude.lookup i cs)
                $ [0.. nArgs - 1]

          -> let tsParamTerm' = map (substituteExistsT cs) tsParamTerm
             in  Just (tsArgInst, tsParamTerm')

         _ -> Nothing


