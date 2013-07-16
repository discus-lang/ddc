
module DDC.Core.Flow.Transform.Prep
        (prepModule)
where
import DDC.Core.Flow.Prim
import DDC.Core.Flow.Prim.TyConPrim
import DDC.Core.Compounds
import DDC.Core.Module
import DDC.Core.Exp
import Control.Monad.State.Strict
import Data.Map                 (Map)
import qualified Data.Map       as Map
import DDC.Type.Env             (TypeEnv)
import qualified DDC.Type.Env   as Env

-- | Prepare a module for lowering.
--   We need all worker functions passed to flow operators to be eta-expanded
--   and for their parameters to have real names.
prepModule 
        ::  Module a Name 
        -> (Module a Name, Map Name [Type Name])

prepModule mm
 = do   runState (prepModuleM mm) Map.empty


prepModuleM :: Module a Name -> PrepM (Module a Name)
prepModuleM mm
 = do   xBody'  <- prepX Env.empty $ moduleBody mm
        return  $  mm { moduleBody = xBody' }


-- Do a bottom-up rewrite,
--  on the way up remember names of variables that are passed as workers 
--  to flow operators, then eta-expand bindings with those names.
-- Record the environment of let-bound expressions, to know whether to 
--  eta-expand in their definition or at the callsite.
prepX   :: TypeEnv Name -> Exp a Name -> PrepM (Exp a Name)
prepX tenv xx
 = case xx of
        -- Detect workers passed to maps.
        -- Check if the worker is bound in a let: if so, defer till later
        XApp{}
         | Just (XVar _ u, [_,  XType tA, XType _tB, XVar _ (UName n), _])
                                                <- takeXApps xx
         , UPrim (NameOpFlow (OpFlowMap 1)) _   <- u
         , Env.member (UName n) tenv
         -> do  addWorkerArgs n [tA]
                return xx
        -- Map2
        XApp{}
         | Just (XVar _ u, [_,  XType tA, XType tB, XType _tC, XVar _ (UName n), _])
                                                <- takeXApps xx
         , UPrim (NameOpFlow (OpFlowMap 2)) _   <- u
         , Env.member (UName n) tenv
         -> do  addWorkerArgs n [tA, tB]
                return xx

        -- Worker passed to map, but not let-bound.
        -- Eta-expand in-place.
        XApp{}
         | Just (xmap@(XVar _ u), args@[_,  XType tA, XType _tB, f@(XVar a _), _])
                                                <- takeXApps xx
         , UPrim (NameOpFlow (OpFlowMap 1)) _   <- u
         -> do  let f'    = xEtaExpand a f [tA]
                    args' = take 3 args ++ [f'] ++ [last args]
                return $ xApps a xmap args'

        -- Detect workers passed to folds.
        XApp{}
         | Just (XVar _ u, [_, XType tA, XType tB, XVar _ (UName n), _, _])
                                               <- takeXApps xx
         , UPrim (NameOpFlow OpFlowFold) _     <- u
         -> do   addWorkerArgs n [tA, tB]
                 return xx

        -- FoldIndex
        XApp{}
         | Just (XVar _ u, [_, XType tA, XType tB, XVar _ (UName n), _, _])
                                                <- takeXApps xx
         , UPrim (NameOpFlow OpFlowFoldIndex) _ <- u
         -> do   addWorkerArgs n [tInt, tA, tB]
                 return xx


        -- Detect workers passed to mkSels
        XApp{}
         | Just (XVar _ u, [XType _tK1, XType _tA, _, XVar _ (UName n)])
                                                <- takeXApps xx
         , UPrim (NameOpFlow (OpFlowMkSel _)) _ <- u
         -> do  addWorkerArgs n []
                return xx

        -- Bottom-up transform boilerplate.
        XVar{}          -> return xx
        XCon{}          -> return xx
        XLAM  a b x     -> liftM3 XLAM  (return a) (return b) (go x)
        XLam  a b x     -> liftM3 XLam  (return a) (return b) (go x)
        XApp  a x1 x2   -> liftM3 XApp  (return a) (go x1)    (go x2)

        XLet  a lts x   
         -> do  -- Slurp binds from lets, add to tenv
                let tenv' = Env.extends (valwitBindsOfLets lts) tenv
                x'      <- prepX tenv' x
                -- Use old tenv for the binders
                lts'    <- prepLts tenv a lts
                return  $  XLet a lts' x'

        XCase a x alts  -> liftM3 XCase (return a) (go x)     (mapM (prepAlt tenv) alts)
        XCast a c x     -> liftM3 XCast (return a) (return c) (go x)
        XType{}         -> return xx
        XWitness{}      -> return xx
 where
  go = prepX tenv



-- Prepare let bindings for lowering.
prepLts :: TypeEnv Name -> a -> Lets a Name -> PrepM (Lets a Name)
prepLts tenv a lts
 = case lts of
        LLet b@(BName n _) x
         -> do  x'      <- prepX tenv x

                mArgs   <- lookupWorkerArgs n
                case mArgs of
                 Just tsArgs
                  |  length tsArgs > 0
                   -> return $ LLet b $ xEtaExpand a x' tsArgs

                 _ -> return $ LLet b x'

        LLet b x
         -> do  x'      <- prepX tenv x
                return  $ LLet b x'

        LRec bxs
         -> do  let (bs, xs) = unzip bxs
                let tenv'    = Env.extends bs tenv
                xs'     <- mapM (prepX tenv') xs
                return  $ LRec $ zip bs xs'

        LLetRegions{}   -> return lts
        LWithRegion{}   -> return lts


-- Prepare case alternative for lowering.
prepAlt :: TypeEnv Name -> Alt a Name -> PrepM (Alt a Name)
prepAlt tenv (AAlt w x)
        = liftM (AAlt w) (prepX tenv x)


xEtaExpand :: a -> Exp a Name -> [Type Name] -> Exp a Name
xEtaExpand a x tys
 = xLams a    (map BAnon tys)
 $ xApps a x  [ XVar a (UIx (length tys - 1 - ix))
              | ix <- [0 ..  length tys - 1] ]


-- State ----------------------------------------------------------------------
type PrepS      = Map   Name [Type Name]
type PrepM      = State PrepS


-- | Record this name as being of a worker function.
addWorkerArgs   :: Name -> [Type Name] -> PrepM ()
addWorkerArgs name tsParam
        = modify $ Map.insert name tsParam


-- | Check whether this name corresponds to a worker function.
lookupWorkerArgs    :: Name -> PrepM (Maybe [Type Name])
lookupWorkerArgs name
 = do   names   <- get
        return  $ Map.lookup name names

