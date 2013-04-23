
module DDC.Core.Flow.Transform.Prep
        (prepModule)
where
import DDC.Core.Flow.Prim
import DDC.Core.Compounds
import DDC.Core.Module
import DDC.Core.Exp
import Control.Monad.State.Strict
import Data.Map                 (Map)
import qualified Data.Map       as Map

-- | Prepare a module for lowering.
--   We need all worker functions passed to flow operators to be eta-expanded
--   and for their parameters to have real names.
--
prepModule 
        ::  Module () Name 
        -> (Module () Name, Map Name [Type Name])

prepModule mm
 = do   runState (prepModuleM mm) Map.empty


prepModuleM :: Module () Name -> PrepM (Module () Name)
prepModuleM mm
 = do   xBody'  <- prepX $ moduleBody mm
        return  $  mm { moduleBody = xBody' }


-- Do a bottom-up rewrite,
--  on the way up remember names of variables that are passed as workers 
--  to flow operators, then eta-expand bindings with those names.
prepX   :: Exp () Name -> PrepM (Exp () Name)
prepX xx
 = case xx of
        -- Detect workers passed to folds.
        XApp{}
         | Just (XVar _ u, [_, XType tA, XType tB, XVar _ (UName n), _, _])
                                               <- takeXApps xx
         , UPrim (NameFlowOp FlowOpFold) _     <- u
         -> do   addWorkerArgs n [tA, tB]
                 return xx


        -- Bottom-up transform boilerplate.
        XVar{}          -> return xx
        XCon{}          -> return xx
        XLAM  a b x     -> liftM3 XLAM  (return a) (return b) (prepX x)
        XLam  a b x     -> liftM3 XLam  (return a) (return b) (prepX x)
        XApp  a x1 x2   -> liftM3 XApp  (return a) (prepX x1) (prepX x2)

        XLet  a lts x   
         -> do  x'      <- prepX x
                lts'    <- prepLts lts
                return  $  XLet a lts' x'

        XCase{}         -> error "ddc-core-flow.prepX: cases not handled yet"
        XCast a c x     -> liftM3 XCast (return a) (return c) (prepX x)
        XType{}         -> return xx
        XWitness{}      -> return xx


prepLts :: Lets () Name -> PrepM (Lets () Name)
prepLts lts
 = case lts of
        LLet m b@(BName n _) x
         -> do  x'      <- prepX x

                mArgs   <- lookupWorkerArgs n
                case mArgs of
                 Just tsArgs
                  |  length tsArgs > 0
                  -> let x_eta = xLams  ()    (map BAnon tsArgs)
                               $ xApps () x'  [ XVar () (UIx (length tsArgs - 1 - ix))
                                              | ix <- [0 ..   length tsArgs - 1] ]
                     in  return $ LLet m b x_eta

                 _ -> return $ LLet m b x'

        LLet m b x
         -> do  x'      <- prepX x
                return  $ LLet m b x'

        LRec bxs
         -> do  let (bs, xs) = unzip bxs
                xs'     <- mapM prepX xs
                return  $ LRec $ zip bs xs'

        LLetRegions{}   -> return lts
        LWithRegion{}   -> return lts


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

