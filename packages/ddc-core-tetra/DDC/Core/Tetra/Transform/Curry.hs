
module DDC.Core.Tetra.Transform.Curry
        (curryModule)
where
import DDC.Type.Env                             (KindEnv, TypeEnv)
import DDC.Core.Transform.TransformDownX
import DDC.Core.Annot.AnTEC
import DDC.Core.Tetra
import DDC.Core.Tetra.Compounds
import DDC.Core.Module
import DDC.Core.Exp
import Data.Maybe
import Data.List                                (foldl')
import Data.Map                                 (Map)
import qualified DDC.Type.Env                   as Env
import qualified Data.Map                       as Map
import Debug.Trace

-- TODO: handle supers names being shadowed by local bindings.
-- TODO: ensure type lambdas are out the front of supers, supers in prenex form.
-- TODO: build thunks for partially applied foreign functions.

-- | Insert primitives to manage higher order functions in a module.
curryModule 
        :: Show a
        => Module (AnTEC a Name) Name -> Module (AnTEC a Name) Name

curryModule mm
 = let  
        -- Add all the foreign functions to the function map.
        -- We can do a saturated call for these directly.
        funs_foreign
                = foldl' funMapAddForeign Map.empty
                $ moduleImportValues mm

        -- Apply curry transform in the body of the module.
        xBody'  = curryBody (funs_foreign, Env.empty, Env.empty)
                $ moduleBody mm

   in   mm { moduleBody = xBody' }


---------------------------------------------------------------------------------------------------
-- | Map of functional values to their types and arities.
type FunMap
        = Map Name Fun

-- | Enough information about a functional thing to decide how we 
--   should call it. 
data Fun
        -- | A locally defined top-level supercombinator. 
        --   We can do a saturated call for these directly.
        --   The arity of the super can be determined by inspecting the
        --   definition in the current module.
        = FunLocalSuper
        { _funName      :: Name
        , _funType      :: Type Name 
        , _funArity     :: Int }

        | FunExternSuper
        { _funName      :: Name
        , _funType      :: Type Name
        , _funArity     :: Int }

        -- | A foreign imported function.
        --   We can do a saturated call for these directly.
        --   Foreign functions are not represented as closures, 
        --   so we can determine their arity directly from their types.
        | FunForeignSea
        { _funName      :: Name
        , _funType      :: Type Name
        , _funArity     :: Int }
        deriving Show


-- | Add the type of this binding to the function map.
funMapAddLocalSuper :: FunMap -> (Bind Name, Exp a Name) -> FunMap
funMapAddLocalSuper funs (b, x)
        | BName n t             <- b
        = let   -- Get the value arity of the super, that is, how many
                -- values we need to saturate all the value lambdas.
                (flags, _) = fromMaybe ([], x) (takeXLamFlags x)
                arity      = length $ filter (== False) $ map fst flags

          in    Map.insert n (FunLocalSuper n t arity) funs

        | otherwise
        = funs


-- | Add the type of a foreign import to the function map.
funMapAddForeign :: FunMap -> (Name, ImportSource Name) -> FunMap
funMapAddForeign funs (n, is)
        | ImportSourceSea _ t  <- is
        = Map.insert n (FunForeignSea n t 0) funs

        | otherwise
        = funs


---------------------------------------------------------------------------------------------------
type Context
        = (FunMap, KindEnv Name, TypeEnv Name)

-- | Manage higher-order functions in a module body.
curryBody
        :: Show a
        => Context
        -> Exp (AnTEC a Name) Name
        -> Exp (AnTEC a Name) Name

curryBody (funs, kenv, tenv) xx
 = case xx of
        XLet a (LRec bxs) x2
         -> let (bs, xs) = unzip bxs

                -- Add types of supers to the function map.
                funs'   = foldl funMapAddLocalSuper funs bxs

                -- The new type environment.
                tenv'   = Env.extends bs tenv

                -- Rewrite bindings in the body of the let-expression.
                ctx'    = (funs', kenv, tenv')
                xs'     = map (curryX ctx') xs
                bxs'    = zip bs xs'

            in  XLet a (LRec bxs') 
                 $ curryBody ctx' x2

        _ -> xx


---------------------------------------------------------------------------------------------------
curryX  :: Show a
        => Context
        -> Exp (AnTEC a Name) Name 
        -> Exp (AnTEC a Name) Name

curryX (funs, kenv, tenv) xx
 = transformDownX (curryX1 funs) kenv tenv xx
                                
-- | Mangage higher-order functions in an expression.
curryX1 topTypes _kenv _tenv xx 

 -- Rewrite applications.
 | Just (xF, xsArgs)    <- takeXApps xx         -- TODO: split type args and combine into xF
 , XVar a (UName nF)    <- xF
 , length xsArgs  > 0
 = makeCall a topTypes nF xsArgs

 | otherwise
 = xx


---------------------------------------------------------------------------------------------------
-- | Call a thing, depending on what it is.
makeCall
        :: Show a
        => AnTEC a Name         -- ^ Annotation from functional part of application.
        -> FunMap               -- ^ Types and arities of functions in the environment.
        -> Name                 -- ^ Name of function to call.
        -> [Exp (AnTEC a Name) Name]    -- ^ Arguments to function.
        ->  Exp (AnTEC a Name) Name

makeCall aF funMap nF xsArgs
 -- | Saturated call a top-level super in the local module.
 | Just (FunLocalSuper _ _ iArity) <- Map.lookup nF funMap
 = makeCallSuper aF nF iArity xsArgs

 -- | Saturated call of a foreign imported function.
 | Just (FunForeignSea _ _ iArity) <- Map.lookup nF funMap
 = makeCallSuper aF nF iArity xsArgs

 | otherwise
 = makeCallThunk aF nF xsArgs


---------------------------------------------------------------------------------------------------
-- | Call a top-level supercombinator,
--   or foriegn function imported from Sea land.
makeCallSuper 
        :: Show a 
        => AnTEC a Name                 -- ^ Annotation to use.
        -> Name                         -- ^ Name of super to call.
        -> Int                          -- ^ Arity of super.
        -> [Exp (AnTEC a Name) Name]    -- ^ Arguments to super.
        -> Exp  (AnTEC a Name) Name

makeCallSuper a nF iArity xsArgs
 | iArity == length xsArgs
 = xApps a (XVar a (UName nF)) xsArgs

 | otherwise
 = trace ("pap " ++ show (nF, iArity, length xsArgs))
 $ xApps a (XVar a (UName nF)) xsArgs


---------------------------------------------------------------------------------------------------
-- | Call a thunk.
makeCallThunk
        :: Show a
        => AnTEC a Name                 -- ^ Annotation from functional part of application.
        -> Name                         -- ^ Name of thunk.
        -> [Exp (AnTEC a Name) Name]    -- ^ Arguments to thunk.
        ->  Exp (AnTEC a Name) Name

makeCallThunk aF nF xsArgs
 = let  tsArgs          = map annotType $ map annotOfExp xsArgs
        (_, tResult)    = takeTFunArgResult $ annotType aF
   in   xFunApply aF tsArgs tResult (XVar aF (UName nF)) xsArgs


