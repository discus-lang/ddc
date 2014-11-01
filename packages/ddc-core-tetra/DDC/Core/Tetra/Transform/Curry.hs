
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
import Data.Map                                 (Map)
import qualified DDC.Type.Env                   as Env
import qualified Data.Map                       as Map

-- TODO: handle supers names being shadowed by local bindings.
-- TODO: need to rewrite types on lambda bindings as we decend into the tree.
-- \(f : a -> b)  becomes  \(f : C# (a -> b))



-- | Insert primitives to manage higher order functions in a module.
curryModule 
        :: Show a
        => Module (AnTEC a Name) Name -> Module (AnTEC a Name) Name
curryModule mm
 = let
        -- Apply curry transform in the body of the module.
        xBody'          = curryBody Map.empty Env.empty Env.empty 
                        $ moduleBody mm


   in   mm { moduleBody = xBody' }

---------------------------------------------------------------------------------------------------
type SuperMap
        = Map Name (Type Name)

superMapAddBind :: SuperMap -> Bind Name -> SuperMap
superMapAddBind sm b
 = case b of
        BName n t -> Map.insert n t sm
        _         -> sm

---------------------------------------------------------------------------------------------------
-- | Manage higher-order functions in a module body.
curryBody
        :: Show a
        => SuperMap
        -> KindEnv Name                 -- ^ Kind environment.
        -> TypeEnv Name                 -- ^ Type environment.
        -> Exp (AnTEC a Name) Name
        -> Exp (AnTEC a Name) Name

curryBody sm kenv tenv xx
 = case xx of
        XLet a (LRec bxs) x2
         -> let (bs, xs)        = unzip bxs

                -- Rewrite types of binders to use closures,
                -- and add them to the super map.
                bs'             = map currySuperBind bs
                sm'             = foldl superMapAddBind sm bs'

                -- The new type environment.
                tenv'           = Env.extends bs' tenv

                -- Rewrite bindings
                xs'             = map (curryX sm' kenv tenv) xs
                bxs'            = zip bs' xs'

            in  XLet a (LRec bxs') 
                 $ curryBody sm' kenv tenv' x2

        _ -> xx

---------------------------------------------------------------------------------------------------
curryX, curryX1 
        :: Show a
        => Map Name (Type Name)
        -> KindEnv Name 
        -> TypeEnv Name
        -> Exp (AnTEC a Name) Name 
        -> Exp (AnTEC a Name) Name

curryX sm kenv tenv xx
 = transformDownX (curryX1 sm) kenv tenv xx
                                
-- | Mangage higher-order functions in an expression.
curryX1 topTypes _kenv _tenv xx 

 -- Rewrite applications.
 | Just (xF, xsArgs)    <- takeXApps xx
 , XVar a (UName nF)    <- xF
 , length xsArgs  > 0
 = makeCall a topTypes nF xsArgs

 -- Rewrite types of super arguments.
 | XLam a b x           <- xx
 = XLam a (curryArgBind b) x

 | otherwise
 = xx


---------------------------------------------------------------------------------------------------
-- | When a local binder has functional type then we rewrite it to have
--   closure type. eg  \f : a -> b. e  goes to  \f : C# (a -> b). e
curryArgBind :: Bind Name -> Bind Name
curryArgBind b
 = case b of
        BName u t       -> BName u (rewriteArgT t)
        BAnon t         -> BAnon   (rewriteArgT t)
        BNone t         -> BNone   (rewriteArgT t)


currySuperBind :: Bind Name -> Bind Name
currySuperBind b
 = case b of
        BName u t       -> BName u (rewriteSuperT t)
        BAnon t         -> BAnon (rewriteSuperT t)
        BNone t         -> BNone (rewriteSuperT t)


rewriteSuperT :: Type Name -> Type Name
rewriteSuperT t
        | TForall b t'         <- t
        = TForall b (rewriteSuperT t')
        
        | (tsArgs, tResult)    <- takeTFunArgResult t
        , Just t'              <- tFunOfList ( (map rewriteArgT tsArgs) ++ [tResult])
        = t'

        | otherwise = t


rewriteArgT :: Type Name -> Type Name
rewriteArgT t
        | Just _       <- takeTFun t
        = tCloValue t

        | otherwise
        = t


---------------------------------------------------------------------------------------------------
-- | Call a thing, depending on what it is.
makeCall
        :: Show a
        => AnTEC a Name                 -- ^ Annotation from functional part of application.
        -> Map Name (Type Name)         -- ^ Map names of supers to their types.
        -> Name                         -- ^ Name of function to call.
        -> [Exp (AnTEC a Name) Name]    -- ^ Arguments to function.
        ->  Exp (AnTEC a Name) Name

makeCall aF topTypes nF xsArgs
 | Just _       <- Map.lookup nF topTypes
 = let  iArgs   = length xsArgs
        iArity  = iArgs
   in   makeCallSuper aF nF iArity xsArgs

 | otherwise
 = makeCallThunk aF nF xsArgs


---------------------------------------------------------------------------------------------------
-- | Call a top-level supercombinator.
makeCallSuper 
        :: AnTEC a Name                 -- ^ Annotation to use.
        -> Name                         -- ^ Name of super to call.
        -> Int                          -- ^ Arity of super.
        -> [Exp (AnTEC a Name) Name]    -- ^ Arguments to super.
        -> Exp  (AnTEC a Name) Name

makeCallSuper a nF _iArity xsArgs
 = xApps a (XVar a (UName nF)) xsArgs


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
   in   xFunCEval aF tsArgs tResult (XVar aF (UName nF)) xsArgs

-- error $ "call thunk " ++ show (nF, xsArgs, tsArgs, tResult)

--  = xApps a (XVar a (UName nF)) xsArgs
-- = let  tsArgs  = map typeOfExp xsArgs

