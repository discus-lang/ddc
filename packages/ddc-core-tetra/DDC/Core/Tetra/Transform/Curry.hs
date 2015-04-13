
module DDC.Core.Tetra.Transform.Curry
        (curryModule)
where
import DDC.Core.Tetra.Transform.Curry.Call
import DDC.Core.Tetra.Transform.Curry.Interface

import DDC.Type.Env                             (KindEnv, TypeEnv)
import DDC.Core.Annot.AnTEC
import DDC.Core.Tetra
import DDC.Core.Module
import DDC.Core.Exp
import Data.List                                (foldl')
import qualified DDC.Core.Call                  as Call
import qualified DDC.Type.Env                   as Env
import qualified Data.Map                       as Map


---------------------------------------------------------------------------------------------------
-- TODO: handle supers names being shadowed by local bindings.
-- TODO: ensure type lambdas are out the front of supers, supers in prenex form.
-- TODO: build thunks for partially applied foreign functions.
-- TODO: handle monomorphic functions being passed to contructors, etc.
--       not an app but we need to build a closure.
-- TOOD: also handle under/over applied data constructors, do a transform
--       beforehand to saturate them.


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

curryX ctx@(funs, _kenv, _tenv) xx
 = case xx of
        XVar a u
         -> case u of 
                UName nF -> makeCall xx a funs nF []
                _        -> xx

        XApp  a x1 x2
         -> case curryX_call xx of
                Just xx' -> xx'
                Nothing  -> XApp a (down x1) (down x2)

        XCast a CastRun x1
         -> case curryX_call xx of
                Just xx' -> xx'
                Nothing  -> XCast a CastRun x1

        -- Boilerplate.
        XCon{}          -> xx
        XLam  a b x     -> XLam  a b (down x)
        XLAM  a b x     -> XLAM  a b (down x)
        XLet  a lts x   -> XLet  a   (curryLts ctx lts) (down x)
        XCase a x alts  -> XCase a   (down x) (map (curryAlt ctx) alts)
        XCast a c x     -> XCast a c (down x)
        XType{}         -> xx
        XWitness{}      -> xx

 where          

        curryX_call x
         -- If this is a call of a named function then split it 
         -- into the functional part and arguments, then work out
         -- how to call it
         | (xF, esArgs)         <- Call.takeCallElim x
         , XVar aF (UName nF)   <- xF
         , length esArgs  > 0
         = let esArgs' = map downElim esArgs
           in  Just $ makeCall x aF funs nF esArgs'

         | otherwise
         = Nothing

        down x
         = curryX ctx x

        downElim ee
         = case ee of
                Call.ElimType{}         -> ee
                Call.ElimValue a x      -> Call.ElimValue a (down x)
                Call.ElimRun{}          -> ee


-- | Manage function application in a let binding.
curryLts :: Show a
        => Context
        -> Lets (AnTEC a Name) Name -> Lets (AnTEC a Name) Name

curryLts ctx lts
 = case lts of
        LLet b x        -> LLet b (curryX ctx x)
        LRec bxs        -> LRec [(b, curryX ctx x) | (b, x) <- bxs]
        LPrivate{}      -> lts
        LWithRegion{}   -> lts


-- | Manage function application in a case alternative.
curryAlt :: Show a
        => Context
        -> Alt (AnTEC a Name) Name  -> Alt (AnTEC a Name) Name

curryAlt ctx alt
 = case alt of
        AAlt w x        -> AAlt w (curryX ctx x)

