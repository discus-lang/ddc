
module DDC.Core.Tetra.Transform.Curry
        (curryModule)
where
import DDC.Core.Tetra.Transform.Curry.Call
import DDC.Core.Tetra.Transform.Curry.Interface
import DDC.Core.Transform.Reannotate

import DDC.Type.Env                             (KindEnv, TypeEnv)
import DDC.Core.Annot.AnTEC
import DDC.Core.Tetra
import DDC.Core.Module
import DDC.Core.Exp
import Data.List                                (foldl')
import qualified DDC.Core.Call                  as Call
import qualified DDC.Type.Env                   as Env
import qualified Data.Map                       as Map


-- TODO: handle supers names being shadowed by local bindings.
--
-- TODO: ensure type lambdas are out the front of supers, supers in prenex form.
--
-- TODO: handle monomorphic functions being passed to contructors, etc.
--       not an app but we need to build a closure.
--
-- TODO: also handle under/over applied data constructors, do a transform
--       beforehand to saturate them.

---------------------------------------------------------------------------------------------------
-- | Insert primitives to manage higher order functions in a module.
curryModule 
        :: Module (AnTEC a Name) Name 
        -> Module () Name
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
        :: Context 
        -> Exp (AnTEC a Name) Name 
        -> Exp () Name

curryBody (funs, kenv, tenv) xx
 = case xx of
        XLet _ (LRec bxs) x2
         -> let (bs, xs) = unzip bxs

                -- Add types of supers to the function map.
                funs'   = foldl funMapAddLocalSuper funs bxs

                -- The new type environment.
                tenv'   = Env.extends bs tenv

                -- Rewrite bindings in the body of the let-expression.
                ctx'    = (funs', kenv, tenv')
                xs'     = map (curryX ctx') xs
                bxs'    = zip bs xs'

            in  XLet () (LRec bxs') 
                 $ curryBody ctx' x2

        _ -> reannotate (const ()) xx


---------------------------------------------------------------------------------------------------
-- | Manage function application in an expression.
curryX  :: forall a
        .  Context -> Exp (AnTEC a Name) Name -> Exp () Name
curryX ctx@(funs, _kenv, _tenv) xx
 = case xx of
        XVar  a (UName nF)
         -> case makeCall funs nF (annotType a) [] of
                Just xx' -> xx'
                Nothing  -> XVar () (UName nF)

        XVar  _ u
         -> XVar () u

        XApp  _ x1 x2
         -> case curryX_call xx of
                Just xx' -> xx'
                Nothing  -> XApp () (down x1) (down x2)

        XCast _ CastRun x1
         -> case curryX_call xx of
                Just xx' -> xx'
                Nothing  -> XCast () CastRun (down x1)

        -- Boilerplate.
        XCon     _ c     -> XCon     () c
        XLam     _ b x   -> XLam     () b (down x)
        XLAM     _ b x   -> XLAM     () b (down x)
        XLet     _ lts x -> XLet     () (curryLts ctx lts) (down x)
        XCase    _ x as  -> XCase    () (down x) (map (curryAlt ctx) as)
        XCast    _ c x   -> XCast    () (reannotate (const ()) c) (down x)
        XType    _ t     -> XType    () t
        XWitness _ w     -> XWitness () (reannotate (const ()) w)

 where          

        curryX_call :: Exp (AnTEC a Name) Name -> Maybe (Exp () Name)
        curryX_call x
         -- If this is a call of a named function then split it into the
         --  functional part and arguments, then work out how to call it.
         | (xF, esArgs)         <- Call.takeCallElim x
         , XVar aF (UName nF)   <- xF
         , length esArgs  > 0
         = let esArgs' = map downElim esArgs
           in  makeCall funs nF (annotType aF) esArgs'

         | otherwise
         = Nothing

        down x
         = curryX ctx x

        downElim ee
         = case ee of
                Call.ElimType  _ _ t -> Call.ElimType  () () t
                Call.ElimValue _ x   -> Call.ElimValue () (down x)
                Call.ElimRun   _     -> Call.ElimRun   ()


-- | Manage function application in a let binding.
curryLts :: Context -> Lets (AnTEC a Name) Name -> Lets () Name
curryLts ctx lts
 = case lts of
        LLet b x          -> LLet b (curryX ctx x)
        LRec bxs          -> LRec [(b, curryX ctx x) | (b, x) <- bxs]
        LPrivate bs mt ws -> LPrivate bs mt ws


-- | Manage function application in a case alternative.
curryAlt :: Context -> Alt (AnTEC a Name) Name -> Alt () Name
curryAlt ctx alt
 = case alt of
        AAlt w x        -> AAlt w (curryX ctx x)

