        
module DDC.Type.Transform.SpreadT
        (SpreadT(..))
where
import DDC.Type.Exp
import DDC.Type.Env                     (TypeEnv)
import qualified DDC.Type.Env           as Env
import qualified DDC.Type.Sum           as T


class SpreadT (c :: * -> *) where

 -- | Rewrite `UName` bounds to `UPrim` bounds and attach their types.
 --   Primitives have their types attached because they are so common in the
 --   language, their types are closed, and we don't want to keep having to
 --   look them up from the environment.
 spreadT :: forall n. Ord n 
         => TypeEnv n -> c n -> c n
        

instance SpreadT Type where
 spreadT kenv tt
  = case tt of
        TVar u          -> TVar $ spreadT kenv u
        TCon tc         -> TCon $ spreadT kenv tc

        TForall b t
         -> let b'      = spreadT kenv b
            in  TForall b' $ spreadT (Env.extend b' kenv) t

        TApp t1 t2      -> TApp (spreadT kenv t1) (spreadT kenv t2)
        TSum ss         -> TSum (spreadT kenv ss)
        

instance SpreadT TypeSum where
 spreadT kenv ss
        = T.fromList (spreadT kenv $ T.kindOfSum ss)
        $ map (spreadT kenv)
        $ T.toList ss


instance SpreadT Bind where
 spreadT kenv bb
  = case bb of
        BName n t       -> BName n (spreadT kenv t)
        BAnon t         -> BAnon (spreadT kenv t)
        BNone t         -> BNone (spreadT kenv t)


instance SpreadT Bound where
 spreadT kenv uu
  = case uu of
        UIx{}           -> uu
        UPrim{}         -> uu

        UName n
         -> case Env.envPrimFun kenv n of
                Nothing -> UName n
                Just t  -> UPrim n t
                 

instance SpreadT TyCon where
 spreadT kenv tc
  = case tc of
        TyConBound (UName n) _
         -> case Env.envPrimFun kenv n of
                Nothing -> tc
                Just t  -> TyConBound (UPrim n t) t

        _               -> tc

