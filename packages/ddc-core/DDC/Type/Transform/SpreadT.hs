        
module DDC.Type.Transform.SpreadT
        (SpreadT(..))
where
import DDC.Type.Exp
import DDC.Type.Env                     (Env)
import qualified DDC.Type.Pretty        as P
import qualified DDC.Type.Env           as Env
import qualified DDC.Type.Sum           as T


class SpreadT (c :: * -> *) where
 -- | Spread type annotations from the environment and binders
 --   into variables at the leaves.
 spreadT :: forall n. (Ord n, Show n, P.Pretty n) 
         => Env n -> c n -> c n
        

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
        UIx ix _      
         | Just t'       <- Env.lookup uu kenv
         -> UIx ix t'
         
        UName n _
         | Just t'      <- Env.lookup uu kenv
         -> if Env.isPrim kenv n 
                 then UPrim n t'                         -- TODO: recursively spread into dropped type, but do occ check.
                 else UName n t'
                 
        UPrim n _
         | Just t'      <- Env.lookup uu kenv
         -> UPrim n t'
        
        _ -> uu


instance SpreadT TyCon where
 spreadT kenv tc
  = case tc of
        TyConBound u    -> TyConBound (spreadT kenv u)
        _               -> tc

