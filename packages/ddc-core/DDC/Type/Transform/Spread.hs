        
module DDC.Type.Transform.Spread
        (Spread(..))
where
import DDC.Type.Exp
import DDC.Type.Env                     (Env)
import qualified DDC.Type.Pretty        as P
import qualified DDC.Type.Env           as Env
import qualified DDC.Type.Sum           as T


class Spread (c :: * -> *) where
 -- | Spread type annotations from the environment and binders into variables at the leaves.
 spread :: forall n. (Ord n, Show n, P.Pretty n) => Env n -> c n -> c n
        

instance Spread Type where
 spread env tt
  = case tt of
        TVar u          -> TVar $ spread env u
        TCon tc         -> TCon $ spread env tc

        TForall b t
         -> let b'      = spread env b
            in  TForall b' $ spread (Env.extend b' env) t

        TApp t1 t2      -> TApp (spread env t1) (spread env t2)
        TSum ss         -> TSum (spread env ss)
        

instance Spread TypeSum where
 spread env ss
        = T.fromList (spread env $ T.kindOfSum ss)
        $ map (spread env)
        $ T.toList ss


instance Spread Bind where
 spread env bb
  = case bb of
        BName n t       -> BName n (spread env t)
        BAnon t         -> BAnon (spread env t)
        BNone t         -> BNone (spread env t)


instance Spread Bound where
 spread env uu
  = case uu of
        UIx ix _      
         | Just t'       <- Env.lookup uu env
         -> UIx ix t'
         
        UName n _
         | Just t'      <- Env.lookup uu env
         -> if Env.isPrim env n 
                 then UPrim n t'                         -- TODO: recursively spread into dropped type, but do occ check.
                 else UName n t'
                 
        UPrim n _
         | Just t'      <- Env.lookup uu env
         -> UPrim n t'
        
        _ -> uu


instance Spread TyCon where
 spread env tc
  = case tc of
        TyConBound u    -> TyConBound (spread env u)
        _               -> tc

