
module DDC.Type.Transform.Spread
        (Spread(..))
where
import DDC.Type.Compounds
import DDC.Type.Exp
import DDC.Type.Check.Env               (Env)
import qualified DDC.Type.Check.Env     as Env
import qualified DDC.Type.Sum           as T


-- | Spread type annotations from the environment and binders,
--   into variables at the leaves.
class Spread (c :: * -> *) where
        spread :: forall n. Ord n => Env n -> c n -> c n
        

instance Spread Type where
 spread env tt
  = case tt of
        TVar u          -> TVar $ spread env u
        TCon tc         -> TCon $ spread env tc
        TForall u t     -> TForall u $ spread (Env.extend u env) t
        TApp t1 t2      -> TApp (spread env t1) (spread env t2)
        TSum ss         -> TSum (spread env ss)
        TBot k          -> TBot (spread env k)
        

instance Spread TypeSum where
 spread env ss
        = T.fromList (spread env $ T.kindOfSum ss)
        $ map (spread env)
        $ T.toList ss


instance Spread Bound where
 spread env uu
  = case Env.lookup uu env of
          Just t        -> replaceTypeOfBound t uu
          Nothing       -> uu


instance Spread TyCon where
 spread env tc
  = case tc of
        TyConComp cc    -> TyConComp (spread env cc)
        _               -> tc


instance Spread TcCon where
 spread env cc
  = case cc of
        TcConData n k
         -> case Env.lookupName n env of
                 Just k' -> TcConData n k'
                 Nothing -> TcConData n k
                 
        _               -> cc