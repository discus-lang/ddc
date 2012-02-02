
-- | Collect the free variables of a type.
module DDC.Type.Collect.FreeT
        (FreeT(..))
where
import DDC.Type.Exp
import DDC.Type.Compounds
import DDC.Type.Env                     (Env)
import Data.Set                         (Set)
import qualified DDC.Type.Sum           as Sum
import qualified DDC.Type.Env           as Env
import qualified Data.Set               as Set


class FreeT n a where
 -- | Determine the set of type variables not bound in the given environment.
 freeT :: Ord n => Env n -> a -> Set (Bound n)


instance FreeT n (Bind n) where
 freeT env b
        = freeT env (typeOfBind b)


-- | Add type variables to the set if they are not in the environment.
--   Use freeX for value or witness variables.
instance FreeT n (Bound n) where
 freeT env u
        | Env.member u env      = Set.empty
        | otherwise             
        = case u of
                UName{}         -> Set.singleton u
                UPrim{}         -> Set.empty
                UIx i t         -> Set.singleton $ UIx (i - Env.depth env) t


instance FreeT n (Type n) where
 freeT env tt
  = case tt of
        TVar u          -> freeT env u
        TCon u          -> freeT env u
        TForall b t     -> Set.unions [freeT env b,  freeT (Env.extend b env) t]
        TApp t1 t2      -> Set.unions [freeT env t1, freeT env t2]
        TSum ss         -> freeT env ss


instance FreeT n (TypeSum n) where
 freeT env ss
        = Set.unions $ map (freeT env) $ Sum.toList ss


instance FreeT n (TyCon n) where
 freeT env tc
  = case tc of
        TyConBound u    -> freeT env u
        _               -> Set.empty


