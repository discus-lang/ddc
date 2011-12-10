
module DDC.Type.Collect.Free
        (Free(..))
where
import DDC.Type.Exp
import DDC.Type.Compounds
import DDC.Type.Env                     (Env)
import Data.Set                         (Set)
import qualified DDC.Type.Sum           as Sum
import qualified DDC.Type.Env           as Env
import qualified Data.Set               as Set


class Free n a where
 -- | Determine the set of variables not bound in the given environment.
 free :: Ord n => Env n -> a -> Set (Bound n)


instance Free n (Bind n) where
 free env b
        = free env (typeOfBind b)


instance Free n (Bound n) where
 free env u
        | Env.member u env      = Set.empty
        | otherwise             
        = case u of
                UName{}         -> Set.singleton u
                UIx i t         -> Set.singleton $ UIx (i - Env.depth env) t


instance Free n (Type n) where
 free env tt
  = case tt of
        TVar u          -> free env u
        TCon u          -> free env u
        TForall b t     -> Set.unions [free env b,  free (Env.extend b env) t]
        TApp t1 t2      -> Set.unions [free env t1, free env t2]
        TSum ss         -> free env ss


instance Free n (TypeSum n) where
 free env ss
        = Set.unions $ map (free env) $ Sum.toList ss

instance Free n (TyCon n) where
 free env tc
  = case tc of
        TyConBound u    -> free env u
        _               -> Set.empty


