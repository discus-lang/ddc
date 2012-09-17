
module DDC.Type.Collect.FreeT
        (FreeVarConT(..))
where
import DDC.Type.Exp
import Data.Set                 (Set)
import DDC.Type.Env             (KindEnv)
import qualified DDC.Type.Env   as Env
import qualified DDC.Type.Sum   as Sum
import qualified Data.Set       as Set


class FreeVarConT (c :: * -> *) where
  -- | Collect the free type variables and constructors used in a thing.
  freeVarConT 
        :: Ord n 
        => KindEnv n -> c n 
        -> (Set (Bound n), Set (Bound n))


instance FreeVarConT Type where
 freeVarConT kenv tt
  = case tt of
        TVar u  
         -> if Env.member u kenv
                then (Set.empty, Set.empty)
                else (Set.singleton u, Set.empty)

        TCon tc
         | TyConBound u _ <- tc -> (Set.empty, Set.singleton u)
         | otherwise            -> (Set.empty, Set.empty)

        TForall b t
         -> freeVarConT (Env.extend b kenv) t

        TApp t1 t2
         -> let (vs1, cs1)      = freeVarConT kenv t1
                (vs2, cs2)      = freeVarConT kenv t2
            in  ( Set.union vs1 vs2
                , Set.union cs1 cs2)

        TSum ts
         -> let (vss, css)      = unzip $ map (freeVarConT kenv) 
                                $ Sum.toList ts
            in  (Set.unions vss, Set.unions css)


