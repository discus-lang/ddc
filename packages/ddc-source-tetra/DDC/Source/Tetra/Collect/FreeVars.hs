
module DDC.Source.Tetra.Collect.FreeVars where
import DDC.Source.Tetra.Exp
import DDC.Source.Tetra.Env             (Env, Presence(..))
import Data.Set                         (Set)
import qualified DDC.Source.Tetra.Env   as Env
import qualified Data.Set               as Set


-- | Collect the variables a type that do not appear in the given environment.
freeVarsT :: Env -> Type -> Set Bound
freeVarsT env tt
 = case tt of
        TAnnot _ t
         -> freeVarsT env t

        TCon{}
         -> Set.empty

        TVar u@(UName _)
         -> case Env.lookupTyVar env u of
                Present _ -> Set.empty
                Unknown   -> Set.empty
                Absent    -> Set.singleton u

        TVar u@(UIx i)
         -> case Env.lookupTyVar env u of
                Present _ -> Set.empty
                Unknown   -> Set.empty
                Absent    -> Set.singleton (UIx (i - Env.tyStackDepth env))

        TVar UHole
         -> Set.empty

        TAbs{}
         |  Just (k, b, t)       <- takeTForall tt
         -> freeVarsT (Env.extendTyVar b k env) t

         |  otherwise
         -> Set.empty

        TApp t1 t2
         -> Set.union (freeVarsT env t1) (freeVarsT env t2)

