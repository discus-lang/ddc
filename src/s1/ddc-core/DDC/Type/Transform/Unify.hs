
module DDC.Type.Transform.Unify
        (unifyExistsRight)
where
import DDC.Type.Exp.Simple
import DDC.Core.Env.EnvT
import qualified Data.Map       as Map


-- | Half-assed unification of left and right types, also known as "matching".
--   Where we're permitted to create constraints for the right type only.
unifyExistsRight
        :: Ord n
        => EnvT n       -- ^ Environment containing synonyms to look through.
        -> Type n       -- ^ Match against this type.
        -> Type n       -- ^ Create constraints for existentials in this type.
        -> Maybe [(Int, Type n)]

unifyExistsRight envt tL tR

 -- Look through synonyms on the left.
 | TCon (TyConBound (UName n1) _) <- tL
 , Just tL' <- Map.lookup n1 (envtEquations envt)
 = unifyExistsRight envt tL' tR

 -- Look through synonyms on the right.
 | TCon (TyConBound (UName n2) _) <- tR
 , Just tR' <- Map.lookup n2 (envtEquations envt)
 = unifyExistsRight envt tL tR'

 | otherwise
  = case (tL, tR) of

        (t1, TCon (TyConExists i2 _k2))
          -> Just [(i2, t1)]

        (TCon (TyConBound u1 _k1), TCon (TyConBound u2 _k2))
         | u1 == u2     -> Just []
         | otherwise    -> Nothing


        (TCon tc1, TCon tc2)
         | tc1 == tc2   -> Just []
         | otherwise    -> Nothing


        (TVar u1,  TVar u2)
         | u1 == u2     -> Just []
         | otherwise    -> Nothing

        (TAbs{}, TAbs{})
         -> Nothing

        (TApp t11 t12, TApp t21 t22)
         |  Just cs1 <- unifyExistsRight envt t11 t21
         ,  Just cs2 <- unifyExistsRight envt t12 t22
         -> Just (cs1 ++ cs2)

        (TForall{}, TForall{})
         -> Nothing

        (TSum {}, TSum{})
         | equivT envt tL tR    -> Just []
         | otherwise            -> Nothing

        _ -> Nothing

