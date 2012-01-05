
module DDC.Type.Transform.Crush
        (crushT)
where
import DDC.Type.Predicates
import DDC.Type.Compounds
import DDC.Type.Exp
import qualified DDC.Type.Sum   as Sum


-- | Crush compound effect terms into their components.
crushT :: Ord n => Type n -> Type n
crushT tt
 = case tt of
        TVar{}          -> tt
        TCon{}          -> tt
        TForall b t
         -> TForall b (crushT t)

        TSum ts         
         -> TSum
          $ Sum.fromList (Sum.kindOfSum ts)   
          $ map crushT 
          $ Sum.toList ts

        TApp t1 t2
         -- Head Read.
         |  Just (TyConComp TcConHeadRead, [t]) <- takeTyConApps tt
         -> case takeTyConApps t of

             -- Type has a head region.
             Just (TyConBound u, (tR : _)) 
              |  k1 : _  <- takeKFuns (typeOfBound u)
              ,  isRegionKind k1
              -> tRead tR

             -- Type has no head region.
             -- This happens with  case () of { ... }
             Just (TyConBound _, [])        -> tBot kEffect

             _ -> tt

         | otherwise
         -> TApp (crushT t1) (crushT t2)
         
