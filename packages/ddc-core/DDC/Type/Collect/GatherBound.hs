
module DDC.Type.Collect.GatherBound
        (GatherBound(..))
where
import DDC.Type.Exp
import DDC.Type.Compounds
import Data.Set                         (Set)
import qualified DDC.Type.Sum           as Sum
import qualified Data.Set               as Set


class GatherBound n a where
 -- | Gather all bound names used in a thing,
 --   indpendent of whether they are free or not.
 gatherBound :: Ord n => a -> Set (Bound n)
 

instance GatherBound n (Bind n) where
 gatherBound bb
        = gatherBound (typeOfBind bb)
        

instance GatherBound n (Bound n) where
 gatherBound uu
        = Set.singleton uu


instance GatherBound n (Type n) where
 gatherBound tt
  = case tt of
          TVar uu       -> gatherBound uu
          TCon uu       -> gatherBound uu
          TForall b t   -> Set.unions [gatherBound b,  gatherBound t]
          TApp t1 t2    -> Set.unions [gatherBound t1, gatherBound t2]
          TSum ss       -> gatherBound ss
          

instance GatherBound n (TypeSum n) where
 gatherBound ss
        = Set.unions $ map gatherBound $ Sum.toList ss


instance GatherBound n (TyCon n) where
 gatherBound tc
  = case tc of
        TyConBound u    -> gatherBound u
        _               -> Set.empty
        
