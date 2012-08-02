module DDC.Core.Transform.Rewrite.Env
    ( RewriteEnv
    , empty
    , extend
    , extendLets
    , containsRegion
    , containsWitness
    , lift
    )
where

import DDC.Core.Exp
import qualified DDC.Type.Exp			as T
import qualified DDC.Type.Compounds		as T
import qualified DDC.Type.Predicates		as T
import qualified DDC.Type.Transform.LiftT	as L

-- | The environment of an expression,
-- with witnesses and distinct regions.
--
-- Because of the de Bruijn indices, using lists to make lifting easier.
data RewriteEnv n = RewriteEnv
    { witnesses	 :: [[T.Type n]]
	-- ^ types of all witnesses in scope
    , letregions :: [[Bind n]]
	-- ^ names of letregion-bound regions:
	-- this is interesting because they must be distinct.
    }
    deriving (Show,Eq)

-- | Create an empty environment
empty
    :: Ord n
    => RewriteEnv n
empty = RewriteEnv [] []

-- | Extend an environment with some lambda-bound binder (XLam)
-- Might be a witness. Don't count if it's a region, 
extend
    :: (Ord n, Show n)
    => Bind n
    -> RewriteEnv n
    -> RewriteEnv n
extend b env
  | T.isWitnessType ty	= env { witnesses = extend' (witnesses env) }
  | otherwise		= env
 where
    ty = T.typeOfBind b
    extend' (w:ws')	= (ty:w) : ws'
    extend' []		= [[ty]]


-- | Extend with some lets.
-- If it's a letregion, remember the region's name and any witnesses.
extendLets
    :: (Ord n, Show n)
    => Lets a n
    -> RewriteEnv n
    -> RewriteEnv n
extendLets (LLetRegion b cs) (RewriteEnv ws rs)
 | BAnon _	<- b
 = foldl (flip extend) (lift b $ RewriteEnv ws ([b]:rs)) cs
 | BName _ _	<- b
 = foldl (flip extend) (lift b $ RewriteEnv ws (extend' rs)) cs
 where
    extend' (r:rs') = (b:r) : rs'
    extend' []	   = [[b]]

extendLets _ env = env

containsRegion
    :: (Eq n, Ord n, Show n)
    => Bound n
    -> RewriteEnv n
    -> Bool
containsRegion r env
 = go r (letregions env)
 where
    go _  []	= False

    go (UIx 0 ty) (w:_) 
	= (BAnon ty) `elem` w
    go (UIx n ty) (_:ws) 
	= go (UIx (n-1) ty) ws

    go (UName n ty) (w:ws) 
	= BName n ty `elem` w || go r ws

    go (UPrim _ _) _
	= False
    go (UHole _) _
	= False

-- | check if witness map contains given type
-- tries each set, lowering c by -1 after each failure
-- c may end up with negative indices,
-- not such a big deal since sets certainly won't match that
containsWitness c env
 = go c (witnesses env)
 where
    go _  []	= False
    go c' (w:ws)= c' `elem` w || go (L.liftT (-1) c') ws

-- | raise all elements in witness map if binder is anonymous
-- only call with type binders ie XLAM, not XLam
lift (BAnon _) (RewriteEnv ws rs)
    = RewriteEnv ([]:ws) ([]:rs)
lift _ env = env

