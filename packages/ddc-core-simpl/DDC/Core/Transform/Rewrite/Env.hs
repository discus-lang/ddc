module DDC.Core.Transform.Rewrite.Env
    ( RewriteEnv
    , empty
    , extend
    , extendLets
    , containsRegion
    , containsWitness
    , insertDef
    , getDef
    , lift
    , liftValue
    )
where

import DDC.Core.Exp
import qualified DDC.Type.Exp			as T
import qualified DDC.Type.Compounds		as T
import qualified DDC.Type.Predicates		as T
import qualified DDC.Type.Transform.LiftT	as L
import qualified DDC.Core.Transform.LiftX	as L

import Data.Maybe (listToMaybe)

-- | The environment of an expression,
-- with witnesses and distinct regions.
--
-- Because of the de Bruijn indices, using lists to make lifting easier.
data RewriteEnv a n = RewriteEnv
    { witnesses	 :: [[T.Type n]]
	-- ^ types of all witnesses in scope
    , letregions :: [[Bind n]]
	-- ^ names of letregion-bound regions:
	-- this is interesting because they must be distinct.
    , defs	 :: [[RewriteDef a n]]
	-- ^ assoc of known values
	-- if going to inline them, they must only reference de bruijn binds
	-- these are value-level bindings, so be careful lifting
    }
    deriving (Show,Eq)
type RewriteDef a n = (Bind n, Exp a n)

-- | Create an empty environment
empty
    :: Ord n
    => RewriteEnv a n
empty = RewriteEnv [] [] []

-- | Extend an environment with some lambda-bound binder (XLam)
-- Might be a witness. Don't count if it's a region, 
extend
    :: (Ord n, Show n)
    => Bind n
    -> RewriteEnv a n
    -> RewriteEnv a n
extend b env
  | T.isWitnessType ty	= liftValue b $ env { witnesses = extend' (witnesses env) }
  | otherwise		= liftValue b $ env
 where
    ty = T.typeOfBind b
    extend' (w:ws')	= (ty:w) : ws'
    extend' []		= [[ty]]


-- | Extend with some lets.
-- If it's a letregion, remember the region's name and any witnesses.
extendLets
    :: (Ord n, Show n)
    => Lets a n
    -> RewriteEnv a n
    -> RewriteEnv a n
extendLets (LLetRegion b cs) (env@RewriteEnv{letregions = rs})
 | BAnon _	<- b
 = foldl (flip extend) (lift b $ env { letregions = [b]:rs } ) cs
 | BName _ _	<- b
 = foldl (flip extend) (lift b $ env { letregions = extend' rs } ) cs
 where
    extend' (r:rs') = (b:r) : rs'
    extend' []	   = [[b]]

extendLets (LLet _ b def) env
 =  insertDef b def' (liftValue b env)
 where
   def' = case b of
	  BAnon{} -> L.liftX 1 def
	  _	  -> def
 -- =  liftValue b $ insertDef b def env

extendLets (LRec bs) env
 = foldl (flip liftValue) env (map fst bs)

extendLets _ env = env

insertDef b def env
 =  env { defs = extend' $ defs env }
 where
    extend' (r:rs') = ((b,def):r) : rs'
    extend' []	    = [[(b,def)]]

containsRegion
    :: (Eq n, Ord n, Show n)
    => Bound n
    -> RewriteEnv a n
    -> Bool
containsRegion r env
 = go r (letregions env)
 where
    go _  []	= False

    go (UIx 0) (w:_) 
	= any (T.boundMatchesBind (UIx 0)) w

    go (UIx n) (_:ws) 
	= go (UIx (n-1)) ws

    go (UName n) (w:ws) 
	= any (T.boundMatchesBind (UName n)) w || go r ws

    go (UPrim _ _) _
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

getDef
    :: (Eq n, Ord n, Show n, L.LiftX (Exp a))
    => Bound n
    -> RewriteEnv a n
    -> Maybe (Exp a n)
getDef b env
 = go b 0 (defs env)
 where
    go _ _  []	   = Nothing
    go b' i (w:ws) = match b' i w `orM` go (L.liftX (-1) b') (i+1) ws

    match b' i ds  = fmap (L.liftX i) $ listToMaybe $ map snd $ filter (T.boundMatchesBind b' . fst) ds

    orM (Just x) _  = Just x
    orM Nothing  y  = y


-- | raise all elements in witness map if binder is anonymous
-- only call with type binders ie XLAM, not XLam
lift (BAnon _) (RewriteEnv ws rs is)
    = RewriteEnv ([]:ws) ([]:rs) is
lift _ env = env


-- | raise all elements in definitions map if binder is anonymous
-- *value* binders, not type binders
liftValue (BAnon _) (RewriteEnv ws rs is)
    = RewriteEnv ws rs ([]:is)
liftValue _ env = env

