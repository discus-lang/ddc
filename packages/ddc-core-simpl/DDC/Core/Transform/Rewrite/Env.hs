module DDC.Core.Transform.Rewrite.Env
    ( RewriteEnv
    , empty
    , extend
    , extendLets
    , containsRegion
    , containsWitness
    , insertDef
    , getDef
    , getDef'
    , isDef
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

import Data.Maybe (fromMaybe, listToMaybe, isJust)

-- | The environment of an expression with witnesses and distinct regions.
--
--   Because of the de Bruijn indices, using lists to make lifting easier.
data RewriteEnv a n = RewriteEnv
    { witnesses	 :: [[T.Type n]]
	-- ^ Types of all witnesses in scope
    , letregions :: [[Bind n]]
	-- ^ Names of letregion-bound regions:
	-- this is interesting because they must be distinct.
    , defs	 :: [[RewriteDef a n]]
	-- ^ Assoc of known values
	-- If going to inline them, they must only reference de bruijn binds
	-- these are value-level bindings, so be careful lifting
    }
    deriving (Show,Eq)

type RewriteDef a n = (Bind n, Maybe (Exp a n))


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
  | otherwise		= insertDef b Nothing (liftValue b env)
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
extendLets (LLetRegions bs cs) (renv@RewriteEnv{letregions = rs})
 = foldl extendB renv bs
 where
    extendB env b
      | BAnon _     <- b
      = foldl (flip extend) (lift b $ env { letregions = [b]:rs } ) cs
      | BName _ _   <- b
      = foldl (flip extend) (lift b $ env { letregions = extend' b rs } ) cs

      -- Otherwise it's a BNone, so don't need to modify environment
      | otherwise
      = env

    extend' b (r:rs') = (b:r) : rs'
    extend' b []	   = [[b]]

extendLets (LLet _ b def) env
 =  insertDef b (Just def') (liftValue b env)
 where
   def' = case b of
	  BAnon{} -> L.liftX 1 def
	  _	  -> def

extendLets (LRec bs) env
 = foldl lift' env (map fst bs)
 where
  lift' e b = insertDef b Nothing (liftValue b e)

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
    :: (Eq n, Ord n, Show n, L.MapBoundX (Exp a) n)
    => Bound n
    -> RewriteEnv a n
    -> Maybe (Exp a n)
getDef b env
 = fromMaybe Nothing $ getDef' b env


getDef'
    :: (Eq n, Ord n, Show n, L.MapBoundX (Exp a) n)
    => Bound n
    -> RewriteEnv a n
    -> Maybe (Maybe (Exp a n))
getDef' b env
 = go b 0 (defs env)
 where
    go _ _  []	    = Nothing
    go b' i (w:ws)  = match b' i w `orM` go (L.liftX (-1) b') (i+1) ws

    match b' i ds   = fmap (fmap $ L.liftX i)
		    $ listToMaybe
		    $ map snd
		    $ filter (T.boundMatchesBind b' . fst) ds

    orM (Just x) _  = Just x
    orM Nothing  y  = y


isDef
    :: (Eq n, Ord n, Show n, L.MapBoundX (Exp a) n)
    => Bound n
    -> RewriteEnv a n
    -> Bool
isDef b env
 = isJust $ getDef' b env


-- | Raise all elements in witness map if binder is anonymous.
-- Only call with type binders: ie XLAM, not XLam
lift (BAnon _) (RewriteEnv ws rs is)
    = RewriteEnv ([]:ws) ([]:rs) is
lift _ env = env


-- | Raise all elements in definitions map if binder is anonymous
-- Use for *value* binders, not type binders.
liftValue (BAnon _) (RewriteEnv ws rs is)
    = RewriteEnv ws rs ([]:is)
liftValue _ env = env

