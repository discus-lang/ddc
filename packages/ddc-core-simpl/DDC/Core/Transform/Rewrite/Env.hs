module DDC.Core.Transform.Rewrite.Env
        ( RewriteEnv
        , empty
        , extend
        , extendLets
        , containsRegion
        , containsWitness
        , getWitnesses
        , insertDef
        , getDef
        , hasDef
        , lift
        , liftValue)
where
import DDC.Core.Exp
import qualified DDC.Type.Exp			as T
import qualified DDC.Type.Compounds		as T
import qualified DDC.Type.Predicates		as T
import qualified DDC.Type.Transform.LiftT	as L
import qualified DDC.Core.Transform.LiftX	as L
import Data.Maybe (fromMaybe, listToMaybe, isJust)


-- | A summary of the environment that we perform a rewrite in.
--
--   As we decend into the program looking for expressions to rewrite, 
--   we keep track of what information as been defined in the environment
--   in a `RewriteEnv`.
--
--   When we go under an anonymous binder then we push a new outermost
--   list instead of lifting every element on the environment eagerly.
--   
data RewriteEnv a n 
        = RewriteEnv
        { -- | Types of all witnesses in scope.
          --   We use these to satisfy constraints on rewrite rules like Const r.
          witnesses     :: [[T.Type n]]

          -- | Names of letregion-bound regions:
          --   this is interesting because they must be distinct.
        , letregions    :: [[Bind n]]

          -- | Assoc of known values
          --   If going to inline them, they must only reference de bruijn binds
          --   these are value-level bindings, so be careful lifting.
        , defs           :: [[RewriteDef a n]] }
        deriving (Show,Eq)


type RewriteDef a n 
        = (Bind n, Maybe (Exp a n))


-- | An empty environment.
empty   :: Ord n => RewriteEnv a n
empty = RewriteEnv [] [] []


-- | Extend an environment with some lambda-bound binder (XLam)
--   Might be a witness. Don't count if it's a region.
extend  :: Ord n => Bind n -> RewriteEnv a n -> RewriteEnv a n
extend b env
        | T.isWitnessType (T.typeOfBind b)
        = let   ty      = T.typeOfBind b
                extend' (w:ws') = (ty:w) : ws'
                extend' []              = [[ty]]
          in    liftValue b $ env { witnesses = extend' (witnesses env) }

        | otherwise
        = insertDef b Nothing (liftValue b env)


-- | Extend an environment with the variables bount by these let-bindings.
--
--   If it's a letregion, remember the region's name and any witnesses.
--
extendLets :: Ord n => Lets a n -> RewriteEnv a n -> RewriteEnv a n
extendLets (LLetRegions bs cs) renv
 = foldl (flip extend) (foldl extendB renv bs) cs
 where  
        extendB (env@RewriteEnv{witnesses = ws, letregions = rs}) b
         = case b of
                BAnon{}
                 -> env { witnesses  = []  : ws
                        , letregions = [b] : rs }
      
                BName{}
                 -> env { letregions = extend' b rs }

                BNone{}
                 -> env

        extend' b (r:rs') = (b:r) : rs'
        extend' b []      = [[b]]

extendLets (LLet b def) env
 = insertDef b (Just def') (liftValue b env)
 where  def' = case b of
                 BAnon{} -> L.liftX 1 def
                 _       -> def

extendLets (LRec bs) env
 = foldl lift' env (map fst bs)
 where  lift' e b = insertDef b Nothing (liftValue b e)


extendLets _ env  = env


-- Witnesses ------------------------------------------------------------------
-- | Check if the witness map in the given environment.
---  
--  This tries each set in turn, lowering the indices in c by 1 after each
--  unsuccessful match. If nothing matches then 'c' may end up with negative 
--  indices, which will definiately not match anything else.
--
containsWitness :: Ord n => Type n -> RewriteEnv a n -> Bool
containsWitness c env
 = go c (witnesses env)
 where  go _  []        = False
        go c' (w:ws)    = c' `elem` w || go (L.liftT (-1) c') ws


-- | Get a list of all the witness types in an environment, 
--   normalising their indices.
getWitnesses :: Ord n => RewriteEnv a n -> [Type n]
getWitnesses env
 = go (witnesses env) 0
 where  go []     _ = []
        go (w:ws) i = map (L.liftT i) w ++ go ws (i+1)


-- Regions --------------------------------------------------------------------
-- | Check whether an environment contains the given region, 
--   bound by a letregion.
containsRegion :: Ord n => Bound n -> RewriteEnv a n -> Bool
containsRegion r env
 = go r (letregions env)
 where  
        go _  []
         = False

        go (UIx 0) (w:_) 
         = any (T.boundMatchesBind (UIx 0)) w

        go (UIx n) (_:ws) 
         = go (UIx (n-1)) ws

        go (UName n) (w:ws) 
         = any (T.boundMatchesBind (UName n)) w || go r ws

        go (UPrim _ _) _
         = False


-- Defs -----------------------------------------------------------------------
-- | Insert a rewrite definition into the environment.
insertDef :: Bind n -> Maybe (Exp a n) -> RewriteEnv a n -> RewriteEnv a n
insertDef b def env
 = env { defs = extend' $ defs env }
 where  
        extend' (r:rs') = ((b,def):r) : rs'
        extend' []      = [[(b,def)]]


hasDef  :: (Ord n, L.MapBoundX (Exp a) n)
        => Bound n -> RewriteEnv a n -> Bool

hasDef b env
 = isJust $ getDef' b env


-- | Lookup the definition of some let-bound variable from the environment.
getDef  :: (Ord n, L.MapBoundX (Exp a) n)
        => Bound n
        -> RewriteEnv a n
        -> Maybe (Exp a n)
getDef b env
 = fromMaybe Nothing $ getDef' b env


getDef' :: (Ord n, L.MapBoundX (Exp a) n)
        => Bound n
        -> RewriteEnv a n
        -> Maybe (Maybe (Exp a n))

getDef' b env
 = go b 0 (defs env)
 where  
        go _ _  []      = Nothing
        go b' i (w:ws)  = match b' i w `orM` go (L.liftX (-1) b') (i+1) ws

        match b' i ds
                = fmap (fmap $ L.liftX i)
                $ listToMaybe
                $ map snd
                $ filter (T.boundMatchesBind b' . fst) ds

        orM (Just x) _  = Just x
        orM Nothing  y  = y


-- | Raise all elements in witness map if binder is anonymous.
--   Only call with type binders: ie XLAM, not XLam
lift :: Bind n -> RewriteEnv a n -> RewriteEnv a n
lift b env@(RewriteEnv ws rs is)
 = case b of
        BAnon{} -> RewriteEnv ([]:ws) ([]:rs) is
        _       -> env


-- | Raise all elements in definitions map if binder is anonymous
--   Use for *value* binders, not type binders.
liftValue :: Bind n -> RewriteEnv a n -> RewriteEnv a n
liftValue b env@(RewriteEnv ws rs is)
 = case b of
        BAnon{} -> RewriteEnv ws rs ([]:is)
        _       -> env

