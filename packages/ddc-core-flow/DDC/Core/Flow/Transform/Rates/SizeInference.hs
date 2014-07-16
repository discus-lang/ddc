-- | Performing size inference on a program in Combinator Normal Form
module DDC.Core.Flow.Transform.Rates.SizeInference where
import DDC.Core.Flow.Transform.Rates.Combinators

import Data.List     (sortBy)
import Data.Function (on)
import Control.Applicative

-----------------------------------
-- = Size types, constraints and schemes

-- | Given some variable type, append a number of primes onto it.
-- We want to be able to distinguish between the raw variable types and unification, existential Klock (rate) variables.
-- We generate constraints so that raw variables will appear on the left of equalities, and Ks may appear on the right.
data K v
 = KV v
 | K' (K v)
 deriving (Eq,Ord)


-- | tau ::=
data Type v
 -- | k
 = TVar   (K v)
 -- | tau * tau
 | TCross (Type v) (Type v)

-- | Find all variables in type
freeT :: Type a -> [K a]
freeT (TVar a)     = [a]
freeT (TCross a b) = freeT a ++ freeT b


-- | C ::=
data Constraint v
 -- | true
 = CTrue
 -- | v = tau
 | CEqual v (Type v)
 -- | C /\ C
 | CAnd (Constraint v) (Constraint v)

-- | Big conjunction. Join a bunch of constraints together
ands :: [Constraint v] -> Constraint v
ands = foldr CAnd CTrue

-- | Flatten a set of constraints into a simpler, canonical form.
-- Turn it into a list of variable/type equalities, ordered by variable name.
flatten :: Ord v => Constraint v -> [(v, Type v)]
flatten = sortBy (compare `on` fst) . go
 where
  go CTrue        = []
  go (CEqual v k) = [(v,k)]
  go (CAnd a b)   = go a ++ go b

-- | Convert back from flattened form to a set of @Constraint@
unflatten :: [(v, Type v)] -> Constraint v
unflatten = ands . map (uncurry CEqual)


-- | sigma ::= forall k... exists k... (x : t)... -> (x : t)...
-- Note that these are all raw variable types.
-- There is no point mentioning unification variables in this solved form.
data Scheme v
 = Scheme
 { _forall :: [v]
 , _exists :: [v]
 , _from   :: [(v, Type v)]
 , _to     :: [(v, Type v)]
 }


-----------------------------------
-- = Constraint generation
-- == Environment

-- | Gamma ::= • | Gamma, Gamma...
type Env v = [Scope v]

-- | Gamma ::= ...
data Scope v
 -- | v : k
 = EVar v (K v)
 -- | k
 | EUnify (K v)
 -- | exists k
 | ERigid (K v)

evar :: v -> Scope v
evar v
 = EVar v (KV v)


-- | Search for first (should be only) mention of k in environment, along with its index
lookupK :: Eq v => Env v -> K v -> Maybe (Int, Scope v)
lookupK es k
 = go es 0
 where
  go [] _ = Nothing
  go (e@(EVar _ k') : _) i
     | k == k'
     = Just (i, e)
  go (e@(EUnify k') : _) i
     | k == k'
     = Just (i, e)
  go (e@(ERigid k') : _) i
     | k == k'
     = Just (i, e)
  go (_:es') i
   = go es' (i+1)

-- == Generation of constraints
-- For example, the program
--
-- > normalize2 :: Array Int -> (Array Int, Array Int)
-- > normalize2 xs
-- >  = let sum1 = fold (+) 0 xs
-- >        gts = filter (> 0) xs
-- >        sum2 = fold (+) 0 gts
-- >        ys1 = map (/ sum1) xs
-- >        ys2 = map (/ sum2) xs
-- >    in (ys1, ys2)
--
-- will generate environment and constraints
--
-- >    xs : kxs, gts : kgts, ys1 : kys1, ys2 : kys2, ∃k1, k2, k3
-- > |- true ∧ kgts = k1 ∧ true
-- >         ∧ kxs  = k2 ∧ kys1 = k2 ∧ kxs = k3 ∧ kys2 = k3
--
-- | program :_s sigma
generate :: Ord a => Program s a -> Maybe (Scheme a)
generate (Program (_inSs,inAs) binds _outs)
 = do   let e         = concatMap (\i -> [EUnify (KV i), evar i]) inAs
        let (e',  c') = generateLets e binds
        c''        <- solve e' c'
        return undefined

-- | Gamma |- lets ~> Gamma |- C
generateLets :: Ord a => Env a -> [Bind s a] -> (Env a, Constraint a)
generateLets e bs
 = foldl go (e, CTrue) bs
 where
  go (e',c') b
   = let (e'', c'') = generateBind e' b
     in  (e'', c' `CAnd` c'')

-- | Gamma | z |- bind ~> Gamma |- C
generateBind :: Ord a => Env a -> Bind s a -> (Env a, Constraint a)
generateBind env b
 = case b of
   ABind z (MapN _ xs)
    -> let u    = K' (KV z)
           env' = evar z : EUnify u : env
           con  = ands $ map (\i -> CEqual i (TVar u)) (z : xs)
       in (env', con)

   ABind z (Filter _ _)
    -> let u    = K' (KV z)
           env' = evar z : ERigid u : env
           con  = CEqual z (TVar u)
       in (env', con)

   SBind _ (Fold _ _)
    -> (env, CTrue)

   ABind z (Generate _ _)
    -> let u    = K' (KV z)
           env' = evar z : ERigid u : env
           con  = CEqual z (TVar u)
       in (env', con)

   ABind z (Gather _ i)
    -> let u    = K' (KV z)
           env' = evar z : EUnify u : env
           con  = CEqual z (TVar u) `CAnd` CEqual z (TVar $ KV i)
       in (env', con)

   ABind z (Cross x y)
    -> let u    =     K' (KV z)
           u'   = K' (K' (KV z))
           env' = evar z : EUnify u' : EUnify u : env
           con  = ands [ CEqual z (TCross (TVar u) (TVar u'))
                       , CEqual x (TVar u)
                       , CEqual y (TVar u') ]
       in (env', con)

   Ext (_outS, outA) _ (_inS, _inA)
    -> let env' = concatMap (\a -> [evar a, ERigid $ K' $ KV a]) outA ++ env
           con  = ands $ map(\a -> CEqual a $ TVar $ K' $ KV a) outA
       in (env', con)



        
-- | Solving constraints.
-- If we take the environment and constraints from @normalize2@,
-- >    xs : kxs, gts : kgts, ys1 : kys1, ys2 : kys2, ∃k1, k2, k3
-- > |- true ∧ kgts = k1 ∧ true
-- >         ∧ kxs  = k2 ∧ kys1 = k2 ∧ kxs = k3 ∧ kys2 = k3
-- the constraints can be converted to canonical form by 'flatten':
-- >  [kgts = k1, kxs = k2, kxs = k3, kys1 = k2, kys2 = k3]
-- After that, we iterate through the list of constraints, finding duplicate left hand sides.
-- For some duplicate left hand side, such as
-- >              kxs = k2, kxs = k3
-- the k2 and k3 must be unified in Env.

solve :: Ord a => Env a -> Constraint a -> Maybe (Constraint a)
solve e c
 = let cs   = flatten c
   in  unflatten <$> go cs cs
 where
  go [ ] c' = return c'
  go [_] c' = return c'

  go ((x,a):(y,b):rs) c'
   | x == y
   = do  sub <- unify e a b
         -- Substitute into both the full constraint set,
         -- and just those remaining to check
         let c'' = substCs sub c'
         let rest= substCs sub ((y,b) : rs)
         go rest c''
   | otherwise
   = go ((y,b) : rs) c'


-- | Check that environment binds all variables before being mentioned in types etc.
-- For example,...
{-
checkE :: Ord a => Env a -> Bool
checkE env = go env []
 where
  go [] _
   = True
  go (e:es) bound
   = case e of
     EVar x t
      -> checkT t bound && elem (KV x) bound && go es bound
     ERigid x
      -> go es (x : bound)
     EUnify x
      -> go es (x : bound)

  checkT (TVar x) bound
   = elem x bound
  checkT (TCross a b) bound
   = checkT a bound && checkT b bound
-}

-- | A substitution, mapping some variable to a type
type Subst a = [(K a, Type a)]

-- | Perform substitution over types
substT :: Ord a => Subst a -> Type a -> Type a
substT sub t@(TVar a)
 | Just t' <- lookup a sub
 = t'
 | otherwise
 = t
substT sub (TCross a b)
 = TCross (substT sub a) (substT sub b)

-- | Perform substitution over already-flattened constraints
substCs :: Ord a => Subst a -> [(a, Type a)] -> [(a, Type a)]
substCs sub cs
 = map (\(v,t) -> (v, substT sub t)) cs

-- | Perform substitution over environment
{-
substE :: Ord a => Subst a -> Env a -> Env a
substE sub es
 = map go es
 where
  go (EVar v t) = EVar v (substT sub t)
  go e          = e
-}

-- | Given two types, find a substitution that unifies the two together
-- The only tricky thing is, given a choice between unifying two variables, set to the variable that is bound first in the environment.
-- This makes it that no matter which order substitutions are applied, the result will still be valid according to 'checkE'.
--
-- For example, consider
-- > E: forall k1, x : k1, forall k2, y : k2
-- > C: x = k1 /\ x = k2 /\ y = k2
-- if we substitute k1 with k2 in the environment, we would get
-- > E: forall k1, x : k2, forall k2, y : k2
-- which is not valid, but substituting k2 with k1 works fine.
--
unify :: Ord a => Env a -> Type a -> Type a -> Maybe (Subst a)
unify e l r
 = let subs = go l r
   in  if   all check subs
       then Just subs
       else Nothing
 where
  go (TVar a) (TVar b)
   | a == b       = []
   | otherwise    = [(a, TVar b)]

  go (TCross a1 a2) (TCross b1 b2)
   = go a1 b1 ++ go a2 b2

  go (TVar a) b@(TCross _ _)
   = [(a, b)]

  go a@(TCross _ _) (TVar b)
   = [(b, a)]

  check (v,t)
   | Just (vi, ve)  <- lookupK e v
   , Just  ts       <- mapM (lookupK e) (freeT t)
   -- We are only interested in existentials (rigids):
   -- two rigids cannot be unified together,
   -- and a rigid can only be unified with a unify if the unify is bound *after* the rigid
   = all (good (vi,ve)) ts

   | otherwise
   = False

  good (vi,ve) (ti,te)
   = case (ve,te) of
     (ERigid _, ERigid _) -> False
     -- Because we add to the front of the env, later ones with higher indices are bound first
     (ERigid _, EUnify _) -> vi > ti
     (EUnify _, ERigid _) -> ti > vi
     _                    -> True
     

-- | Check whether first var is bound before second in Env.
{-
firstE :: Ord a => Env a -> K a -> K a -> Bool
firstE []     _ _ = False
firstE (e:es) a b =
 case e of
  ERigid x -> check x
  EUnify x -> check x
  _        -> firstE es a b
 where
  check x
   | x == a
   = True
   | x == b
   = False
   | otherwise
   = firstE es a b
-}

