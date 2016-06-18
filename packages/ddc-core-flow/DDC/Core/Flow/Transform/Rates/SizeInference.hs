-- | Performing size inference on a program in Combinator Normal Form
module DDC.Core.Flow.Transform.Rates.SizeInference
    ( Type(..), K(..), Env, Scope(..), Scheme(..)
    , generate
    , lookupV
    , iter
    , parents
    , trans ) where
import DDC.Base.Pretty
import DDC.Core.Flow.Transform.Rates.Combinators

import Data.List
import Data.Function (on)
import Data.Maybe
import qualified Control.Applicative as A
import Control.Monad

-----------------------------------
-- = Size types, constraints and schemes

-- | Given some variable type, append a number of primes onto it.
-- We want to be able to distinguish between the raw variable types and unification, existential Klock (rate) variables.
-- We generate constraints so that raw variables will appear on the left of equalities, and Ks may appear on the right.
data K v
 = KV v
 | K' (K v)
 deriving (Eq,Ord,Show)


-- | tau ::=
data Type v
 -- | k
 = TVar   (K v)
 -- | tau * tau
 | TCross (Type v) (Type v)
 deriving (Eq, Ord,Show)

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
 deriving (Show)

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
 { _forall :: [K v]
 , _exists :: [K v]
 , _from   :: [(v, Type v)]
 , _to     :: [(v, Type v)]
 }
 deriving (Show)


-----------------------------------
-- = Constraint generation
-- == Environment

-- | Gamma ::= • | Gamma, Gamma...
type Env v = [Scope v]

-- | Gamma ::= ...
data Scope v
 -- | v : k
 = EVar v (Type v)
 -- | k
 | EUnify (K v)
 -- | exists k
 | ERigid (K v)
 deriving (Show)

evar :: v -> Scope v
evar v
 = EVar v $ TVar $ KV v


-- | Search for first (should be only) mention of k in environment, along with its index
lookupV :: Eq v => Env v -> v -> Maybe (Type v)
lookupV es v
 = go es
 where
  go [] = Nothing
  go (EVar v' t : _)
     | v == v'
     = Just t
  go (_:es')
   = go es'

-- | Search for first (should be only) mention of k in environment, along with its index
isUnify :: Eq v => Env v -> K v -> Bool
isUnify es k
 = go es
 where
  go [] = False
  go (EUnify k' : _)
     | k == k'
     = True
  go (_:es')
   = go es'


-- | Find all free variables in types of environment
freeE :: Env a -> [K a]
freeE es
 = concatMap go es
 where
  go (EVar _ t) = freeT t
  go  _         = []


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
generate :: Ord a => Program s a -> Maybe (Env a, Scheme a)
generate program@(Program (_inSs,inAs) _binds (_outSs,outAs))
 = do   e <- generateEnv program
        let fvs = freeE e

        let alls  x = case x of
                        EUnify v | v `elem` fvs -> [v]
                        _                       -> []
        let exis  x = case x of
                        ERigid v | v `elem` fvs -> [v]
                        _                       -> []

        let us = concatMap alls e
        let rs = concatMap exis e

        let inTs = catMaybes $ map (lookupV e) inAs
        let ouTs = catMaybes $ map (lookupV e) outAs

        let fvIn = nub $ concatMap freeT inTs
        let fvOu = nub $ concatMap freeT ouTs

        -- check if there are any rigids mentioned in input types:
        -- this means we'd have an existential input, which is nonsense.
        when (any (flip elem rs) fvIn)
            Nothing

        let sch = Scheme
                { _forall = fvIn  `intersect` us
                , _exists = fvOu  `intersect` rs
                , _from   = inAs  `zip` inTs
                , _to     = outAs `zip` ouTs
                }
        return (e, sch)


-- | Get environment
generateEnv :: Ord a => Program s a -> Maybe (Env a)
generateEnv (Program (_inSs,inAs) binds _outs)
 = do   let e         = concatMap (\i -> [EUnify (KV i), evar i]) inAs
        let (e',  c') = generateLets e binds
        -- If solve succeeds, there will be no duplicate left-hand sides in c''.
        (e'', c'')   <- solve e' c'
        -- Now, we must group the variables into equivalence classes.
        -- Take all the leftover constraints and substitute them into the environment
        return $ substEAll c'' e''


-- | Gamma |- lets ~> Gamma |- C
generateLets :: Env a -> [Bind s a] -> (Env a, Constraint a)
generateLets e bs
 = foldl go (e, CTrue) bs
 where
  go (e',c') b
   = let (e'', c'') = generateBind e' b
     in  (e'', c' `CAnd` c'')

-- | Gamma | z |- bind ~> Gamma |- C
generateBind :: Env a -> Bind s a -> (Env a, Constraint a)
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

   SBind _ (Fold _ _ _)
    -> (env, CTrue)

   ABind z (Generate _ _)
    -> let u    = K' (KV z)
           env' = evar z : ERigid u : env
           con  = CEqual z (TVar u)
       in (env', con)

   ABind z (Gather _ i)
    -> let u    = K' (KV z)
           env' = evar z : EUnify u : env
           con  = CEqual z (TVar u) `CAnd` CEqual i (TVar u)
       in (env', con)

   ABind z (Cross x y)
    -> let u    =     K' (KV z)
           u'   = K' (K' (KV z))
           env' = evar z : EUnify u' : EUnify u : env
           con  = ands [ CEqual z (TCross (TVar u) (TVar u'))
                       , CEqual x (TVar u)
                       , CEqual y (TVar u') ]
       in (env', con)

   Ext (NameArray  a) _ (_, _)
    -> let env' = [evar a, ERigid $ K' $ KV a] ++ env
           con  = CEqual a $ TVar $ K' $ KV a
       in (env', con)

   Ext (NameScalar _) _ (_, _)
    -> (env, CTrue)



        
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
solve :: Ord a => Env a -> Constraint a -> Maybe (Env a, Constraint a)
solve e c
 = let cs   = flatten c
   in  go cs e cs
 where
  go [ ] e' c' = return (e', unflatten c')
  go [_] e' c' = return (e', unflatten c')

  go ((x,a):(y,b):rs) e' c'
   | x == y
   = do  sub <- unify e a b
         -- Substitute into both the full constraint set,
         -- and just those remaining to check
         let e'' = substE  sub e'
         let c'' = substCs sub c'
         let rest= substCs sub ((y,b) : rs)
         go rest e'' c''
   | otherwise
   = go ((y,b) : rs) e' c'


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
substE :: Ord a => Subst a -> Env a -> Env a
substE sub es
 = map go es
 where
  go (EVar v t) = EVar v (substT sub t)
  go e          = e


-- | Take all constraints, treat them as substitutions
substEAll :: Ord a => Constraint a -> Env a -> Env a
substEAll cs es
 = substE (map mkSub $ flatten cs) es
 where
  mkSub (v,t) = (KV v, t)


-- | Given two types, find a substitution that unifies the two together
-- The substitution may only change the value of unifier variables
unify :: Ord a => Env a -> Type a -> Type a -> Maybe (Subst a)
unify e l r
 = go l r
 where
  go (TVar a) (TVar b)
   | a == b       = Just []
   | isUnify e a  = Just [(a, TVar b)]
   | isUnify e b  = Just [(b, TVar a)]
   -- Neither variables are unifiers. It cannot be done.
   | otherwise    = Nothing

  go (TCross a1 a2) (TCross b1 b2)
   = (++) A.<$> go a1 b1 A.<*> go a2 b2

  go (TVar a) b@(TCross _ _)
   | isUnify e a
   = Just [(a, b)]
   | otherwise
   = Nothing

  go a@(TCross _ _) (TVar b)
   | isUnify e b
   = Just [(b, a)]
   | otherwise
   = Nothing


-- = Iteration size and transducers

-- | Find iteration size of given combinator
iter :: (Eq a, Eq s) => Program s a -> Env a -> CName s a -> Maybe (Type a)
iter program e nm
 | NameScalar  nm' <- nm
 = do   b <- lookupS program nm'
        case b of
         Fold _ _ n -> get n 
 | NameArray   nm' <- nm
 = do   b <- lookupA program nm'
        case b of
         MapN{}         -> get nm'
         Filter _ as    -> get as
         Generate _ _   -> get nm'
         Gather _d is   -> get is
         Cross  as bs   -> TCross A.<$> get as A.<*> get bs
 -- Otherwise, it's external.
 | otherwise
 = Nothing
 where
  get = lookupV e


-- | Find a bindings' transducer.
-- Only array bindings can have transducers.
trans :: (Eq a, Eq s) => Program s a -> CName s a -> Maybe (CName s a)
trans bs nm
 | NameArray nm' <- nm
 , Just (Filter _ n) <- lookupA bs nm' = trans' (NameArray n)
 | otherwise                           = trans' nm
 where
  trans' (NameScalar o)
   = case lookupS bs o of
     Just (Fold _ _ n)
      -> trans' (NameArray n)

     -- Not a binding, or an external
     Nothing
      -> Nothing

  trans' (NameArray o)
   = case lookupA bs o of
     Just (Filter _ _n)
      -> Just (NameArray o)

     Just (MapN _ ns)
      -> listToMaybe $ catMaybes $ map (trans' . NameArray) ns
     Just (Gather _d i)
      -> trans' (NameArray i)

     Just (Generate _ _)
      -> Nothing
     Just (Cross _ _)
      -> Nothing

     -- Not a binding, or an external
     Nothing
      -> Nothing


-- | Find pair of parent transducers with same iteration size
parents :: (Eq a, Eq s) => Program s a -> Env a -> CName s a -> CName s a -> Maybe (CName s a, CName s a)
parents bs e a b
 | itsz a == itsz b
 = Just (a,b)

 | otherwise
 = let pas = trans bs a >>= \a'' -> parents bs e a'' b
       pbs = trans bs b >>= \b'' -> parents bs e a   b''
       ps  = catMaybes [pas, pbs]
       -- If @b@ is a child of @a@, we want to find the parents @(a,a)@.
       -- There may be other parents, but @(a,a)@ is the "most specific".
       -- There is actually an error in the paper - nodes may have multiple parents
       -- if one node is the parent/transducer of the other.
       same= filter (\(a',b') -> a' `elem` [a,b] || b' `elem` [a,b]) ps
   in  case same of
        (s:_) -> Just s
        []    -> listToMaybe ps

 where
  itsz = iter bs e
 
-----------------------------------
-- == Pretty printing

instance (Pretty v) => Pretty (K v) where
 ppr (KV v) = ppr v
 ppr (K' v) = ppr v <> squote

instance (Pretty v) => Pretty (Type v) where
 ppr (TVar   v)   = ppr v
 ppr (TCross a b) = ppr a <+> text "*" <+> ppr b


instance (Pretty v) => Pretty (Maybe (Type v)) where
 ppr (Just t)     = ppr t
 ppr Nothing      = text "(no type)"

instance (Pretty v) => Pretty (Scope v) where
 ppr (EVar v t)   = ppr v <+> text ":" <+> ppr t
 ppr (EUnify v)   = text "∀" <> ppr v
 ppr (ERigid v)   = text "∃" <> ppr v

{-
instance (Pretty v) => Pretty (Env v) where
 ppr = hcat . map ppr
-}

instance (Pretty v) => Pretty (Scheme v) where
 ppr (Scheme foralls exists from to)
  =   text "∀" <> hcat (map ppr foralls) <> text ". "
  <+> text "∃" <> hcat (map ppr exists)  <> text ". "
  <+> tupled (map tppr from) <+> text "→"
  <+> tupled (map tppr to)
  where
   tppr (v,t) = ppr v <+> text ":" <+> ppr t


