-- | Create substitution to make (subst template) == target
module DDC.Core.Transform.Rewrite.Match
        ( -- * Substitutions
          SubstInfo
        , emptySubstInfo 

          -- * Matching
        , match)
where
import DDC.Core.Exp
import DDC.Type.Transform.Crush
import DDC.Type.Bind
import DDC.Type.Compounds
import Data.Set                                 (Set)
import Data.Map                                 (Map)
import qualified DDC.Type.Sum                   as Sum
import qualified DDC.Type.Transform.AnonymizeT	as T
import qualified DDC.Core.Transform.AnonymizeX	as T
import qualified DDC.Core.Transform.Reannotate	as T
import qualified DDC.Type.Equiv			as TE
import qualified Data.Map			as Map
import qualified Data.Set			as Set


-------------------------------------------------------------------------------
-- | Value and type substition.
type SubstInfo a n 
        = (Map n (Exp a n), Map n (Type n))

-- | An empty substition info.
emptySubstInfo :: SubstInfo a n
emptySubstInfo 
        = (Map.empty, Map.empty)

lookupx n (xs,_) 
        = Map.lookup n xs

insertx n x (xs,tys) 
        = (Map.insert n x xs, tys)


-- Match Exp ------------------------------------------------------------------
-- | Create substitution to make (subst template) == target
--   Does not handle higher-order templates (ie ones with binders)
--
--  @ match emptySubstInfo (Set.fromList [r1, r2, s])
--   (stream [r1]  (unstream [r2]  s))
--   (stream [R0#] (unstream [R1#] (someStream 23))
--
--   => { r1 |-> R0#, r2 |-> R1, s |-> someStream 23 }
--  @
match   :: (Show a, Show n, Ord n)
        => SubstInfo a n        -- ^ Current substitution
        -> Set n                -- ^ Variables we're interested in
        -> Exp a n              -- ^ Template expression.
        -> Exp a n              -- ^ Target expression.
        -> Maybe (SubstInfo a n)

-- Variables bound by the rule: restricted to just UName earlier.
match m bs (XVar _ (UName n)) r
 | n `Set.member` bs
 -- Check if it's already been matched
 = case lookupx n m of
   Nothing -> return $ insertx n r m
   Just x  
    ->  -- Check if they're equal. Anonymize so names don't matter.
	-- Reannotate so annotations are ignored.
	let  x' = T.anonymizeX $ T.reannotate (const ()) x
	     r' = T.anonymizeX $ T.reannotate (const ()) r
	in if   x' == r'
	   then Just m
	   else Nothing

match m _ (XVar _ v1) (XVar _ v2)
 | v1 == v2      = Just m

match m _ (XCon _ c1) (XCon _ c2)
 | c1 == c2      = Just m

match m bs (XApp _ x11 x12) (XApp _ x21 x22)
 = do	m' <- match m bs x11 x21
	match m' bs x12 x22

match m bs (XCast _ c1 x1) (XCast _ c2 x2)
 | eqCast c1 c2	= match m bs x1 x2

match (xs, tys) bs (XType t1) (XType t2)
 = do	tys' <- matchT bs tys t1 t2
	return (xs, tys')

match m _ (XWitness w1) (XWitness w2)
 | eqWit w1 w2	= return m

match _ _ _ _ 
 = Nothing


eqCast lc rc 
 = clean lc == clean rc
 where  clean c 
         = case c of
                CastWeakenEffect  eff -> CastWeakenEffect  $ T.anonymizeT eff
                CastWeakenClosure clo -> CastWeakenClosure $ map cleanX clo
                CastPurify	      wit -> CastPurify wit
                CastForget	      wit -> CastForget wit

        cleanX 
         = T.anonymizeX . T.reannotate (const ())

eqWit lw rw 
        = lw == rw

-- Types ----------------------------------------------------------------------
type VarSet n = Set.Set n
type Subst n  = Map.Map n (Type n)


-- | Try to find a simple substitution between two types.
--   Ignoring complicated effect sums.
--   Eg given template @a -> b@ and target @Int -> Float@,
--   returns substitution:
--      @{ a |-> Int, b |-> Float }@
--
matchT  :: Ord n
        => VarSet n     -- ^ Only attempt to match these names.
        -> Subst n      -- ^ Already matched (or @Map.empty@)
        -> Type n       -- ^ Template type.
        -> Type n       -- ^ Target type.
        -> Maybe (Subst n)

matchT vs subst t1 t2
        = matchT' [] [] t1 t2 vs subst


matchT' :: Ord n
        => [Bind n]
        -> [Bind n]
        -> Type n   -> Type n
        -> VarSet n -> Subst n
        -> Maybe (Subst n)

matchT' stack1 stack2 t1 t2 vs subst
 = let  t1'     = unpackSumT $ crushSomeT t1
        t2'     = unpackSumT $ crushSomeT t2
   in case (t1', t2') of
        (TVar u1,         TVar u2)
         -- If variables are bound in foralls, no need to match.
         -- Don't check their names - lookup bind depth instead.
         -- 
         -- I was calling equivT' here, but changing to matchT':
         -- perhaps if we had
         --     RULE [a : **] [b : a]. something [b] ...
         -- then matching against
         --     let i = Int in
         --             something [i]
         -- so to find a, we need to find i's type.
         | Just (ix1, t1a)   <- getBindType stack1 u1
         , Just (ix2, t2a)   <- getBindType stack2 u2
         , ix1 == ix2
         -> matchT' stack1 stack2 t1a t2a vs subst

        -- Constructor names must be equal.
        --
        -- Will this still work when it's a TyConBound - basically same as TVar?
        (TCon tc1,        TCon tc2)
         | tc1 == tc2
         -> Just subst

        -- Push binders on the stack as we enter foralls.
        (TForall b11 t12, TForall b21 t22)
         -> do
                subst' <- matchT' stack1 stack2 (typeOfBind b11) (typeOfBind b21) vs subst
                matchT' (b11 : stack1)
                        (b21 : stack2)
                        t12 t22
                        vs subst'

        -- Decend into applications.
        (TApp t11 t12,    TApp t21 t22)
         -> do
                subst' <- matchT' stack1 stack2 t11 t21 vs subst
                matchT' stack1 stack2 t12 t22 vs subst'
        

        -- ISSUE #279: Rewrite rule type matcher doesn't handle type sums
        --  Unifying two arbitrary sums is probably too hard, but we could handle
        --  simple specific cases, like matching a single variable against a sum,
        --  or when the two sums share the same components.
        --
        --  Also fix the other TODOs in this function.
        (TSum _,        TSum _)
         -> Just subst

        {-
        -- Sums are equivalent if all of their components are.
        (TSum ts1,        TSum ts2)
         -> let ts1'      = Sum.toList ts1
                ts2'      = Sum.toList ts2
                equiv     = equivT' stack1 depth1 stack2 depth2

                -- If all the components of the sum were in the element
                -- arrays then they come out of Sum.toList sorted
                -- and we can compare corresponding pairs.
                checkFast = and $ zipWith equiv ts1' ts2'

                -- If any of the components use a higher kinded type variable
                -- like (c : % ~> !) then they won't nessesarally be sorted,
                -- so we need to do this slower O(n^2) check.
                checkSlow = and [ or (map (equiv t1c) ts2') | t1c <- ts1' ]
                         && and [ or (map (equiv t2c) ts1') | t2c <- ts2' ]

            in  (length ts1' == length ts2')
            &&  (checkFast || checkSlow)
            -}

        -- If template is in variable set, push the target into substitution
        -- But we might need to rename bound variables...
        (TVar (UName n), _)

        -- TODO rewrite binders from t2 to t1 in t2'
         | Set.member n vs
         , Nothing <- Map.lookup n subst
         -> Just $ Map.insert n t2' subst

         | Set.member n vs
         , Just t1'' <- Map.lookup n subst
         , TE.equivWithBindsT stack1 stack2 t1'' t2'
         -> Just subst

        (_, _)  -> Nothing


-- | Unpack single element sums into plain types.
unpackSumT :: Type n -> Type n
unpackSumT (TSum ts)
        | [t]   <- Sum.toList ts = t
unpackSumT tt                    = tt

