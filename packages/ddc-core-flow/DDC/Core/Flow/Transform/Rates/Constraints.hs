module DDC.Core.Flow.Transform.Rates.Constraints
        ( Constraint(..)
        , ConstraintMap, EquivClass
        , canonName
        , checkBindConstraints
        , getMaxSize )
where
import DDC.Core.Flow.Compounds
import DDC.Core.Flow.Prim
import DDC.Core.Flow.Exp
import DDC.Core.Flow.Transform.Rates.Fail
import Control.Monad
import qualified Data.Map               as Map
import qualified Data.Set               as Set



-- | Constraint information
-- An equal can have multiple - eg map3
-- Filtered only has its source input
data Constraint
 = ConEqual     [Name]
 | ConFiltered   Name
 deriving (Eq,Show)

type ConstraintMap = Map.Map Name Constraint
type EquivClass    = [Set.Set Name]


-- | Get canonical name for given equivalence class
-- Return original if there is none
-- (for example, a filter with no maps applied would have none since equiv classes are only built from maps)
canonName :: EquivClass -> Name -> Name
canonName equivs n
 = case equivSet equivs n of
    Nothing -> n
    Just s  -> Set.findMin s


-- | Get set of associated names in given equivalence class
equivSet :: EquivClass -> Name -> Maybe (Set.Set Name)
equivSet equivs n = go equivs
 where
  -- No classes left, not found
  go []
   = Nothing

  -- If @n@ is a member of this class, return it
  go (c:cs')
   | Set.member n c
   = Just c

   -- Check the rest 
   | otherwise
   = go cs'


-- | Check constraints for a single function body's bindings.
-- The bindings must be in a-normal form.
checkBindConstraints :: [(Name,ExpF)] -> LogFailures (ConstraintMap, EquivClass)
checkBindConstraints binds
 = -- Generate all constraints
   let constrs       = getConstraints binds
   -- Squash down eqs into equivalence classes
       equivs        = equivConstrs   constrs
   -- Get filter constraints as pairs
       filts         = filterConstrs  constrs equivs

   -- Check for ill-formed constraints:
   --      Filter "a <= a" is bad, as restricts to a=a
   --      Filter "a <= b" and "a <= c" is bad because 'a' mentioned twice in lhs
   in   checkFilters filts >> return (constrs, equivs)


getMaxSize :: ConstraintMap -> EquivClass -> [Name] -> Name -> Name
getMaxSize constrs equivs mans get
 = let get' = upFiltered get
   in  getFromMans get'
 where
  -- Keep moving up through filtered constraints until we hit the top
  upFiltered g
   | Just eqs <- equivSet equivs g
   = upFiltered' g (Set.toList eqs)
   | otherwise
   = g

  upFiltered' g []
   = g
  upFiltered' g (e:es)
   | Just (ConFiltered g') <- Map.lookup e constrs
   = upFiltered g'
   | otherwise
   = upFiltered' g es

  -- Find a manifest vector in the same equivalence class
  getFromMans g
   = let e = canonName equivs g
     in  getFromMans' e mans

  getFromMans' g []
   = g
  getFromMans' g (m:ms)
   | g == canonName equivs m
   = m
   | otherwise
   = getFromMans' g ms
   
 

-- | Squash constraints into equivalence classes
-- I'm sure this could be smarter.
equivConstrs :: ConstraintMap -> EquivClass
equivConstrs m
 = let sets = filter (not . Set.null)
            $ map gen
            $ Map.toList m
   in  squash sets []
 where
  -- Simply generate a set from each constraint
  gen (k, (ConEqual eqs))
   = Set.fromList (k:eqs)
  -- Ignore filter constraints
  gen (k, (ConFiltered _from))
   = Set.singleton k

  -- Squash constraint sets together
  squash []     acc
   = acc

  squash (a:as) acc
   -- Try to merge the @a@ set into @acc@ somewhere
   -- If so, start merging the whole thing again
   | Just merged <- squash_merge a acc
   = squash (merged ++ as) []

   -- Nothing in @a@ is mentioned in @acc@, so no merging required:
   --   just add this set to the accumulator
   | otherwise
   = squash as (a:acc)

  squash_merge ins (s:ss)
   -- Check if any members of @ins@ are mentioned in @s@
   -- If so, merge them into one equivalence class
   | not $ Set.null $ ins `Set.intersection` s
   = Just (ins `Set.union` s : ss)

   -- Check if there is a chance to merge later
   | Just ss' <- squash_merge ins ss
   = Just (s : ss')

  -- No merge is possible
  squash_merge _ins _ss
   = Nothing


-- Get canonical names of all filter constraints
filterConstrs :: ConstraintMap -> EquivClass -> [(Name,Name, Name, Name)]
filterConstrs m equivs = Map.foldWithKey go [] m
 where
  go k (ConFiltered src) ms
   = (canonName equivs k, canonName equivs src, k, src) : ms
  go _  _                ms
   = ms


-- | Generate constraints map from bindings
getConstraints :: [(Name,ExpF)] -> ConstraintMap
getConstraints lets
 = foldl go Map.empty lets
 where
  go m (n,x)
   | Just (n',c) <- getConstraint n x 
   = Map.insert n' c m
   | otherwise
   = m

getConstraint :: Name -> ExpF -> Maybe (Name, Constraint)
getConstraint n xx
 | Just (f, args)                   <- takeXApps xx
 , XVar (UPrim (NameOpVector ov) _) <- f
 = case ov of
   OpVectorMap i
    -- Args:
    -- map1 :: [a b   : *]. (a -> b)      -> Vector a -> Vector b
    -- (drop 3)
    -- map2 :: [a b c : *]. (a -> b -> c) -> Vector a -> Vector b -> Vector c
    -- (drop 4)
    | vecs         <- drop (i+2) args
    -- Must be fully applied
    , length vecs  == i
    , names        <- getNames vecs
    -- Each name must also be a bound variable
    , length names == i
    -> Just (n, ConEqual names)

   OpVectorFilter
    | [_tyA, _p, XVar (UName vec)] <- args
    -> Just (n, ConFiltered vec)

   OpVectorGenerate
   -- Not really sure about this
    -> Just (n, ConEqual [])

   OpVectorReduce
    | [_tyA, _f, _z, XVar (UName vec)] <- args
    -> Just (n, ConEqual [vec])

   OpVectorLength
    | [_tyA, XVar (UName vec)] <- args
    -> Just (n, ConEqual [vec])

   _
    -> Nothing

 | otherwise
 = Nothing

-- | Get bound name for each expression
-- All expressions must be variables of bound names,
-- otherwise result list will be shorter than input.
getNames :: [ExpF] -> [Name]
getNames vs
 = concatMap get vs
 where
  get x
   | XVar (UName v) <- x
   = [v]
   | otherwise
   = []


-- | Check for ill-formed constraints:
--      Filter "a <= a" is bad, as restricts to a=a
--      Filter "a <= b" and "a <= c" is bad because 'a' mentioned twice in lhs
-- For some filter
-- > bs = filter p as
-- the arguments are
-- > (canon bs, canon as,  bs, as)
-- the 'raw' variable names bs and as are only used for error messages;
-- comparisons are done on canonical names.
checkFilters :: [(Name,Name, Name,Name)] -> LogFailures ()
checkFilters cs
 = go cs
 where
  go []
   = return ()
  go ((lc,rc, ln, rn):cs')
   = do when (lc == rc) $
          warn $ FailConstraintFilteredLessFiltered ln rn
        -- Check against later ones
        forM_ cs' $ \(lc', _, ln', _) ->
          when (lc == lc') $
            warn $ FailConstraintFilteredNotUnique  ln ln'

        go cs'


{-

f = \(as : Vector a).
    as'    = vmap [:a b:] g as
    return as'

==>
[as=as']
==>

f = \(as : Vector a).
    runSeries as /\(k1 : Rate). \(asS : Series k1 a).
    as'    = valloc [:k1 b:]
    as'S   = smap   [:k1 a b:] g asS
    sfill [:k1 b:] as' as'S
    return as'

---

f = \(as : Vector a).
    as'    = vmap [:a b:] g as
    as''   = vmap [:b b:] h as'
    return as''

==>
[as = as' = as'']
==>

f = \(as : Vector a).
    runSeries as /\(k1 : Rate). \(asS : Series k1 a).
    as'S   = smap   [:k1 a b:] g asS
    as''   = valloc [:k1 c:]
    as''S  = smap   [:k1 b c:] h as'S
    sfill [:k1 b:] as'' as''S
    return as''

---

f = \(as : Vector a).
    as' = filter p as
    n   = length   as'
    ns  = map (/n) as'

==>
[as' <= as
,ns   = as']
==>

f = \(as : Vector a).
    runSeries as /\(k1 : Rate). \(asS : Series k1 a).
    as'F = smap [:k1 a Bool:] p asS
    mkSel [:k1:] as'F /\(k2 : Rate). \(as'Se : Sel k1 k2).
    as'S = spack   [:k1 k2 a:] as'Se asS
    n    = slength [:k2:]
    nsS  = smap    [:k2 a a:] (/n) as'S
    ns   = valloc  [:k2 a:]
    sfill [:k2 a:] ns nsS

    return ns

---

f = \(as : Vector a).
    bs = filter p as
    cs = map2   f as bs
    return cs

==>
[bs <= as
,cs = as = bs]
==>
[as <= as]
Error!

---

f = \(as bs : Vector a).
    cs = filter p as
    ds = filter p bs
    es = map2   f cs ds
    return es

==>
[cs <= as
,ds <= bs
,cs=ds=es]
==>
[cs <= as
,cs <= bs]
Error, cs mentioned twice in lhs!
-}

