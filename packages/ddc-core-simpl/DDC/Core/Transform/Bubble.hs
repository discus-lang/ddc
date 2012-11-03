
-- | Bubble casts outwards.
--   We float casts up and outwards so they are just inside the inner-most
--   enclosing let. This way the functions still have the same effect and
--   closure, but the casts don't get in the way of subsequent transforms.
--
module DDC.Core.Transform.Bubble
        ( bubbleModule
        , bubbleX)
where
import DDC.Core.Collect
import DDC.Core.Collect.Support
import DDC.Core.Transform.LiftX
import DDC.Core.Predicates
import DDC.Core.Compounds
import DDC.Core.Module
import DDC.Core.Exp
import DDC.Type.Env                             (KindEnv, TypeEnv)
import qualified DDC.Type.Env                   as Env
import qualified DDC.Type.Sum                   as Sum
import qualified Data.Set                       as Set
import Data.Set                                 (Set)
import Data.List


-- | Bubble casts outwards in a `Module`.
bubbleModule 
        :: Ord n
        => Module a n -> Module a n

bubbleModule mm@ModuleCore{}
 = let  kenv    = moduleKindEnv mm
        tenv    = moduleTypeEnv mm
   in   mm { moduleBody = bubbleX kenv tenv (moduleBody mm) }


-- | Bubble casts outwards in an `Exp`.
bubbleX :: Ord n
        => KindEnv n -> TypeEnv n -> Exp a n -> Exp a n
bubbleX kenv tenv x
 = let  
        -- Bubble the expression, yielding any casts that floated out
        -- to top-level.
        (cs, x')        = bubble kenv tenv x
        Just a          = takeAnnotOfExp x'

        -- Reattach top-level casts.
   in   dropAllCasts kenv tenv a cs x'


-------------------------------------------------------------------------------
-- | Bubble casts outwards in some thing.
class Bubble (c :: * -> * -> *) where
 bubble :: Ord n
        => KindEnv n
        -> TypeEnv n
        -> c a n 
        -> ([FvsCast a n], c a n)


instance Bubble Exp where
 bubble kenv tenv xx
  = case xx of
        XVar{}  -> ([], xx)
        XCon{}  -> ([], xx)

        -- Drop casts before we leave lambda abstractions, because the
        -- function type depends on the effect and closure of the body.
        -- The cast could also reference the bound variable.
        XLAM a b x
         -> let kenv'           = Env.extend b kenv
                (cs, x')        = bubble kenv' tenv x
            in  ([], XLAM a b (dropAllCasts kenv' tenv a cs x'))

        XLam a b x
         -> let tenv'           = Env.extend b tenv
                (cs, x')        = bubble kenv tenv' x
            in  ([], XLam a b (dropAllCasts kenv tenv' a cs x'))

        -- Decend into applications.
        XApp a x1 x2
         -> let (cs1, x1')      = bubble kenv tenv x1
                (cs2, x2')      = bubble kenv tenv x2
            in  (cs1 ++ cs2, XApp a x1' x2')

        -- After decending into let-expressions, make sure to drop
        -- any casts that mention variables bound here.
        XLet a lts x2
         -> let (cs1, lts')     = bubble kenv tenv lts
                (bs1, bs0)      = bindsOfLets lts
                kenv'           = Env.extends bs1 kenv
                tenv'           = Env.extends bs0 tenv
                (cs2, x2')      = bubble kenv' tenv' x2
                (cs2', x2'')    = dropCasts kenv' tenv' a bs1 bs0 cs2 x2'
            in  ( cs1 ++ cs2'
                , XLet a lts' x2'')

        -- Decent into case-expressions.
        --  Casts that depend on bound variables are dropped 
        --  in the corresponding alternatives.
        XCase a x alts
         -> let (cs, x')        = bubble kenv tenv x
                (css, alts')    = unzip $ map (bubble kenv tenv) alts
            in  ( cs ++ concat css
                , XCase a x' alts')

        -- Strip of cast and pass it up.
        XCast _ c x
         -> let (cs, x')        = bubble kenv tenv x
                fvsT            = freeT Env.empty c
                fvsX            = freeX Env.empty c
                fc              = FvsCast c fvsT fvsX
            in  (fc : cs, x')

        XType{}         -> ([], xx)
        XWitness{}      -> ([], xx)


instance Bubble Lets where
 bubble kenv tenv lts
  = case lts of

        -- Drop casts that mention the bound variable here, 
        -- but we can float the others further outwards.
        LLet m b x
         -> let (cs, x')        = bubble kenv tenv x
                Just a          = takeAnnotOfExp x'
                (cs', xc')      = dropCasts kenv tenv a [] [b] cs x'
            in  (cs', LLet m b xc')

        -- TODO: Bubble casts out of recursive lets.
        LRec bxs
         -> let bs              = map fst bxs
                tenv'           = Env.extends bs tenv

                bubbleRec (b, x)
                 = let  (cs, x') = bubble kenv tenv' x
                        Just a   = takeAnnotOfExp x'
                   in   (b, dropAllCasts kenv tenv' a cs x')

                bxs'            = map bubbleRec bxs

            in  ([], LRec bxs')

        LLetRegions{}           -> ([], lts)
        LWithRegion{}           -> ([], lts)


instance Bubble Alt where

 -- Default patterns don't bind variables, 
 -- so there is no problem floating casts outwards.
 bubble kenv tenv (AAlt PDefault x)
  = let (cs, x') = bubble kenv tenv x
    in  (cs, AAlt PDefault x')

 -- Drop casts before we leave the alt because they could contain
 -- variables bound by the pattern.
 bubble kenv tenv (AAlt p x)
  = let bs              = bindsOfPat p
        Just a          = takeAnnotOfExp x'
        tenv'           = Env.extends bs tenv
        (cs, x')        = bubble kenv tenv' x
        (csUp, xcHere)  = dropCasts kenv tenv' a [] bs cs x'
    in  (csUp, AAlt p xcHere)


-- FvsCast --------------------------------------------------------------------
-- | A Cast along with its free level-1 and level-0 vars.
--   When we first build a `FvsCast` we record its free variables, 
--   so that we don't have to keep recomputing them.
data FvsCast a n
        = FvsCast (Cast a n)
                  (Set (Bound n))       -- Free level-1 variables.
                  (Set (Bound n))       -- Free level-0 variables.

instance Ord n => MapBoundX (FvsCast a) n where
 mapBoundAtDepthX f d (FvsCast c fvs1 fvs0)
  = FvsCast (mapBoundAtDepthX f d c)
            fvs1
            (Set.fromList
                $ map (mapBoundAtDepthX f d)
                $ Set.toList fvs0)


packFvsCasts 
        :: Ord n
        => KindEnv n -> TypeEnv n
        -> a -> [FvsCast a n] -> [Cast a n]

packFvsCasts kenv tenv a fvsCasts
        = packCasts kenv tenv a [ c | FvsCast c _ _ <- fvsCasts ]


-- | Pack down casts by combining multiple 'weakclo' and 'weakeff' casts
--   together. We pack casts just before we drop them, so that the resulting
--   code is easier to read.
packCasts :: Ord n
          => KindEnv n -> TypeEnv n -> a -> [Cast a n] -> [Cast a n]
packCasts kenv tenv a vs
 = let  
        -- Sort casts into weakeff, weakclo and any others.
        -- We'll collect the weakeff and weakclo casts together.
        collect weakEffs weakClos others cc
         = case cc of
            []                        
             -> (reverse weakEffs, reverse weakClos, reverse others)

            CastWeakenEffect eff : cs 
             -> collect (eff : weakEffs) weakClos others cs

            CastWeakenClosure xs : cs 
             -> collect weakEffs        (xs ++ weakClos) others cs

            c : cs
             -> collect weakEffs        weakClos (c : others) cs


        (effs, xsClos, csOthers) 
                = collect [] [] [] vs

        xsClos_packed
                = packWeakenClosureXs kenv tenv a xsClos

   in   (if null effs 
                then []
                else [CastWeakenEffect  (TSum $ Sum.fromList kEffect effs)])
     ++ (if null xsClos_packed
                then []
                else [CastWeakenClosure xsClos_packed])
     ++ csOthers


-- | Pack the expressions given to a `WeakenClosure` to just the ones that we
--   care about. We only need region variables, and value variables with 
--   open types.
packWeakenClosureXs 
        :: Ord n
        => KindEnv n -> TypeEnv n 
        -> a -> [Exp a n] -> [Exp a n]

packWeakenClosureXs kenv tenv a xx
 = let  
        eat fvsSp fvsDa []
         = (fvsSp, fvsDa)

        eat fvsSp fvsDa (x : xs)
         = let  sup      = support Env.empty Env.empty x
                fvsSp'   = supportSpVar sup
                fvsDa'   = supportDaVar sup
           in   eat (Set.union fvsSp fvsSp') (Set.union fvsDa fvsDa') xs

        (vsSp, vsDa)      = eat Set.empty Set.empty xx

   in   [XType (TVar u) | u <- Set.toList vsSp]
     ++ [XVar a u       | u <- Set.toList vsDa, keepBound kenv tenv u]


-- | When packing vars given to a closure weakening, we only need to keep
--   vars that have open types or contain region handles.
keepBound :: Ord n => KindEnv n -> TypeEnv n -> Bound n -> Bool
keepBound _kenv tenv u
        | Just t        <- Env.lookup u tenv
        , sup           <- support Env.empty Env.empty t
        , Set.null (supportSpVar sup)
        , all (not . isRegionHandle) $ Set.toList $ supportTyCon sup
        = False

        | otherwise
        = True 


-- | Treat primitive constructors with region kind as region handles.
--   Region handles are only really a part of the 'Disciple Core Eval' 
--   language, but they're easy to check for even if the name type 'n'
--   hasn't been revealed.
isRegionHandle :: Bound n -> Bool
isRegionHandle u
 = case u of
        UPrim _ k       -> isRegionKind k
        _               -> False


-- Dropping -------------------------------------------------------------------
-- | Wrap the provided expression with these casts.
dropAllCasts 
        :: Ord n
        => KindEnv n
        -> TypeEnv n
        -> a 
        -> [FvsCast a n] -> Exp a n 
        -> Exp a n

dropAllCasts kenv tenv a cs x
 = let  cs'     = packFvsCasts kenv tenv a cs
   in   foldr (XCast a) x cs'


-- | Split the provided casts into ones that contain variables
--   bound by these binders. The casts that do are used to wrap
--   the provided expression, and the casts that don't are returned
--   seprately so we can keep bubbling them up the tree.
dropCasts 
        :: Ord n
        => KindEnv n -> TypeEnv n
        -> a 
        -> [Bind n]             -- ^ Level-1 binders.
        -> [Bind n]             -- ^ Level-0 binders.
        -> [FvsCast a n] 
        -> Exp a n 
        -> ([FvsCast a n], Exp a n)

dropCasts kenv tenv a bs1 bs0 cs x
 = let  (csHere1, cs1)    = partition (fvsCastUsesBinds1 bs1) cs
        (csHere0, csUp)   = partition (fvsCastUsesBinds0 bs0) cs1
        csHere            = packFvsCasts kenv tenv a $ csHere1 ++ csHere0
   in   ( map (lowerX 1) csUp
        , foldr (XCast a) x csHere)


-- | Check if a `FvsCast` mentions any of these level-0 variables.
fvsCastUsesBinds0 :: Ord n => [Bind n] -> FvsCast a n -> Bool
fvsCastUsesBinds0 bb (FvsCast _ _ fvs0)
        = bindsMatchBoundSet bb fvs0


-- | Check if a `FvsCast` mentions any of these level-1 variables.
fvsCastUsesBinds1 :: Ord n => [Bind n] -> FvsCast a n -> Bool
fvsCastUsesBinds1 bb (FvsCast _ fvs1 _)
        = bindsMatchBoundSet bb fvs1


-- | Check if a set of bound variables matches any of the given binders.
bindsMatchBoundSet :: Ord n => [Bind n] -> Set (Bound n) -> Bool
bindsMatchBoundSet bb fvs
 = go bb
 where  go []           = False
        go (b : bs)
         | Just u       <- takeSubstBoundOfBind b
         = if Set.member u fvs
                then True
                else go bs

         | otherwise
         = go bs

