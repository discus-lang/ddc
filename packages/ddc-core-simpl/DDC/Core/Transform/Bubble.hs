
-- | Bubble casts outwards.
--   We float casts up and outwards so they are just inside the inner-most
--   enclosing let. This way the functions still have the same effect and
--   closure, but the casts don't get in the way of subsequent transforms.
--
--   TODO: also merge duplicate effect and closure weakenings.
module DDC.Core.Transform.Bubble
        ( bubbleModule
        , bubbleX)
where
import DDC.Core.Module
import DDC.Core.Exp
import DDC.Core.Collect
import DDC.Core.Compounds
import DDC.Core.Transform.LiftX
import DDC.Type.Compounds
import qualified DDC.Type.Env   as Env
import qualified Data.Set       as Set
import Data.Set                 (Set)
import Data.List


-- | Bubble casts outwards in a `Module`.
bubbleModule 
        :: Ord n 
        => Module a n -> Module a n

bubbleModule mm@ModuleCore{}
 = let tenv     = moduleTypeEnv mm
   in  mm { moduleBody = bubbleX tenv (moduleBody mm) }


-- | Bubble casts outwards in an `Exp`.
bubbleX :: Ord n => TypeEnv n -> Exp a n -> Exp a n
bubbleX tenv x
 = let  (cs, x')        = bubble tenv x
        Just a          = takeAnnotOfExp x'
   in   dropAllCasts tenv a cs x'


-- | Bubble casts outwards.
class Bubble (c :: * -> * -> *) where
 bubble :: Ord n => TypeEnv n -> c a n -> ([FvsCast a n], c a n)


instance Bubble Exp where
 bubble tenv xx
  = case Trim.trimX xx of
        XVar{}  -> ([], xx)
        XCon{}  -> ([], xx)

        -- Drop casts before we leave lambda abstractions, because the
        -- function type depends on the effect and closure of the body.
        -- The cast could also reference the bound variable.
        XLAM a b x
         -> let (cs, x')        = bubble tenv x
            in  ([], XLAM a b (dropAllCasts tenv a cs x'))

        XLam a b x
         -> let tenv'           = Env.extend b tenv
                (cs, x')        = bubble tenv' x
            in  ([], XLam a b (dropAllCasts tenv a cs x'))

        XApp a x1 x2
         -> let (cs1, x1')      = bubble tenv x1
                (cs2, x2')      = bubble tenv x2
            in  (cs1 ++ cs2, XApp a x1' x2')

        XLet a lts x2
         -> let (cs1, lts')     = bubble tenv lts
                (bs1, bs0)      = bindsOfLets lts
                tenv'           = Env.extends bs0 tenv
                (cs2, x2')      = bubble tenv' x2
                (cs2', x2'')    = dropCasts tenv a bs1 bs0 cs2 x2'
            in  ( cs1 ++ cs2'
                , XLet a lts' x2'')

        XCase a x alts
         -> let (cs, x')        = bubble tenv x
                (css, alts')    = unzip $ map (bubble tenv) alts
            in  ( cs ++ concat css
                , XCase a x' alts')

        -- Strip of cast and pass it up.
        XCast _ c x
         -> let (cs, x')        = bubble tenv x
                fvsT            = freeT Env.empty c
                fvsX            = freeX Env.empty c
                fc              = FvsCast c fvsT fvsX
            in  (fc : cs, x')

        XType{}         -> ([], xx)
        XWitness{}      -> ([], xx)


instance Bubble Lets where
 bubble tenv lts
  = case lts of

        -- Drop casts that mention the bound variable here, 
        -- but we can float the others further outwards.
        LLet m b x
         -> let (cs, x')        = bubble tenv x
                Just a          = takeAnnotOfExp x'
                (cs', xc')      = dropCasts tenv a [] [b] cs x'
            in  (cs', LLet m b xc')

        -- TODO: Bubble casts out of recursive lets.
        LRec bxs
         -> let bs              = map fst bxs
                tenv'           = Env.extends bs tenv

                bubbleRec (b, x)
                 = let  (cs, x') = bubble tenv' x
                        Just a   = takeAnnotOfExp x'
                   in   (b, dropAllCasts tenv a cs x')

                bxs'            = map bubbleRec bxs

            in  ([], LRec bxs')

        LLetRegions{}           -> ([], lts)
        LWithRegion{}           -> ([], lts)


instance Bubble Alt where

 -- Default patterns don't bind variables, 
 -- so there is no problem floating casts outwards.
 bubble tenv (AAlt PDefault x)
  = let (cs, x') = bubble tenv x
    in  (cs, AAlt PDefault x')

 -- Drop casts before we leave the alt because they could contain
 -- variables bound by the pattern.
 bubble tenv (AAlt p x)
  = let bs              = bindsOfPat p
        Just a          = takeAnnotOfExp x'
        tenv'           = Env.extends bs tenv
        (cs, x')        = bubble tenv' x
        (csUp, xcHere)  = dropCasts tenv a [] bs cs x'
    in  (csUp, AAlt p xcHere)


-- FvsCast --------------------------------------------------------------------
-- | A Cast along with its free level-1 and level-0 vars.
--   When we first build a `FvsCast` we record its free variables, 
--   so that we don't have to keep recomputing them.
data FvsCast a n
        = FvsCast (Cast a n)
                  (Set (Bound n))
                  (Set (Bound n))

instance Ord n => MapBoundX (FvsCast a) n where
 mapBoundAtDepthX f d (FvsCast c fvs1 fvs0)
  = FvsCast (mapBoundAtDepthX f d c)
            fvs1
            (Set.fromList
                $ map (mapBoundAtDepthX f d)
                $ Set.toList fvs0)


packFvsCasts 
        :: Ord n 
        => TypeEnv n
        -> a 
        -> [FvsCast a n] -> [Cast a n]

packFvsCasts tenv a fvsCasts
        = packCasts tenv a [ c | FvsCast c _ _ <- fvsCasts ]


-- | Pack down casts by combining multiple 'weakclo' and 'weakeff' casts
--   together. We pack casts just before we drop them, so that the resulting
--   code is easier to read.
packCasts :: Ord n => TypeEnv n -> a -> [Cast a n] -> [Cast a n]
packCasts _tenv a vs
 = let  collect weakEffs weakClos others cc
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

   in   (if null effs 
                then []
                else [CastWeakenEffect  (TSum $ Sum.fromList kEffect effs)])
     ++ (if null xsClos
                then []
                else [CastWeakenClosure (packWeakenClosureXs a xsClos)])
     ++ csOthers


-- | Pack the expressions given to a `WeakenClosure` to just the ones that we
--   care about. We only need region variables, and value variables with 
--   open types.
packWeakenClosureXs :: Ord n => a -> [Exp a n] -> [Exp a n]
packWeakenClosureXs a xx
 = let  eat fvs1 fvs0 []
         = (fvs1, fvs0)

        eat fvs1 fvs0 (x : xs)
         = let  fvs1'   = freeT Env.empty x
                fvs0'   = freeX Env.empty x
           in   eat (Set.union fvs1 fvs1') (Set.union fvs0 fvs0') xs

        (vs1, vs0)      = eat Set.empty Set.empty xx

   in   [XType (TVar v) | v <- Set.toList vs1]
     ++ [XVar a v       | v <- Set.toList vs0]



-- Dropping -------------------------------------------------------------------
-- | Wrap the provided expression with these casts.
dropAllCasts 
        :: Ord n 
        => TypeEnv n
        -> a 
        -> [FvsCast a n] -> Exp a n 
        -> Exp a n

dropAllCasts tenv a cs x
 = let  cs'     = packFvsCasts tenv a cs
   in   foldr (XCast a) x cs'


-- | Split the provided casts into ones that contain variables
--   bound by these binders. The casts that do are used to wrap
--   the provided expression, and the casts that don't are returned
--   seprately so we can keep bubbling them up the tree.
dropCasts 
        :: Ord n
        => TypeEnv n
        -> a 
        -> [Bind n]             -- ^ Level-1 binders.
        -> [Bind n]             -- ^ Level-0 binders.
        -> [FvsCast a n] 
        -> Exp a n 
        -> ([FvsCast a n], Exp a n)

dropCasts tenv a  bs1 bs0 cs x
 = let  (csHere1, cs1)    = partition (fvsCastUsesBinds1 bs1) cs
        (csHere0, csUp)   = partition (fvsCastUsesBinds0 bs0) cs1
        csHere            = packFvsCasts tenv a $ csHere1 ++ csHere0
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

