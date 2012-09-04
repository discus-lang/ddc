
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
import qualified DDC.Core.Transform.Trim        as Trim
import qualified DDC.Type.Env                   as Env
import qualified Data.Set                       as Set
import Data.Set                 (Set)
import Data.List


-- | Bubble casts outwards in a `Module`.
bubbleModule :: Ord n => Module a n -> Module a n
bubbleModule mm@ModuleCore{}
        = mm { moduleBody = bubbleX (moduleBody mm) }


-- | Bubble casts outwards in an `Exp`.
bubbleX :: Ord n => Exp a n -> Exp a n
bubbleX x
 = let  (cs, x')        = bubble x
        Just a          = takeAnnotOfExp x'
   in   dropAllCasts a cs x'


-- | Bubble casts outwards.
class Bubble (c :: * -> * -> *) where
 bubble :: Ord n => c a n -> ([FvsCast a n], c a n)


instance Bubble Exp where
 bubble xx
  = case Trim.trimX xx of
        XVar{}  -> ([], xx)
        XCon{}  -> ([], xx)

        -- Drop casts before we leave lambda abstractions, because the
        -- function type depends on the effect and closure of the body.
        -- The cast could also reference the bound variable.
        XLAM a b x
         -> let (cs, x')        = bubble x
            in  ([], XLAM a b (dropAllCasts a cs x'))

        XLam a b x
         -> let (cs, x')        = bubble x
            in  ([], XLam a b (dropAllCasts a cs x'))

        XApp a x1 x2
         -> let (cs1, x1')      = bubble x1
                (cs2, x2')      = bubble x2
            in  (cs1 ++ cs2, XApp a x1' x2')

        XLet a lts x2
         -> let (cs1, lts')     = bubble lts
                (cs2, x2')      = bubble x2
                (bs1, bs2)      = bindsOfLets lts
                (cs2', x2'')    = dropCasts a bs1 bs2 cs2 x2'
            in  ( cs1 ++ cs2'
                , XLet a lts' x2'')

        XCase a x alts
         -> let (cs, x')        = bubble x
                (css, alts')    = unzip $ map bubble alts
            in  ( cs ++ concat css
                , XCase a x' alts')

        -- Strip of cast and pass it up.
        XCast _ c x
         -> let (cs, x')        = bubble x
                fvsT            = freeT Env.empty c
                fvsX            = freeX Env.empty c
                fc              = FvsCast c fvsT fvsX
            in  (fc : cs, x')

        XType{}         -> ([], xx)
        XWitness{}      -> ([], xx)


instance Bubble Lets where
 bubble lts
  = case lts of

        -- Drop casts that mention the bound variable here, 
        -- but we can float the others further outwards.
        LLet m b x
         -> let (cs, x')        = bubble x
                Just a          = takeAnnotOfExp x'
                (cs', xc')      = dropCasts a [] [b] cs x'
            in  (cs', LLet m b xc')

        -- TODO: Bubble casts out of recursive lets.
        LRec bxs
         -> let bubbleRec (b, x)
                 = let  (cs, x') = bubble x
                        Just a   = takeAnnotOfExp x'
                   in   (b, dropAllCasts a cs x')

                bxs'            = map bubbleRec bxs

            in  ([], LRec bxs')

        LLetRegions{}           -> ([], lts)
        LWithRegion{}           -> ([], lts)


instance Bubble Alt where

 -- Default patterns don't bind variables, 
 -- so there is no problem floating casts outwards.
 bubble (AAlt PDefault x)
  = let (cs, x') = bubble x
    in  (cs, AAlt PDefault x')

 -- Drop casts before we leave the alt because they could contain
 -- variables bound by the pattern.
 bubble (AAlt p x)
  = let (cs, x')        = bubble x
        bs              = bindsOfPat p
        Just a          = takeAnnotOfExp x'
        (csUp, xcHere)  = dropCasts a [] bs cs x'
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


-- Dropping -------------------------------------------------------------------
-- | Wrap the provided expression with these casts.
dropAllCasts 
        :: a 
        -> [FvsCast a n] -> Exp a n 
        -> Exp a n

dropAllCasts a cs x
        = foldr (makeFvsCast a) x cs


-- | Split the provided casts into ones that contain variables
--   bound by these binders. The casts that do are used to wrap
--   the provided expression, and the casts that don't are returned
--   seprately so we can keep bubbling them up the tree.
dropCasts 
        :: Ord n
        => a 
        -> [Bind n]             -- ^ Level-1 binders.
        -> [Bind n]             -- ^ Level-0 binders.
        -> [FvsCast a n] 
        -> Exp a n 
        -> ([FvsCast a n], Exp a n)

dropCasts a bs1 bs0 cs x
 = let  (csHere1, cs1)    = partition (fvsCastUsesBinds1 bs1) cs
        (csHere0, csUp)   = partition (fvsCastUsesBinds0 bs0) cs1
   in   ( map (lowerX 1) csUp
        , foldr (makeFvsCast a) x (csHere1 ++ csHere0))


-- | Convert a `FvsCast` to a plain `Cast` by erasing the free variables sets.
makeFvsCast :: a -> FvsCast a n -> Exp a n -> Exp a n
makeFvsCast a (FvsCast c _ _) x
        = XCast a c x


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

