{-# OPTIONS_HADDOCK hide #-}
-- | Gather all bound names in a thing,
--   independent of whether they are locally bound or not.
module DDC.Core.Collect.BindTree
        ( freeT
        , freeX
        , gatherBound)
where
import DDC.Type.Compounds
import DDC.Core.Exp
import DDC.Type.Env                     (Env)
import qualified DDC.Type.Env           as Env
import qualified DDC.Type.Sum           as Sum
import qualified Data.Set               as Set
import Data.Set                         (Set)


-- freeT ----------------------------------------------------------------------
freeT   :: (BindStruct c, Ord n) 
        => Env n -> c n -> Set (Bound n)
freeT tenv xx = Set.unions $ map (freeOfTreeT tenv) $ slurpBindTree xx

freeOfTreeT :: Ord n => Env n -> BindTree n -> Set (Bound n)
freeOfTreeT kenv tt
 = case tt of
        BindDef way bs ts
         |  BoundSpec   <- boundLevelOfBindWay way
         ,  kenv'       <- Env.extends bs kenv
         -> Set.unions $ map (freeOfTreeT kenv') ts

        BindDef _ _ ts
         -> Set.unions $ map (freeOfTreeT kenv) ts

        BindUse BoundSpec u
         | Env.member u kenv -> Set.empty
         | otherwise         -> Set.singleton u
        _                    -> Set.empty


-- freeX ----------------------------------------------------------------------
freeX   :: (BindStruct c, Ord n) 
        => Env n -> c n -> Set (Bound n)
freeX tenv xx = Set.unions $ map (freeOfTreeX tenv) $ slurpBindTree xx

freeOfTreeX :: Ord n => Env n -> BindTree n -> Set (Bound n)
freeOfTreeX tenv tt
 = case tt of
        BindDef way bs ts
         |  BoundExpWit <- boundLevelOfBindWay way
         ,  tenv'       <- Env.extends bs tenv
         -> Set.unions $ map (freeOfTreeX tenv') ts

        BindDef _ _ ts
         -> Set.unions $ map (freeOfTreeX  tenv) ts

        BindUse BoundExpWit u
         | Env.member u tenv -> Set.empty
         | otherwise         -> Set.singleton u
        _                    -> Set.empty


-- gatherBound ----------------------------------------------------------------
-- | Gather all the bound variables in a thing, 
--   independent of whether they are free or not.
gatherBound :: (BindStruct c, Ord n) => c n -> Set (Bound n)
gatherBound = Set.unions . map boundsOfTree . slurpBindTree 


boundsOfTree :: Ord n => BindTree n -> Set (Bound n)
boundsOfTree tt
 = case tt of
        BindDef _ _ ts  -> Set.unions $ map boundsOfTree ts
        BindUse _ u     -> Set.singleton u
        BindCon _ u     -> Set.singleton u


-------------------------------------------------------------------------------
-- | A description of the binding structure of some type or expression.
data BindTree n
        -- | An abstract binding expression.
        = BindDef BindWay    [Bind n] [BindTree n]

        -- | Use of a variable.
        | BindUse BoundLevel (Bound n)

        -- | Use of a constructor.
        | BindCon BoundLevel (Bound n)
        deriving (Eq, Show)


-- | Describes how a variable was bound.
data BindWay
        = BindForall
        | BindLAM
        | BindLam
        | BindLet
        | BindLetRec
        | BindLetRegion
        | BindLetRegionWith
        | BindCasePat
        deriving (Eq, Show)


-- | What level this binder is at.
data BoundLevel
        = BoundSpec
        | BoundExpWit
        deriving (Eq, Show)


-- | Get the `BoundLevel` corresponding to a `BindWay`.
boundLevelOfBindWay :: BindWay -> BoundLevel
boundLevelOfBindWay way
 = case way of
        BindForall              -> BoundSpec
        BindLAM                 -> BoundSpec
        BindLam                 -> BoundExpWit
        BindLet                 -> BoundExpWit
        BindLetRec              -> BoundExpWit
        BindLetRegion           -> BoundSpec
        BindLetRegionWith       -> BoundExpWit
        BindCasePat             -> BoundExpWit


-- BindStruct -----------------------------------------------------------------
class BindStruct (c :: * -> *) where
 slurpBindTree :: c n -> [BindTree n]


instance BindStruct Type where
 slurpBindTree tt
  = case tt of
        TVar u          -> [BindUse BoundSpec u]
        TCon tc         -> slurpBindTree tc
        TForall b t     -> [bindDefT BindForall [b] [t]]
        TApp t1 t2      -> slurpBindTree t1 ++ slurpBindTree t2
        TSum ts         -> concatMap slurpBindTree $ Sum.toList ts


instance BindStruct TyCon where
 slurpBindTree tc
  = case tc of
        TyConBound u    -> [BindCon BoundSpec u]
        _               -> []


instance BindStruct (Exp a) where
 slurpBindTree xx
  = case xx of
        XVar _ u        -> [BindUse BoundExpWit u]
        XCon _ u        -> [BindCon BoundExpWit u]
        XApp _ x1 x2    -> slurpBindTree x1 ++ slurpBindTree x2
        XLAM _ b x      -> [bindDefT BindLAM [b] [x]]
        XLam _ b x      -> [bindDefX BindLam [b] [x]]      

        XLet _ (LLet m b x1) x2
         -> slurpBindTree m
         ++ slurpBindTree x1
         ++ [bindDefX BindLet [b] [x2]]

        XLet _ (LRec bxs) x2
         -> [bindDefX BindLetRec 
                     (map fst bxs) 
                     (map snd bxs ++ [x2])]
        
        XLet _ (LLetRegion b bs) x2
         -> [ BindDef  BindLetRegion [b]
             [bindDefX BindLetRegionWith bs [x2]]]

        XLet _ (LWithRegion u) x2
         -> BindUse BoundExpWit u : slurpBindTree x2

        XCase _ x alts  -> slurpBindTree x ++ concatMap slurpBindTree alts
        XCast _ c x     -> slurpBindTree c ++ slurpBindTree x
        XType t         -> slurpBindTree t
        XWitness w      -> slurpBindTree w


instance BindStruct LetMode where
 slurpBindTree mm
  = case mm of
        LetLazy (Just w) -> slurpBindTree w
        _                -> []


instance BindStruct Cast where
 slurpBindTree cc
  = case cc of
        CastWeakenEffect eff    -> slurpBindTree eff
        CastWeakenClosure clo   -> slurpBindTree clo
        CastPurify w            -> slurpBindTree w
        CastForget w            -> slurpBindTree w


instance BindStruct (Alt a) where
 slurpBindTree alt
  = case alt of
        AAlt PDefault x
         -> slurpBindTree x

        AAlt (PData _ bs) x
         -> [bindDefX BindCasePat bs [x]]


instance BindStruct Witness where
 slurpBindTree ww
  = case ww of
        WVar u          -> [BindUse BoundExpWit u]
        WCon{}          -> []
        WApp  w1 w2     -> slurpBindTree w1 ++ slurpBindTree w2
        WJoin w1 w2     -> slurpBindTree w1 ++ slurpBindTree w2
        WType t         -> slurpBindTree t


-- | Helper for constructing the `BindTree` for an expression or witness binder.
bindDefX :: BindStruct c 
         => BindWay -> [Bind n] -> [c n] -> BindTree n
bindDefX way bs xs
        = BindDef way bs
        $   concatMap (slurpBindTree . typeOfBind) bs
        ++  concatMap slurpBindTree xs


-- | Helper for constructing the `BindTree` for a type binder.
bindDefT :: BindStruct c
         => BindWay -> [Bind n] -> [c n] -> BindTree n
bindDefT way bs xs
        = BindDef way bs $ concatMap slurpBindTree xs


