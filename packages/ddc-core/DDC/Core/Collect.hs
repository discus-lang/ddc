
-- | Collecting sets of variables and constructors.
module DDC.Core.Collect
        ( freeT
        , freeX
        , collectBound
        , collectBinds)
where
import DDC.Type.Collect
import DDC.Type.Compounds
import DDC.Core.Exp
import DDC.Type.Env                     (Env)
import qualified DDC.Type.Env           as Env
import qualified Data.Set               as Set
import Data.Set                         (Set)


-- freeX ----------------------------------------------------------------------
-- | Collect the free Data and Witness variables in a thing (level-0).
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
