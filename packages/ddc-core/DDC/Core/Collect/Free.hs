-- | Collecting sets of variables and constructors.
module DDC.Core.Collect.Free
        (freeX)
where
import DDC.Type.Collect
import DDC.Type.Compounds
import DDC.Core.Module
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
 = {-# SCC freeOfTreeX #-}
   case tt of
        BindDef way bs ts
         |  isBoundExpWit $ boundLevelOfBindWay way
         ,  tenv'        <- Env.extends bs tenv
         -> Set.unions $ map (freeOfTreeX tenv') ts

        BindDef _ _ ts
         -> Set.unions $ map (freeOfTreeX  tenv) ts

        BindUse bl u
         | isBoundExpWit bl
         , Env.member u tenv -> Set.empty
         | isBoundExpWit bl  -> Set.singleton u
        _                    -> Set.empty


-- Module ---------------------------------------------------------------------
instance BindStruct (Module a) where
 slurpBindTree mm
        = slurpBindTree $ moduleBody mm


-- Exp ------------------------------------------------------------------------
instance BindStruct (Exp a) where
 slurpBindTree xx
  = case xx of
        XVar _ u
         -> [BindUse BoundExp u]

        XCon _ dc
         -> case dc of
                DaConBound n    -> [BindCon BoundExp (UName n) Nothing]
                _               -> []

        XApp _ x1 x2            -> slurpBindTree x1 ++ slurpBindTree x2
        XLAM _ b x              -> [bindDefT BindLAM [b] [x]]
        XLam _ b x              -> [bindDefX BindLam [b] [x]]      

        XLet _ (LLet b x1) x2
         -> slurpBindTree x1
         ++ [bindDefX BindLet [b] [x2]]

        XLet _ (LRec bxs) x2
         -> [bindDefX BindLetRec 
                     (map fst bxs) 
                     (map snd bxs ++ [x2])]
        
        XLet _ (LLetRegions b bs) x2
         -> [ BindDef  BindLetRegions b
             [bindDefX BindLetRegionWith bs [x2]]]

        XLet _ (LWithRegion u) x2
         -> BindUse BoundExp u : slurpBindTree x2

        XCase _ x alts  -> slurpBindTree x ++ concatMap slurpBindTree alts
        XCast _ c x     -> slurpBindTree c ++ slurpBindTree x
        XType t         -> slurpBindTree t
        XWitness w      -> slurpBindTree w


instance BindStruct (Cast a) where
 slurpBindTree cc
  = case cc of
        CastWeakenEffect  eff   -> slurpBindTree eff
        CastWeakenClosure xs    -> concatMap slurpBindTree xs
        CastPurify w            -> slurpBindTree w
        CastForget w            -> slurpBindTree w
        CastSuspend             -> []
        CastRun                 -> []


instance BindStruct (Alt a) where
 slurpBindTree alt
  = case alt of
        AAlt PDefault x
         -> slurpBindTree x

        AAlt (PData _ bs) x
         -> [bindDefX BindCasePat bs [x]]


instance BindStruct (Witness a) where
 slurpBindTree ww
  = case ww of
        WVar _ u        -> [BindUse BoundWit u]
        WCon{}          -> []
        WApp  _ w1 w2   -> slurpBindTree w1 ++ slurpBindTree w2
        WJoin _ w1 w2   -> slurpBindTree w1 ++ slurpBindTree w2
        WType _ t       -> slurpBindTree t


-- | Helper for constructing the `BindTree` for an expression or witness binder.
bindDefX :: BindStruct c 
         => BindWay -> [Bind n] -> [c n] -> BindTree n
bindDefX way bs xs
        = BindDef way bs
        $   concatMap (slurpBindTree . typeOfBind) bs
        ++  concatMap slurpBindTree xs
