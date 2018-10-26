-- | Collecting sets of variables and constructors.
module DDC.Core.Collect.FreeX
        ( freeX
        , bindDefX)
where
import DDC.Type.Exp.Simple
import DDC.Core.Collect.BindStruct
import DDC.Core.Collect.FreeT           ()
import DDC.Core.Module
import DDC.Core.Exp
import DDC.Type.Env                     (Env)
import qualified DDC.Type.Env           as Env
import qualified Data.Set               as Set
import Data.Set                         (Set)
import Data.Maybe
import Control.Monad


-- freeX ----------------------------------------------------------------------
-- | Collect the free Data and Witness variables in a thing (level-0).
freeX   :: (BindStruct c n, Ord n)
        => Env n -> c -> Set (Bound n)
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
instance BindStruct (Module a n) n where
 slurpBindTree mm
        = slurpBindTree $ moduleBody mm


-- Exp ------------------------------------------------------------------------
instance BindStruct (Exp a n) n where
 slurpBindTree xx
  = case xx of
        XVar _ u        -> [BindUse BoundExp u]
        XApp _ x1 x2    -> slurpBindTree x1 ++ slurpBindTree x2

        XAbs _ (MType     b) x  -> [bindDefT BindLAM [b] [x]]
        XAbs _ (MTerm     b) x  -> [bindDefX BindLam [b] [x]]
        XAbs _ (MImplicit b) x  -> [bindDefX BindLam [b] [x]]

        XLet _ (LLet b x1) x2
         -> slurpBindTree x1
         ++ [bindDefX BindLet [b] [x2]]

        XLet _ (LRec bxs) x2
         -> [bindDefX BindLetRec
                     (map fst bxs)
                     (map snd bxs ++ [x2])]

        XLet _ (LPrivate b mT bs) x2
         -> (concat $ liftM slurpBindTree $ maybeToList mT)
         ++ [ BindDef  BindLetRegions b
             [bindDefX BindLetRegionWith bs [x2]]]

        XAtom{}          -> []
        XCase _ x alts   -> slurpBindTree x ++ concatMap slurpBindTree alts
        XCast _ c x      -> slurpBindTree c ++ slurpBindTree x
        XAsync _ b e1 e2 -> slurpBindTree e1 ++ [bindDefX BindAsync [b] [e2]]


instance BindStruct (Arg a n) n where
 slurpBindTree aa
  = case aa of
        RType t                 -> slurpBindTree t
        RTerm x                 -> slurpBindTree x
        RWitness  w             -> slurpBindTree w
        RImplicit x             -> slurpBindTree x


instance BindStruct (Cast a n) n where
 slurpBindTree cc
  = case cc of
        CastWeakenEffect  eff   -> slurpBindTree eff
        CastPurify w            -> slurpBindTree w
        CastBox                 -> []
        CastRun                 -> []


instance BindStruct (Alt a n) n where
 slurpBindTree alt
  = case alt of
        AAlt PDefault x
         -> slurpBindTree x

        AAlt (PData _ bs) x
         -> [bindDefX BindCasePat bs [x]]


instance BindStruct (Witness a n) n where
 slurpBindTree ww
  = case ww of
        WVar _ u        -> [BindUse BoundWit u]
        WCon{}          -> []
        WApp  _ w1 w2   -> slurpBindTree w1 ++ slurpBindTree w2
        WType _ t       -> slurpBindTree t


-- | Helper for constructing the `BindTree` for an expression or witness binder.
bindDefX :: BindStruct c n
         => BindWay -> [Bind n] -> [c] -> BindTree n
bindDefX way bs xs
        = BindDef way bs
        $   concatMap (slurpBindTree . typeOfBind) bs
        ++  concatMap slurpBindTree xs
