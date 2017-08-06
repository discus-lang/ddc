{-# OPTIONS_HADDOCK hide #-}
{-# LANGUAGE TypeFamilies #-}

module DDC.Core.Exp.Generic.BindStruct where
import DDC.Core.Exp.Generic.Exp
import DDC.Core.Exp.DaCon
import DDC.Core.Collect.FreeX
import DDC.Core.Collect.BindStruct
import qualified DDC.Type.Exp           as T
import Data.Maybe


instance (GBind l ~ T.Bind l, GBound l ~ T.Bound l)
      => BindStruct (GExp l) l where
 slurpBindTree xx
  = case xx of
        XAnnot _ x              -> slurpBindTree x

        XVar u                  -> [BindUse BoundExp u]

        XCon dc
         -> case dc of
                DaConBound n    -> [BindCon BoundExp (T.UName n) Nothing]
                _               -> []

        XPrim{}                 -> []

        XApp x1 a2              -> slurpBindTree x1 ++ slurpBindTree a2

        XAbs (MType b) x        -> [bindDefT BindLAM [b] [x]]
        XAbs (MTerm b) x        -> [bindDefX BindLam [b] [x]]

        XLet (LLet b x1) x2
         -> slurpBindTree x1
         ++ [bindDefX BindLet [b] [x2]]

        XLet (LRec bxs) x2
         -> [bindDefX BindLetRec
                     (map fst bxs)
                     (map snd bxs ++ [x2])]

        XLet (LPrivate b mT bs) x2
         -> (concat $ fmap slurpBindTree $ maybeToList mT)
         ++ [ BindDef  BindLetRegions b
             [bindDefX BindLetRegionWith bs [x2]]]

        XCase x alts            -> slurpBindTree x ++ concatMap slurpBindTree alts
        XCast c x               -> slurpBindTree c ++ slurpBindTree x


instance (GBind l ~ T.Bind l, GBound l ~ T.Bound l)
      => BindStruct (GArg l) l where
 slurpBindTree arg
  = case arg of
        RType t                 -> slurpBindTree t
        RExp x                  -> slurpBindTree x
        RWitness w              -> slurpBindTree w


instance (GBind l ~ T.Bind l, GBound l ~ T.Bound l)
      => BindStruct (GAlt l) l where
 slurpBindTree alt
  = case alt of
        AAlt PDefault x         -> slurpBindTree x
        AAlt (PData _ bs) x     -> [bindDefX BindCasePat bs [x]]


instance (GBind l ~ T.Bind l, GBound l ~ T.Bound l)
      => BindStruct (GCast l) l where
 slurpBindTree cc
  = case cc of
        CastWeakenEffect  eff   -> slurpBindTree eff
        CastPurify w            -> slurpBindTree w
        CastBox                 -> []
        CastRun                 -> []
        CastPrim{}              -> []


instance (GBind l ~ T.Bind l, GBound l ~ T.Bound l)
      => BindStruct (GWitness l) l where
 slurpBindTree ww
  = case ww of
        WVar  u                 -> [BindUse BoundWit u]
        WCon{}                  -> []
        WApp  w1 w2             -> slurpBindTree w1 ++ slurpBindTree w2
        WType t                 -> slurpBindTree t

