
module DDC.Core.Generic.BindStruct where
import DDC.Core.Generic.Exp
import DDC.Core.Collect.Free
import DDC.Core.Exp.DaCon
import DDC.Type.Collect
import DDC.Type.Exp
import Data.Maybe


instance BindStruct (GExp (Bind n) (Bound n) n n) n where
 slurpBindTree xx
  = case xx of
        XVar u                  -> [BindUse BoundExp u]

        XCon dc
         -> case dc of
                DaConBound n    -> [BindCon BoundExp (UName n) Nothing]
                _               -> []

        XPrim{}                 -> []

        XApp x1 x2              -> slurpBindTree x1 ++ slurpBindTree x2
        XLAM b x                -> [bindDefT BindLAM [b] [x]]
        XLam b x                -> [bindDefX BindLam [b] [x]]      

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
        XType t                 -> slurpBindTree t
        XWitness w              -> slurpBindTree w


instance BindStruct (GAlt (Bind n)  (Bound n) n n) n where
 slurpBindTree alt
  = case alt of
        AAlt PDefault x         -> slurpBindTree x
        AAlt (PData _ bs) x     -> [bindDefX BindCasePat bs [x]]


instance BindStruct (GCast (Bind n) (Bound n) n n) n where
 slurpBindTree cc
  = case cc of
        CastWeakenEffect  eff   -> slurpBindTree eff
        CastPurify w            -> slurpBindTree w
        CastBox                 -> []
        CastRun                 -> []


instance BindStruct (GWitness (Bind n) (Bound n) n n) n where
 slurpBindTree ww
  = case ww of
        WVar  u                 -> [BindUse BoundWit u]
        WCon{}                  -> []
        WApp  w1 w2             -> slurpBindTree w1 ++ slurpBindTree w2
        WType t                 -> slurpBindTree t

