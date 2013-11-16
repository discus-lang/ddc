-- | Collecting sets of variables and constructors.
module DDC.Core.Collect.Free.Simple
        ()
where
import DDC.Type.Collect
import DDC.Core.Collect.Free
import DDC.Core.Exp.Simple


-- Exp ------------------------------------------------------------------------
instance BindStruct (Exp a) where
 slurpBindTree xx
  = case xx of
        XAnnot _ x
         -> slurpBindTree x
        XVar u
         -> [BindUse BoundExp u]

        XCon dc
         -> case dc of
                DaConBound n    -> [BindCon BoundExp (UName n) Nothing]
                _               -> []

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
        
        XLet (LLetRegions b bs) x2
         -> [ BindDef  BindLetRegions b
             [bindDefX BindLetRegionWith bs [x2]]]

        XLet (LWithRegion u) x2
         -> BindUse BoundExp u : slurpBindTree x2

        XCase x alts    -> slurpBindTree x ++ concatMap slurpBindTree alts
        XCast c x       -> slurpBindTree c ++ slurpBindTree x
        XType t         -> slurpBindTree t
        XWitness w      -> slurpBindTree w


instance BindStruct (Cast a) where
 slurpBindTree cc
  = case cc of
        CastWeakenEffect  eff   -> slurpBindTree eff
        CastWeakenClosure xs    -> concatMap slurpBindTree xs
        CastPurify w            -> slurpBindTree w
        CastForget w            -> slurpBindTree w
        CastBox                 -> []
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
        WVar u          -> [BindUse BoundWit u]
        WCon{}          -> []
        WApp  w1 w2     -> slurpBindTree w1 ++ slurpBindTree w2
        WJoin w1 w2     -> slurpBindTree w1 ++ slurpBindTree w2
        WType t         -> slurpBindTree t
        WAnnot _ w      -> slurpBindTree w

