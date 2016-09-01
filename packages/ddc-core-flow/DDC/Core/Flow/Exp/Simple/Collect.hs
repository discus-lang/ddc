-- | Collecting sets of variables and constructors.
module DDC.Core.Flow.Exp.Simple.Collect
        ()
where
import DDC.Type.Collect
import DDC.Core.Collect.FreeX
import DDC.Core.Flow.Exp.Simple.Exp


-- Exp ------------------------------------------------------------------------
instance BindStruct (Exp a n) n where
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
        
        XLet (LPrivate bsR mtExtend bs) x2                         
         -> (case mtExtend of
                Nothing -> []
                Just tR -> slurpBindTree tR)
         ++ [ BindDef  BindLetRegions bsR
             [bindDefX BindLetRegionWith bs [x2]] ]

        XCase x alts    -> slurpBindTree x ++ concatMap slurpBindTree alts
        XCast c x       -> slurpBindTree c ++ slurpBindTree x
        XType t         -> slurpBindTree t
        XWitness w      -> slurpBindTree w


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
        WVar u          -> [BindUse BoundWit u]
        WCon{}          -> []
        WApp  w1 w2     -> slurpBindTree w1 ++ slurpBindTree w2
        WType t         -> slurpBindTree t
        WAnnot _ w      -> slurpBindTree w

