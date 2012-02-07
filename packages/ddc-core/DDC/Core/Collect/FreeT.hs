
-- | Collect the free type variables in a core expression.
module DDC.Core.Collect.FreeT
        (FreeT(..))
where
import DDC.Core.Exp
import DDC.Core.Compounds
import DDC.Type.Collect.FreeT
import qualified DDC.Type.Env           as Env
import qualified Data.Set               as Set


instance FreeT n (Exp a n) where
 freeT kenv xx
  = case xx of
        XVar{}          -> Set.empty
        XCon{}          -> Set.empty
        XApp _ x1 x2    -> Set.unions [freeT kenv x1, freeT kenv x2]
        XLAM _ b x      -> freeT (Env.extend b kenv) x
        XLam _ b x      -> Set.unions [ freeT kenv b, freeT kenv x ]

        XLet _ lts x    -> Set.unions [ freeT kenv lts
                                      , freeT (Env.extends (specBindsOfLets lts) kenv) x ]

        XCase _ x alts  -> Set.unions (freeT kenv x : map (freeT kenv) alts)
        XCast _ c x     -> Set.unions [freeT kenv c, freeT kenv x ]
        XType t         -> freeT kenv t
        XWitness w      -> freeT kenv w


instance FreeT n (Alt a n) where
 freeT kenv (AAlt _ x)  =  freeT kenv x


instance FreeT n (Lets a n) where
 freeT kenv lts 
  = case lts of
        LLet m b x        
         -> Set.unions  [freeT kenv m, freeT kenv b, freeT kenv x]

        LRec bxs        
         -> let (bs, xs) = unzip bxs
            in  Set.unions 
                 $  map (freeT kenv) bs
                 ++ map (freeT kenv) xs
        
        LLetRegion b bs   
         -> let kenv'   = Env.extend b kenv
            in  Set.unions $ map (freeT kenv') bs

        LWithRegion{}   -> Set.empty
       

instance FreeT n (LetMode n) where
 freeT kenv mm
  = case mm of
        LetStrict               -> Set.empty
        LetLazy Nothing         -> Set.empty
        LetLazy (Just w)        -> freeT kenv w


instance FreeT n (Cast n) where
 freeT kenv cc
  = case cc of
        CastWeakenEffect  eff   -> freeT kenv eff
        CastWeakenClosure clo   -> freeT kenv clo
        CastPurify w            -> freeT kenv w
        CastForget w            -> freeT kenv w


instance FreeT n (Witness n) where
 freeT kenv ww
  = case ww of
        WCon{}          -> Set.empty
        WVar{}          -> Set.empty
        WApp  w1 w2     -> Set.unions [freeT kenv w1, freeT kenv w2]
        WJoin w1 w2     -> Set.unions [freeT kenv w1, freeT kenv w2]
        WType t         -> freeT kenv t

