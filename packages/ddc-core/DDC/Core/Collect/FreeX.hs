{-# OPTIONS_HADDOCK hide #-}
-- | Collect the free value and witness variables in thing.
module DDC.Core.Collect.FreeX
        (FreeX(..))
where
import DDC.Core.Exp
import DDC.Core.Compounds
import DDC.Type.Env                     (Env)
import Data.Set                         (Set)
import qualified DDC.Type.Env           as Env
import qualified Data.Set               as Set


class FreeX n a where

 -- | Collect the free value and witness variables in a thing.
 freeX  :: Ord n 
        => Env n        -- ^ Type environment.
        -> a            -- ^ The thing.
        -> Set (Bound n)


-- Add value or witness variables to the set if they are not in the 
-- environment. Use freeT for type variables.
instance FreeX n (Bound n) where
 freeX tenv u
  | Env.member u tenv   = Set.empty
  | otherwise             
  = case u of
        UName{}         -> Set.singleton u
        UPrim{}         -> Set.empty
        UIx i t         -> Set.singleton $ UIx (i - Env.depth tenv) t


instance FreeX n (Exp a n) where
 freeX tenv xx
  = case xx of
        XVar _ u       -> freeX tenv u
        XCon _ u       -> freeX tenv u
        XApp _ x1 x2   -> Set.unions [freeX tenv x1, freeX tenv x2]
        XLAM _ _ x     -> freeX tenv x
        XLam _ b  x    -> freeX (Env.extend b tenv) x
        XLet _ lts x   -> freeX (Env.extends (valwitBindsOfLets lts) tenv) x
        XCase _ x alts -> Set.unions (freeX tenv x : map (freeX tenv) alts)
        XCast _ c x    -> Set.unions [freeX tenv c,  freeX tenv x ]
        XType _        -> Set.empty
        XWitness w     -> freeX tenv w


instance FreeX n (Alt a n) where
 freeX tenv (AAlt p x)  = freeX (Env.extends (bindsOfPat p) tenv) x


instance FreeX n (Lets a n) where
 freeX tenv lts 
  = case lts of
        LLet m _ x      -> Set.unions [freeX tenv m, freeX tenv x]

        LRec bxs        
         -> let (bs, xs) = unzip bxs
                tenv'    = Env.extends bs tenv
            in  Set.unions $ map (freeX tenv') xs
        
        LLetRegion{}    -> Set.empty
        LWithRegion{}   -> Set.empty
       

instance FreeX n (LetMode n) where
 freeX tenv mm
  = case mm of
        LetStrict               -> Set.empty
        LetLazy Nothing         -> Set.empty
        LetLazy (Just w)        -> freeX tenv w


instance FreeX n (Cast n) where
 freeX tenv cc
  = case cc of
        CastWeakenEffect{}      -> Set.empty
        CastWeakenClosure{}     -> Set.empty
        CastPurify w            -> freeX tenv w
        CastForget w            -> freeX tenv w


instance FreeX n (Witness n) where
 freeX tenv ww
  = case ww of
        WCon{}          -> Set.empty
        WVar u          -> freeX tenv u
        WApp  w1 w2     -> Set.unions  [freeX tenv w1, freeX tenv w2]
        WJoin w1 w2     -> Set.unions  [freeX tenv w1, freeX tenv w2]
        WType{}         -> Set.empty
        
