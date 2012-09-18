
module DDC.Core.Collect.Support
        ( Support       (..)
        , SupportX      (..))
where
import DDC.Core.Compounds
import DDC.Core.Exp
import DDC.Type.Collect.FreeT
import DDC.Type.Compounds
import Data.Set                 (Set)
import DDC.Type.Env             (KindEnv, TypeEnv)
import qualified DDC.Type.Env   as Env
import qualified Data.Set       as Set
import Data.Monoid


data Support n
        = Support
        { -- | Type constructors used in the expressoin.
          supportTyCon          :: Set (Bound n)

          -- | Free spec variables in an expression.
        , supportSpVar          :: Set (Bound n)

          -- | Free witness variables in an expression.
          --   (from the Witness universe)
        , supportWiVar          :: Set (Bound n)

          -- | Free value variables in an expression.
          --   (from the Data universe)
        , supportDaVar          :: Set (Bound n) }
        deriving Show


instance Ord n => Monoid (Support n) where
 mempty = Support
        { supportTyCon          = Set.empty
        , supportSpVar          = Set.empty
        , supportWiVar          = Set.empty
        , supportDaVar          = Set.empty }

 mappend sp1 sp2
        = Support
        { supportTyCon          = supportTyCon sp1 `Set.union` supportTyCon sp2
        , supportSpVar          = supportSpVar sp1 `Set.union` supportSpVar sp2
        , supportWiVar          = supportWiVar sp1 `Set.union` supportWiVar sp2
        , supportDaVar          = supportDaVar sp1 `Set.union` supportDaVar sp2 }


class SupportX (c :: * -> *) where
 support
        :: Ord n
        => KindEnv n -> TypeEnv n
        -> c n
        -> Support n


instance SupportX Type where
 support kenv _tenv t
  = let (fvs1, tcs)     = freeVarConT kenv t
    in  mempty  { supportTyCon  = tcs
                , supportSpVar  = fvs1 }


instance SupportX Bind where
 support kenv tenv b
  = support kenv tenv 
  $ typeOfBind b


instance SupportX (Exp a) where
 support kenv tenv xx
  = case xx of
        XVar _ u        
         | Env.member u tenv    -> mempty
         | otherwise            -> mempty { supportDaVar = Set.singleton u}

        XCon{}                  
         -> mempty

        XLAM _ b x
         -> support kenv tenv b 
         <> support (Env.extend b kenv) tenv x

        XLam _ b x
         -> support kenv tenv b
         <> support kenv (Env.extend b tenv) x

        XApp _ x1 x2
         -> let s1              = support kenv tenv x1 
                s2              = support kenv tenv x2
            in  mappend s1 s2

        XLet _a lts x2
         -> let s1              = support kenv tenv lts
                (bs1, bs0)      = bindsOfLets lts
                kenv'           = Env.extends bs1 kenv
                tenv'           = Env.extends bs0 tenv
                s2              = support kenv' tenv' x2
            in  mappend s1 s2

        XCase _ x1 alts
         -> let s1              = support kenv tenv x1
                ss              = mconcat $ map (support kenv tenv) alts
            in  mappend s1 ss

        XCast _ c1 x2
         -> let s1              = support kenv tenv c1
                s2              = support kenv tenv x2
            in  mappend s1 s2

        XType t         -> support kenv tenv t
        XWitness w      -> support kenv tenv w


instance SupportX (Alt a) where
 support kenv tenv aa
  = case aa of
        AAlt PDefault x
         -> support kenv tenv x

        AAlt (PData _dc bs0) x
         -> let tenv'   = Env.extends bs0 tenv
            in  support kenv tenv' x


instance SupportX Witness where
 support kenv tenv ww
  = case ww of
        WVar u
         | Env.member u tenv    -> mempty
         | otherwise            -> mempty { supportWiVar = Set.singleton u }

        WCon{}
         -> mempty

        WApp w1 w2
         -> support kenv tenv w1
         <> support kenv tenv w2

        WJoin w1 w2
         -> support kenv tenv w1
         <> support kenv tenv w2

        WType t
         -> support kenv tenv t


instance SupportX (Cast a) where
 support kenv tenv cc
  = case cc of
        CastWeakenEffect eff
         -> support kenv tenv eff

        CastWeakenClosure xs
         -> mconcat $ map (support kenv tenv) xs

        CastPurify w
         -> support kenv tenv w

        CastForget w
         -> support kenv tenv w
         

instance SupportX (Lets a) where
 support kenv tenv lts
  = case lts of
        LLet m b x
         -> support kenv tenv m
         <> support kenv tenv b
         <> support kenv (Env.extend b tenv) x

        LRec bxs
         -> (mconcat $ map (support kenv tenv) $ map fst bxs)
         <> (let tenv' = Env.extends (map fst bxs) tenv
             in  mconcat $ map (support kenv tenv') $ map snd bxs)

        LLetRegions bs ws
         -> (mconcat $ map (support kenv tenv) bs)
         <> (let kenv' = Env.extends bs kenv
             in  mconcat $ map (support kenv' tenv) ws)

        LWithRegion u
         | Env.member u kenv    -> mempty
         | otherwise            -> mempty { supportSpVar = Set.singleton u }


instance SupportX LetMode where
 support kenv tenv mm
  = case mm of
        LetStrict               -> mempty
        LetLazy Nothing         -> mempty
        LetLazy (Just w)        -> support kenv tenv w


