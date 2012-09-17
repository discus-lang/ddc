module DDC.Core.Transform.Trim
--        ( trimClosures, trimX )
where

import DDC.Core.Collect()
import DDC.Type.Collect
import DDC.Core.Exp
import DDC.Type.Env
import DDC.Core.Transform.Reannotate

import Data.List (nubBy)

-- | Trim the expressions of a weaken closure (XCast CastWeakenClosure)
--   into only the free variables.
--
--   For example,
--    trimClosures [build (\k z. something k), else]
--  = [build, something, else]
trimClosures
        :: (Ord n)
        => a
        -> [Exp a n]
        -> [Exp a n]

trimClosures a xs
 = nub' $ concatMap (freeExp a empty empty) xs
 where
  nub' = nubBy (\x y -> reannotate (const ()) x == reannotate (const ()) y)


-- | Trim an expression if it is a weakclo cast. 
--   Does not recurse! If you want to recursively trim closures,
--   use `transformUpX' (const trimX)`.
trimX   :: (Ord n)
        => Exp a n
        -> Exp a n
trimX (XCast a (CastWeakenClosure ws) in_)
 = XCast a (CastWeakenClosure $ trimClosures a ws) in_

trimX x
 = x


-- freeExp --------------------------------------------------------------------
-- | Collect all the free variables, but return them all as expressions:
--   eg
--     freeExp 
--       (let i = 5 [R0#] () in
--        updateInt [:R0# R1#:] <w> i ...)
--
--     will return something like
--       [ XType (TCon R0#)
--       , XVar updateInt
--       , XType (TCon R0#)
--       , XType (TCon R1#)
--       , XWitness w ]
--
freeExp :: (BindStruct c, Ord n) 
        => a
        -> Env n
        -> Env n
        -> c n
        -> [Exp a n]
freeExp a kenv tenv xx 
 = concatMap (freeOfTreeExp a kenv tenv) $ slurpBindTree xx

freeOfTreeExp
        :: Ord n
        => a
        -> Env n
        -> Env n
        -> BindTree n
        -> [Exp a n]
freeOfTreeExp a kenv tenv tt
 = case tt of
        BindDef way bs ts
         |  isBoundExpWit $ boundLevelOfBindWay way
         ,  tenv'        <- extends bs tenv
         -> concatMap (freeOfTreeExp a kenv tenv') ts

        BindDef way bs ts
         |  BoundSpec    <- boundLevelOfBindWay way
         ,  kenv'        <- extends bs kenv
         -> concatMap (freeOfTreeExp a kenv' tenv) ts

        BindDef _ _ ts
         -> concatMap (freeOfTreeExp a kenv tenv) ts

        BindUse BoundExp u
         | member u tenv     -> []
         | otherwise         -> [XVar a u]

        BindUse BoundWit u
         | member u tenv     -> []
         | otherwise         -> [XWitness (WVar u)]

        BindUse BoundSpec u
         | member u kenv     -> []
         | otherwise         -> [XType (TVar u)]

        BindCon BoundSpec u (Just k)
         | member u kenv     -> []
         | otherwise         -> [XType (TCon (TyConBound u k))]

        _                    -> []

