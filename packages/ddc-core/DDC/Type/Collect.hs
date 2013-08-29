
-- | Collecting sets of variables and constructors.
module DDC.Type.Collect
        ( freeT
        , freeVarsT

        , FreeVarConT (..)

        , collectBound
        , collectBinds
        , BindTree   (..)
        , BindWay    (..)
        , BindStruct (..)

        , BoundLevel (..)
        , isBoundExpWit
        , boundLevelOfBindWay
        , bindDefT)
where
import DDC.Type.Exp
import DDC.Type.Collect.FreeT
import DDC.Type.Env                     (Env)
import qualified DDC.Type.Env           as Env
import qualified DDC.Type.Sum           as Sum
import qualified Data.Set               as Set
import Data.Set                         (Set)


-- freeT ----------------------------------------------------------------------
-- | Collect the free Spec variables in a thing (level-1).
freeT   :: (BindStruct c, Ord n) 
        => Env n -> c n -> Set (Bound n)
freeT tenv xx = Set.unions $ map (freeOfTreeT tenv) $ slurpBindTree xx

freeOfTreeT :: Ord n => Env n -> BindTree n -> Set (Bound n)
freeOfTreeT kenv tt
 = case tt of
        BindDef way bs ts
         |  BoundSpec   <- boundLevelOfBindWay way
         ,  kenv'       <- Env.extends bs kenv
         -> Set.unions $ map (freeOfTreeT kenv') ts

        BindDef _ _ ts
         -> Set.unions $ map (freeOfTreeT kenv) ts

        BindUse BoundSpec u
         | Env.member u kenv -> Set.empty
         | otherwise         -> Set.singleton u
        _                    -> Set.empty


-- collectBound ---------------------------------------------------------------
-- | Collect all the bound variables in a thing, 
--   independent of whether they are free or not.
collectBound :: (BindStruct c, Ord n) => c n -> Set (Bound n)
collectBound 
        = Set.unions . map collectBoundOfTree . slurpBindTree 

collectBoundOfTree :: Ord n => BindTree n -> Set (Bound n)
collectBoundOfTree tt
 = case tt of
        BindDef _ _ ts  -> Set.unions $ map collectBoundOfTree ts
        BindUse _ u     -> Set.singleton u
        BindCon _ u _   -> Set.singleton u


-- collectSpecBinds -----------------------------------------------------------
-- | Collect all the spec and exp binders in a thing.
collectBinds 
        :: (BindStruct c, Ord n) 
        => c n 
        -> ([Bind n], [Bind n])

collectBinds thing
 = let  tree    = slurpBindTree thing
   in   ( concatMap collectSpecBindsOfTree tree
        , concatMap collectExpBindsOfTree  tree)
        

collectSpecBindsOfTree :: Ord n => BindTree n -> [Bind n]
collectSpecBindsOfTree tt
 = case tt of
        BindDef way bs ts
         |   BoundSpec <- boundLevelOfBindWay way
         ->  concat ( bs
                    : map collectSpecBindsOfTree ts)

         | otherwise
         ->  concatMap collectSpecBindsOfTree ts

        _ -> []


collectExpBindsOfTree :: Ord n => BindTree n -> [Bind n]
collectExpBindsOfTree tt
 = case tt of
        BindDef way bs ts
         |   BoundExp <- boundLevelOfBindWay way
         ->  concat ( bs
                    : map collectExpBindsOfTree ts)

         | otherwise
         ->  concatMap collectExpBindsOfTree ts

        _ -> []


-------------------------------------------------------------------------------
-- | A description of the binding structure of some type or expression.
data BindTree n
        -- | An abstract binding expression.
        = BindDef BindWay    [Bind n] [BindTree n]

        -- | Use of a variable.
        | BindUse BoundLevel (Bound n)

        -- | Use of a constructor.
        | BindCon BoundLevel (Bound n) (Maybe (Kind n))
        deriving (Eq, Show)


-- | Describes how a variable was bound.
data BindWay
        = BindForall
        | BindLAM
        | BindLam
        | BindLet
        | BindLetRec
        | BindLetRegions
        | BindLetRegionWith
        | BindCasePat
        deriving (Eq, Show)


-- | What level this binder is at.
data BoundLevel
        = BoundSpec
        | BoundExp
        | BoundWit
        deriving (Eq, Show)


-- | Check if a boundlevel is expression or witness
isBoundExpWit :: BoundLevel -> Bool
isBoundExpWit BoundExp = True
isBoundExpWit BoundWit = True
isBoundExpWit _        = False


-- | Get the `BoundLevel` corresponding to a `BindWay`.
boundLevelOfBindWay :: BindWay -> BoundLevel
boundLevelOfBindWay way
 = case way of
        BindForall              -> BoundSpec
        BindLAM                 -> BoundSpec
        BindLam                 -> BoundExp
        BindLet                 -> BoundExp
        BindLetRec              -> BoundExp
        BindLetRegions          -> BoundSpec
        BindLetRegionWith       -> BoundExp
        BindCasePat             -> BoundExp


-- BindStruct -----------------------------------------------------------------
class BindStruct (c :: * -> *) where
 slurpBindTree :: c n -> [BindTree n]


instance BindStruct Type where
 slurpBindTree tt
  = case tt of
        TVar u          -> [BindUse BoundSpec u]
        TCon tc         -> slurpBindTree tc
        TForall b t     -> [bindDefT BindForall [b] [t]]
        TApp t1 t2      -> slurpBindTree t1 ++ slurpBindTree t2
        TSum ts         -> concatMap slurpBindTree $ Sum.toList ts


instance BindStruct TyCon where
 slurpBindTree tc
  = case tc of
        TyConBound u k  -> [BindCon BoundSpec u (Just k)]
        _               -> []


-- | Helper for constructing the `BindTree` for a type binder.
bindDefT :: BindStruct c
         => BindWay -> [Bind n] -> [c n] -> BindTree n
bindDefT way bs xs
        = BindDef way bs $ concatMap slurpBindTree xs
