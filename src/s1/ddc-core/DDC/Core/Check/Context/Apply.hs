{-# OPTIONS_HADDOCK hide #-}
module DDC.Core.Check.Context.Apply
        ( applyContextEither
        , applySolvedEither)
where
import DDC.Core.Check.Context.Elem
import DDC.Core.Check.Context.Base
import DDC.Type.Exp.Simple
import qualified DDC.Type.Sum           as Sum

import Data.Set                         (Set)
import Data.Maybe
import qualified Data.IntMap.Strict     as IntMap
import qualified Data.Set               as Set

import Prelude                          hiding ((<$>))


-- | Apply a context to a type, updating any existentials in the type. This
--   uses just the solved constraints on the stack, but not in the solved set.
--
--   If we find a loop through the existential equations then
--   return `Left` the existential and what is was locally bound to.
--
applyContextEither
        :: Ord n
        => Context n    -- ^ Type checker context.
        -> Set Int      -- ^ Indexes of existentials we've already entered.
        -> Type n       -- ^ Type to apply context to.
        -> Either (Type n, Type n) (Type n)

applyContextEither ctx is tt
 = case tt of
        TVar{}
         ->     return tt

        TCon (TyConExists i k)
         |  Just t      <- lookupExistsEq (Exists i k) ctx
         -> if Set.member i is
                then Left (tt, t)
                else applyContextEither ctx (Set.insert i is) t

        TCon{}
         ->     return tt

        TAbs b t
         -> do  tb'     <- applySolvedEither ctx is (typeOfBind b)
                let b'  =  replaceTypeOfBind tb' b
                t'      <- applySolvedEither ctx is t
                return $ TAbs b' t'

        TApp t1 t2
         -> do  t1'     <- applySolvedEither ctx is t1
                t2'     <- applySolvedEither ctx is t2
                return  $ TApp t1' t2'

        TForall b t
         -> do  tb'     <- applySolvedEither ctx is (typeOfBind b)
                let b'  =  replaceTypeOfBind tb' b
                t'      <- applySolvedEither ctx is t
                return $ TForall b' t'

        TSum ts
         -> do  tss'    <- mapM (applyContextEither ctx is)
                        $  Sum.toList ts

                return  $ TSum
                        $ Sum.fromList (Sum.kindOfSum ts) tss'


-- | Like `applyContextEither`, but for the solved types.
applySolvedEither
        :: Ord n
        => Context n    -- ^ Type checker context.
        -> Set Int      -- ^ Indexes of existentials we've already entered.
        -> Type n       -- ^ Type to apply context to.
        -> Either (Type n, Type n) (Type n)

applySolvedEither ctx is tt
 = case tt of
        TVar{}
         ->     return tt

        TCon (TyConExists i k)
         |  Just t       <- IntMap.lookup i (contextSolved ctx)
         -> if Set.member i is
                then Left (tt, t)
                else applySolvedEither ctx (Set.insert i is) t

         |  Just t       <- lookupExistsEq (Exists i k) ctx
         -> if Set.member i is
                then Left (tt, t)
                else applySolvedEither ctx (Set.insert i is) t

        TCon {}
         ->     return tt

        TAbs b t
         -> do  tb'     <- applySolvedEither ctx is (typeOfBind b)
                let b'  =  replaceTypeOfBind tb' b
                t'      <- applySolvedEither ctx is t
                return  $ TAbs b' t'

        TApp t1 t2
         -> do  t1'     <- applySolvedEither ctx is t1
                t2'     <- applySolvedEither ctx is t2
                return  $ TApp t1' t2'

        TForall b t
         -> do  tb'     <- applySolvedEither ctx is (typeOfBind b)
                let b'  =  replaceTypeOfBind tb' b
                t'      <- applySolvedEither ctx is t
                return  $ TForall b' t'

        TSum ts
         -> do  tss'    <- mapM (applySolvedEither ctx is)
                        $  Sum.toList ts

                return  $  TSum
                        $  Sum.fromList (Sum.kindOfSum ts) tss'

