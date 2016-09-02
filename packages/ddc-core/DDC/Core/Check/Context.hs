
module DDC.Core.Check.Context
        ( 
          -- * Type Checker Mode.
          Mode    (..)

          -- * Positions in the Context.
        , Pos     (..)

          -- * Roles of Type Variable.
        , Role    (..)

          -- * Existentials
        , Exists  (..)
        , typeOfExists
        , takeExists

          -- * Context Elements.
        , Elem    (..)

          -- * Checker Context.
        , Context (..)

          -- * Construction
        , emptyContext
        , contextOfEnvT
        , contextOfEnvX
        , contextOfPrimEnvs

          -- * Projection
        , contextEquations
        , contextCapabilities
        , contextDataDefs
        , contextEnvT

          -- * Pushing
        , pushType,   pushTypes
        , pushKind,   pushKinds
        , pushExists

          -- * Marking
        , markContext

          -- * Popping
        , popToPos

          -- * Lookup
        , lookupType
        , lookupKind
        , lookupExistsEq

          -- * Membership
        , memberType
        , memberKind
        , memberKindBind

          -- * Existentials
        , locationOfExists
        , updateExists

          -- * Lifting and Lowering
        , liftTypes
        , lowerTypes

          -- * Applying
        , applyContextEither
        , applySolvedEither
        , effectSupported)
where
import DDC.Core.Check.Context.Elem
import DDC.Core.Check.Context.Mode
import DDC.Core.Check.Context.Base
import DDC.Type.Exp.Simple
import qualified DDC.Core.Env.EnvT      as EnvT
import qualified DDC.Type.Sum           as Sum

import Data.Set                         (Set)
import Data.Maybe
import qualified Data.IntMap.Strict     as IntMap
import qualified Data.Set               as Set
import qualified Data.Map.Strict        as Map

import Prelude                          hiding ((<$>))


-- Apply ----------------------------------------------------------------------
-- | Apply a context to a type, updating any existentials in the type. This
--   uses just the solved constraints on the stack, but not in the solved set.
--
--   If we find a loop through the existential equations then 
--   return `Left` the existential and what is was locally bound to.
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


-- Support --------------------------------------------------------------------
-- | Check whether this effect is supported by the given context.
--   This is used when effects are treated as capabilities.
--
--   The overall function can be passed a compound effect, 
--    it returns `Nothing` if the effect is supported, 
--    or `Just e`, where `e` is some unsuported atomic effect.
--
effectSupported 
        :: (Ord n, Show n)
        => Context n 
        -> Effect n 
        -> Maybe (Effect n)

effectSupported ctx eff 
        -- Check that all the components of a sum are supported.
        | TSum ts       <- eff
        = listToMaybe $ concat [ maybeToList $ effectSupported ctx e
                               | e <- Sum.toList ts ]

        -- Abstract effects are fine.
        --  We'll find out if it is really supported once it's instantiated.
        | TVar {} <- eff
        = Nothing

        -- Abstract global effects are always supported.
        | TCon (TyConBound _ k) <- eff
        , k == kEffect
        = Nothing

        -- For an effects on concrete region,
        -- the capability is supported if it's in the lexical environment.
        | TApp (TCon (TyConSpec tc)) _t2 <- eff
        , elem tc [TcConRead, TcConWrite, TcConAlloc]

        ,   -- Capability in local environment. 
            (any  (\b -> equivT (contextEnvT ctx) (typeOfBind b) eff) 
                  [ b | ElemType b <- contextElems ctx ] )
   
            -- Capability imported at top level.
         || (any  (\t -> equivT (contextEnvT ctx) t eff)
                  (Map.elems $ EnvT.envtCapabilities $ contextEnvT ctx))
        = Nothing

        -- For an effect on an abstract region, we allow any capability.
        --  We'll find out if it really has this capability when we try
        --  to run the computation.
        | TApp (TCon (TyConSpec tc)) (TVar u) <- eff
        , elem tc [TcConRead, TcConWrite, TcConAlloc]
        = case lookupKind u ctx of
                Just (_, RoleConcrete)  -> Just eff
                Just (_, RoleAbstract)  -> Nothing
                Nothing                 -> Nothing

        | otherwise
        = Just eff

