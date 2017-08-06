{-# OPTIONS_HADDOCK hide #-}
module DDC.Core.Check.Context.Effect
        (effectSupported)
where
import DDC.Core.Check.Context.Base
import DDC.Core.Check.Context.Elem
import DDC.Type.Exp.Simple
import Data.Maybe
import qualified DDC.Core.Env.EnvT      as EnvT
import qualified DDC.Type.Sum           as Sum
import qualified Data.Map.Strict        as Map


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

