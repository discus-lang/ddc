-- | Type checker for witness expressions.
module DDC.Core.Check.Witness
        ( checkWitness
        , checkWitnessM
        , typeOfWitness
        , typeOfWiCon)
where
import DDC.Core.Exp.Annot.AnT
import DDC.Core.Check.Error
import DDC.Core.Check.ErrorMessage              ()
import DDC.Core.Check.Base
import DDC.Core.Check.Judge.Kind
import DDC.Type.Transform.SubstituteT
import qualified DDC.Core.Env.EnvT              as EnvT
import qualified DDC.Core.Check.Context         as Context
import qualified Data.Map.Strict                as Map


-- Wrappers --------------------------------------------------------------------
-- | Check a witness.
--
--   If it's good, you get a new version with types attached to all the bound
--   variables, as well as the type of the overall witness.
--
--   If it's bad, you get a description of the error.
--
--   The returned expression has types attached to all variable occurrences,
--   so you can call `typeOfWitness` on any open subterm.
--
--   The kinds and types of primitives are added to the environments
--   automatically, you don't need to supply these as part of the
--   starting environments.
--
checkWitness
        :: (Ord n, Show n, Pretty n)
        => Config  n            -- ^ Type checker configuration.
        -> EnvX n               -- ^ Type checker environment.
        -> Witness a n          -- ^ Witness to check.
        -> Either (Error a n)
                  ( Witness (AnT a n) n
                  , Type n)

checkWitness config env xx
 = let  ctx     = contextOfEnvX env
   in   evalCheck (mempty, 0, 0)
         $ checkWitnessM config ctx xx


-- | Like `checkWitness`, but check in an empty environment.
--
--   As this function is not given an environment, the types of free variables
--   must be attached directly to the bound occurrences.
--   This attachment is performed by `checkWitness` above.
--
typeOfWitness
        :: (Ord n, Show n, Pretty n)
        => Config n             -- ^ Type checker configuration.
        -> EnvX n               -- ^ Type checker environment.
        -> Witness a n          -- ^ Witness to check.
        -> Either (Error a n) (Type n)

typeOfWitness config env ww
 = case checkWitness config env ww of
        Left  err       -> Left err
        Right (_, t)    -> Right t


------------------------------------------------------------------------------
-- | Like `checkWitness` but using the `CheckM` monad to manage errors.
checkWitnessM
        :: (Ord n, Show n, Pretty n)
        => Config n             -- ^ Type checker configuration.
        -> Context n            -- ^ Type checker context.
        -> Witness a n          -- ^ Witness to check.
        -> CheckM a n
                ( Witness (AnT a n) n
                , Type n)

checkWitnessM !_config !ctx (WVar a u)
 -- Witness is defined locally.
 | Just t       <- lookupType u ctx
 = return ( WVar (AnT t a) u, t)

 -- Witness is defined globally.
 | UName n      <- u
 , Just t       <- Map.lookup n (EnvT.envtCapabilities (Context.contextEnvT ctx))
 = return ( WVar (AnT t a) u, t)

 | otherwise
 = throw $ ErrorUndefinedVar a u UniverseWitness

checkWitnessM !_config !_ctx (WCon a wc)
 = let  t'       = typeOfWiCon wc
   in   return  ( WCon (AnT t' a) wc
                , t')

-- witness-type application
checkWitnessM !config !ctx ww@(WApp a1 w1 (WType a2 t2))
 = do   (w1', t1)       <- checkWitnessM config ctx w1
        (t2', k2, _)    <- checkTypeM    config ctx UniverseSpec t2 Recon
        case t1 of
         TForall b11 t12
          |  typeOfBind b11 == k2
          -> let t'     = substituteT b11 t2' t12
             in  return ( WApp (AnT t' a1) w1' (WType (AnT k2 a2) t2')
                        , t')

          | otherwise   -> throw $ ErrorWAppMismatch a1 ww (typeOfBind b11) k2
         _              -> throw $ ErrorWAppNotCtor  a1 ww t1 t2'

-- witness-witness application
checkWitnessM !config !ctx ww@(WApp a w1 w2)
 = do   (w1', t1)       <- checkWitnessM config ctx w1
        (w2', t2)       <- checkWitnessM config ctx w2
        case t1 of
         TApp (TApp (TCon (TyConWitness TwConImpl)) t11) t12
          |  t11 == t2
          -> return ( WApp (AnT t12 a) w1' w2'
                    , t12)

          | otherwise   -> throw $ ErrorWAppMismatch a ww t11 t2
         _              -> throw $ ErrorWAppNotCtor  a ww t1 t2

-- embedded types
checkWitnessM !config !ctx (WType a t)
 = do   (t', k, _)  <- checkTypeM config ctx UniverseSpec t Recon
        return  ( WType (AnT k a) t'
                , k)


-- | Take the type of a witness constructor.
typeOfWiCon :: WiCon n -> Type n
typeOfWiCon wc
 = case wc of
    WiConBound _ t  -> t

