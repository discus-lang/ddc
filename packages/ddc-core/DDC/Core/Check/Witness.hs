-- | Type checker for witness expressions.
module DDC.Core.Check.Witness
        ( Config(..)
        , configOfProfile

        , checkWitness
        , typeOfWitness
        , typeOfWiCon
        , typeOfWbCon

        , CheckM
        , newExists
        , checkWitnessM

        , checkTypeM
        , checkBindM)
where
import DDC.Core.Exp
import DDC.Core.Annot.AnT
import DDC.Core.Pretty
import DDC.Core.Check.Error
import DDC.Core.Check.ErrorMessage              ()
import DDC.Type.Check                           (Config (..), configOfProfile)
import DDC.Type.Transform.SubstituteT
import DDC.Type.Check.Context
import DDC.Type.Compounds
import DDC.Type.Universe
import DDC.Type.Sum                             as Sum
import DDC.Type.Env                             (KindEnv, TypeEnv)
import DDC.Control.Monad.Check                  (throw, evalCheck)
import DDC.Base.Pretty                          ()
import qualified DDC.Control.Monad.Check        as G
import qualified DDC.Type.Env                   as Env
import qualified DDC.Type.Check                 as T


-- | Type checker monad. 
--   Used to manage type errors.
type CheckM a n   = G.CheckM Int (Error a n)


-- | Allocate a new exisistential.
newExists :: CheckM a n (Type n)
newExists
 = do   ix      <- G.get
        G.put (ix + 1)
        return  $ TCon (TyConExists ix kData)


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
        => Config n             -- ^ Static configuration.
        -> KindEnv n            -- ^ Starting Kind Environment.
        -> TypeEnv n            -- ^ Strating Type Environment.
        -> Witness a n          -- ^ Witness to check.
        -> Either (Error a n) 
                  ( Witness (AnT a n) n
                  , Type n)

checkWitness config kenv tenv xx
        = evalCheck 0 
        $ checkWitnessM config kenv tenv emptyContext xx


-- | Like `checkWitness`, but check in an empty environment.
--
--   As this function is not given an environment, the types of free variables
--   must be attached directly to the bound occurrences.
--   This attachment is performed by `checkWitness` above.
--
typeOfWitness 
        :: (Ord n, Show n, Pretty n) 
        => Config n
        -> Witness a n 
        -> Either (Error a n) (Type n)

typeOfWitness config ww 
 = case checkWitness config Env.empty Env.empty ww of
        Left  err       -> Left err
        Right (_, t)    -> Right t


------------------------------------------------------------------------------
-- | Like `checkWitness` but using the `CheckM` monad to manage errors.
checkWitnessM 
        :: (Ord n, Show n, Pretty n)
        => Config n             -- ^ Data type definitions.
        -> KindEnv n            -- ^ Kind environment.
        -> TypeEnv n            -- ^ Type environment.
        -> Context n            -- ^ Input context
        -> Witness a n          -- ^ Witness to check.
        -> CheckM a n 
                ( Witness (AnT a n) n
                , Type n)

checkWitnessM !_config !_kenv !tenv !ctx (WVar a u)
 -- Witness is defined locally.
 | Just t       <- lookupType u ctx
 = return ( WVar (AnT t a) u, t)

 -- Witness is defined globally.
 | Just t       <- Env.lookup u tenv 
 = return ( WVar (AnT t a) u, t)
           
 | otherwise
 = throw $ ErrorUndefinedVar a u UniverseWitness
 
checkWitnessM !_config !_kenv !_tenv !_ctx (WCon a wc)
 = let  t'       = typeOfWiCon wc
   in   return  ( WCon (AnT t' a) wc
                , t')
  
-- witness-type application
checkWitnessM !config !kenv !tenv !ctx ww@(WApp a1 w1 (WType a2 t2))
 = do   (w1', t1)       <- checkWitnessM  config kenv tenv ctx w1
        (t2', k2)       <- checkTypeM     config kenv ctx t2
        case t1 of
         TForall b11 t12
          |  typeOfBind b11 == k2
          -> let t'     = substituteT b11 t2' t12
             in  return ( WApp (AnT t' a1) w1' (WType (AnT k2 a2) t2')
                        , t')

          | otherwise   -> throw $ ErrorWAppMismatch a1 ww (typeOfBind b11) k2
         _              -> throw $ ErrorWAppNotCtor  a1 ww t1 t2'

-- witness-witness application
checkWitnessM !config !kenv !tenv !ctx ww@(WApp a w1 w2)
 = do   (w1', t1)       <- checkWitnessM config kenv tenv ctx w1
        (w2', t2)       <- checkWitnessM config kenv tenv ctx w2
        case t1 of
         TApp (TApp (TCon (TyConWitness TwConImpl)) t11) t12
          |  t11 == t2   
          -> return ( WApp (AnT t12 a) w1' w2'
                    , t12)
          
          | otherwise   -> throw $ ErrorWAppMismatch a ww t11 t2
         _              -> throw $ ErrorWAppNotCtor  a ww t1 t2

-- witness joining
checkWitnessM !config !kenv !tenv !ctx ww@(WJoin a w1 w2)
 = do   (w1', t1) <- checkWitnessM config kenv tenv ctx w1
        (w2', t2) <- checkWitnessM config kenv tenv ctx w2
        case (t1, t2) of
         (  TApp (TCon (TyConWitness TwConPure)) eff1
          , TApp (TCon (TyConWitness TwConPure)) eff2)
          -> let t'     = TApp (TCon (TyConWitness TwConPure))
                               (TSum $ Sum.fromList kEffect  [eff1, eff2])
             in  return ( WJoin (AnT t' a) w1' w2'
                        , t')

         (  TApp (TCon (TyConWitness TwConEmpty)) clo1
          , TApp (TCon (TyConWitness TwConEmpty)) clo2)
          -> let t'     = TApp (TCon (TyConWitness TwConEmpty))
                               (TSum $ Sum.fromList kClosure [clo1, clo2])
             in  return ( WJoin (AnT t' a) w1' w2'
                        , t')

         _ -> throw $ ErrorCannotJoin a ww w1 t1 w2 t2

-- embedded types
checkWitnessM !config !kenv !_tenv !ctx (WType a t)
 = do   (t', k)  <- checkTypeM config kenv ctx t
        return  ( WType (AnT k a) t'
                , k)
        

-- | Take the type of a witness constructor.
typeOfWiCon :: WiCon n -> Type n
typeOfWiCon wc
 = case wc of
    WiConBuiltin wb -> typeOfWbCon wb
    WiConBound _ t  -> t


-- | Take the type of a builtin witness constructor.
typeOfWbCon :: WbCon -> Type n
typeOfWbCon wb
 = case wb of
    WbConPure    -> tPure  (tBot kEffect)
    WbConEmpty   -> tEmpty (tBot kClosure)
    WbConUse     -> tForall kRegion $ \r -> tGlobal r `tImpl` (tEmpty $ tUse r)
    WbConRead    -> tForall kRegion $ \r -> tConst  r `tImpl` (tPure  $ tRead r)
    WbConAlloc   -> tForall kRegion $ \r -> tConst  r `tImpl` (tPure  $ tAlloc r)


-- checkType ------------------------------------------------------------------
-- | Check a type in the exp checking monad.
checkTypeM 
        :: (Ord n, Show n, Pretty n) 
        => Config n 
        -> KindEnv n 
        -> Context n 
        -> Type n
        -> CheckM a n (Type n, Kind n)

checkTypeM config kenv ctx tt
 = case T.checkTypeWithContext config kenv ctx tt of
        Left err        -> throw $ ErrorType err
        Right (t, k)    -> return (t, k)


-- Bind -----------------------------------------------------------------------
-- | Check a bind.
checkBindM
        :: (Ord n, Show n, Pretty n)
        => Config n
        -> KindEnv n
        -> Context n
        -> Bind n
        -> CheckM a n (Bind n, Kind n)

checkBindM config kenv ctx bb
 = case T.checkTypeWithContext config kenv ctx (typeOfBind bb) of
        Left err        -> throw $ ErrorType err
        Right (t', k)   -> return (replaceTypeOfBind t' bb, k)


