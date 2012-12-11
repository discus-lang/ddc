-- | Type checker for witness expressions.
module DDC.Core.Check.CheckWitness
        ( Config(..)
        , configOfProfile

        , checkWitness
        , typeOfWitness
        , typeOfWiCon
        , typeOfWbCon

        , CheckM
        , checkWitnessM

        , checkTypeM)
where
import DDC.Core.Exp
import DDC.Core.Pretty
import DDC.Core.Check.Error
import DDC.Core.Check.ErrorMessage              ()
import DDC.Type.DataDef
import DDC.Type.Transform.SubstituteT
import DDC.Type.Compounds
import DDC.Type.Universe
import DDC.Type.Sum                             as Sum
import DDC.Type.Env                             (KindEnv, TypeEnv)
import DDC.Control.Monad.Check                  (throw, result)
import DDC.Base.Pretty                          ()
import qualified DDC.Control.Monad.Check        as G
import qualified DDC.Type.Env                   as Env
import qualified DDC.Type.Check                 as T
import qualified DDC.Core.Fragment              as F


-- | Type checker monad. 
--   Used to manage type errors.
type CheckM a n   = G.CheckM (Error a n)


-- Config ---------------------------------------------------------------------
-- | Static configuration for the type checker.
--   These fields don't change as we decend into the tree.
--
--   The starting configuration should be converted from the profile that
--   defines the language fragment you are checking. 
--   See "DDC.Core.Fragment" and use `configOfProfile` below.
data Config n
        = Config
        { -- | Data type definitions.
          configPrimDataDefs            :: DataDefs n 

          -- | Kinds of primitive types.
        , configPrimKinds               :: KindEnv n

          -- | Types of primitive operators.
        , configPrimTypes               :: TypeEnv n

          -- | Suppress all closure information, 
          --   annotating all functions with an empty closure.
          --   
          --   This is used when checking the Disciple Core Salt fragment,
          --   as transforms in this language don't use the closure
          --   information.
        , configSuppressClosures        :: Bool }


-- | Convert a langage profile to a type checker configuration.
configOfProfile :: F.Profile n -> Config n
configOfProfile profile
        = Config
        { configPrimDataDefs    = F.profilePrimDataDefs profile
        , configPrimKinds       = F.profilePrimKinds profile
        , configPrimTypes       = F.profilePrimTypes profile

        , configSuppressClosures      
                = F.featuresUntrackedClosures
                $ F.profileFeatures profile }


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
        -> Witness n            -- ^ Witness to check.
        -> Either (Error a n) (Type n)

checkWitness config kenv tenv xx
        = result $ checkWitnessM config kenv tenv xx


-- | Like `checkWitness`, but check in an empty environment.
--
--   As this function is not given an environment, the types of free variables
--   must be attached directly to the bound occurrences.
--   This attachment is performed by `checkWitness` above.
--
typeOfWitness 
        :: (Ord n, Show n, Pretty n) 
        => Config n
        -> Witness n 
        -> Either (Error a n) (Type n)

typeOfWitness config ww 
        = result 
        $ checkWitnessM config Env.empty Env.empty ww


------------------------------------------------------------------------------
-- | Like `checkWitness` but using the `CheckM` monad to manage errors.
checkWitnessM 
        :: (Ord n, Show n, Pretty n)
        => Config n             -- ^ Data type definitions.
        -> KindEnv n            -- ^ Kind environment.
        -> TypeEnv n            -- ^ Type environment.
        -> Witness n            -- ^ Witness to check.
        -> CheckM a n (Type n)

checkWitnessM !_config !_kenv !tenv (WVar u)
 = case Env.lookup u tenv of
        Nothing -> throw $ ErrorUndefinedVar u UniverseWitness
        Just t  -> return t

checkWitnessM !_config !_kenv !_tenv (WCon wc)
 = return $ typeOfWiCon wc
  
-- witness-type application
checkWitnessM !config !kenv !tenv ww@(WApp w1 (WType t2))
 = do   t1      <- checkWitnessM  config kenv tenv w1
        k2      <- checkTypeM     config kenv t2
        case t1 of
         TForall b11 t12
          |  typeOfBind b11 == k2
          -> return $ substituteT b11 t2 t12

          | otherwise   -> throw $ ErrorWAppMismatch ww (typeOfBind b11) k2
         _              -> throw $ ErrorWAppNotCtor  ww t1 t2

-- witness-witness application
checkWitnessM !config !kenv !tenv ww@(WApp w1 w2)
 = do   t1      <- checkWitnessM config kenv tenv w1
        t2      <- checkWitnessM config kenv tenv w2
        case t1 of
         TApp (TApp (TCon (TyConWitness TwConImpl)) t11) t12
          |  t11 == t2   
          -> return t12

          | otherwise   -> throw $ ErrorWAppMismatch ww t11 t2
         _              -> throw $ ErrorWAppNotCtor  ww t1 t2

-- witness joining
checkWitnessM !config !kenv !tenv ww@(WJoin w1 w2)
 = do   t1      <- checkWitnessM config kenv tenv w1
        t2      <- checkWitnessM config kenv tenv w2
        case (t1, t2) of
         (  TApp (TCon (TyConWitness TwConPure)) eff1
          , TApp (TCon (TyConWitness TwConPure)) eff2)
          -> return $ TApp (TCon (TyConWitness TwConPure))
                           (TSum $ Sum.fromList kEffect  [eff1, eff2])

         (  TApp (TCon (TyConWitness TwConEmpty)) clo1
          , TApp (TCon (TyConWitness TwConEmpty)) clo2)
          -> return $ TApp (TCon (TyConWitness TwConEmpty))
                           (TSum $ Sum.fromList kClosure [clo1, clo2])

         _ -> throw $ ErrorCannotJoin ww w1 t1 w2 t2

-- embedded types
checkWitnessM !config !kenv !_tenv (WType t)
 = checkTypeM config kenv t
        

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
        -> Type n 
        -> CheckM a n (Kind n)

checkTypeM config kenv tt
 = case T.checkType (configPrimDataDefs config) kenv tt of
        Left err        -> throw $ ErrorType err
        Right k         -> return k

