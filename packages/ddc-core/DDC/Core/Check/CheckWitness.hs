
-- | Type checker for witness expressions.
module DDC.Core.Check.CheckWitness
        ( checkWitness
        , typeOfWitness
        , typeOfWiCon
        , typeOfWbCon

        , CheckM
        , checkWitnessM)
where
import DDC.Core.Exp
import DDC.Core.Pretty
import DDC.Core.Check.Error
import DDC.Core.Check.ErrorMessage      ()
import DDC.Type.Transform.SubstituteT
import DDC.Type.Compounds
import DDC.Type.Predicates
import DDC.Type.Equiv
import DDC.Type.Transform.LiftT
import DDC.Type.Sum                     as Sum
import DDC.Type.Env                     (Env)
import DDC.Type.Check.Monad             (result, throw)
import DDC.Base.Pretty                  ()
import qualified DDC.Type.Env           as Env
import qualified DDC.Type.Check         as T
import qualified DDC.Type.Check.Monad   as G


-- | Type checker monad. 
--   Used to manage type errors.
type CheckM a n   = G.CheckM (Error a n)


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
checkWitness
        :: (Ord n, Pretty n)
        => Env n                -- ^ Kind Environment.
        -> Env n                -- ^ Type Environment.
        -> Witness n            -- ^ Witness to check.
        -> Either (Error a n) (Type n)

checkWitness kenv tenv xx
        = result $ checkWitnessM kenv tenv xx


-- | Like `checkWitness`, but check in an empty environment.
--
--   As this function is not given an environment, the types of free variables
--   must be attached directly to the bound occurrences.
--   This attachment is performed by `checkWitness` above.
--
typeOfWitness 
        :: (Ord n, Pretty n) 
        => Witness n 
        -> Either (Error a n) (Type n)
typeOfWitness ww 
        = result 
        $ checkWitnessM Env.empty Env.empty ww


------------------------------------------------------------------------------
-- | Like `checkWitness` but using the `CheckM` monad to manage errors.
checkWitnessM 
        :: (Ord n, Pretty n)
        => Env n                -- ^ Kind environment.
        -> Env n                -- ^ Type environment.
        -> Witness n            -- ^ Witness to check.
        -> CheckM a n (Type n)

checkWitnessM _kenv tenv (WVar u)
 = do   let tBound      = typeOfBound u
        let mtEnv       = Env.lookup u tenv

        let mkResult
             -- When annotation on the bound is bot,
             --  then use the type from the environment.
             | Just tEnv    <- mtEnv
             , isBot tBound
             = return tEnv

             -- The bound has an explicit type annotation,
             --  which matches the one from the environment.
             -- 
             --  When the bound is a deBruijn index we need to lift the
             --  annotation on the original binder through any lambdas
             --  between the binding occurrence and the use.
             | Just tEnv    <- mtEnv
             , UIx i _      <- u
             , equivT tBound (liftT (i + 1) tEnv) 
             = return tBound

             -- The bound has an explicit type annotation,
             --  which matches the one from the environment.
             | Just tEnv    <- mtEnv
             , equivT tBound tEnv
             = return tEnv

             -- The bound has an explicit type annotation,
             --  which does not match the one from the environment.
             --  This shouldn't happen because the parser doesn't add non-bot
             --  annotations to bound variables.
             | Just tEnv    <- mtEnv
             = throw $ ErrorVarAnnotMismatch u tEnv

             -- Variable not in environment, so use annotation.
             --  This happens when checking open terms.
             | otherwise
             = return tBound
        
        tResult  <- mkResult
        return tResult


checkWitnessM _kenv _tenv (WCon wc)
 = return $ typeOfWiCon wc

  
-- value-type application
checkWitnessM kenv tenv ww@(WApp w1 (WType t2))
 = do   t1      <- checkWitnessM  kenv tenv w1
        k2      <- checkTypeM     kenv t2
        case t1 of
         TForall b11 t12
          |  typeOfBind b11 == k2
          -> return $ substituteT b11 t2 t12

          | otherwise   -> throw $ ErrorWAppMismatch ww (typeOfBind b11) k2
         _              -> throw $ ErrorWAppNotCtor  ww t1 t2

-- witness-witness application
checkWitnessM kenv tenv ww@(WApp w1 w2)
 = do   t1      <- checkWitnessM kenv tenv w1
        t2      <- checkWitnessM kenv tenv w2
        case t1 of
         TApp (TApp (TCon (TyConWitness TwConImpl)) t11) t12
          |  t11 == t2   
          -> return t12

          | otherwise   -> throw $ ErrorWAppMismatch ww t11 t2
         _              -> throw $ ErrorWAppNotCtor  ww t1 t2

-- witness joining
checkWitnessM kenv tenv ww@(WJoin w1 w2)
 = do   t1      <- checkWitnessM kenv tenv w1
        t2      <- checkWitnessM kenv tenv w2
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
checkWitnessM kenv _tenv (WType t)
 = checkTypeM kenv t
        

-- | Take the type of a witness constructor.
typeOfWiCon :: WiCon n -> Type n
typeOfWiCon wc
 = case wc of
    WiConBuiltin wb -> typeOfWbCon wb
    WiConBound u    -> typeOfBound u


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
checkTypeM :: (Ord n, Pretty n) => Env n -> Type n -> CheckM a n (Kind n)
checkTypeM kenv tt
 = case T.checkType kenv tt of
        Left err        -> throw $ ErrorType err
        Right k         -> return k

