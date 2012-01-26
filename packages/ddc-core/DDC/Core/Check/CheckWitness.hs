

-- | Type checker for the DDC core language.
module DDC.Core.Check.CheckWitness
        ( checkWitness
        , typeOfWitness, typeOfWitness'
        , typeOfWiCon
        , checkWitnessM
        , CheckM)
where
import DDC.Core.Exp
import DDC.Core.Pretty
import DDC.Core.Check.CheckError
import DDC.Type.Transform
import DDC.Type.Compounds
import DDC.Type.Sum                     as Sum
import DDC.Type.Env                     (Env)
import DDC.Type.Check.Monad             (result, throw)
import DDC.Base.Pretty                  ()
import qualified DDC.Type.Env           as Env
import qualified DDC.Type.Check         as T
import qualified DDC.Type.Check.Monad   as G
import Control.Monad


type CheckM a n   = G.CheckM (Error a n)


-- Witness --------------------------------------------------------------------
typeOfWitness :: (Ord n, Pretty n) => Witness n -> Either (Error a n) (Type n)
typeOfWitness ww = result $ checkWitnessM Env.empty ww


-- | Take the kind of a type, or `error` if there isn't one.
typeOfWitness' :: forall n. (Ord n, Pretty n) => Witness n -> Type n
typeOfWitness' ww
 = case typeOfWitness ww of
        Left err        -> error $ show $ ppr (err :: Error () n)
        Right k         -> k


-- | Check an expression, returning an error or its type, effect and closure.
checkWitness
        :: (Ord n, Pretty n)
        => Env n -> Witness n
        -> Either (Error a n) (Type n)

checkWitness env xx
        = result $ checkWitnessM env xx


checkWitnessM 
        :: (Ord n, Pretty n)
        => Env n -> Witness n
        -> CheckM a n (Type n)

checkWitnessM env ww
 = case ww of
        -- variables and constructors.
        WVar u  -> return $ typeOfBound u
        WCon wc -> return $ typeOfWiCon wc

        -- value-type application
        WApp w1 (WType t2)
         -> do  t1      <- checkWitnessM  env w1
                k2      <- checkTypeM     env t2
                case t1 of
                 TForall b11 t12
                  | typeOfBind b11 == k2
                  -> return $ substituteT b11 t2 t12

                  | otherwise   -> throw $ ErrorWAppMismatch ww (typeOfBind b11) k2
                 _              -> throw $ ErrorWAppNotCtor  ww t1 t2


        -- witness-witness application
        WApp w1 w2
         -> do  t1      <- checkWitnessM env w1
                t2      <- checkWitnessM env w2
                case t1 of
                 TApp (TApp (TCon (TyConWitness TwConImpl)) t11) t12
                  | t11 == t2   
                  -> return t12
                  | otherwise   -> throw $ ErrorWAppMismatch ww t11 t2
                 _              -> throw $ ErrorWAppNotCtor  ww t1 t2


        -- join witnesses
        WJoin w1 w2
         -> do  t1      <- checkWitnessM env w1
                t2      <- checkWitnessM env w2
                case (t1, t2) of
                 (   TApp (TCon (TyConWitness TwConPure)) eff1
                  ,  TApp (TCon (TyConWitness TwConPure)) eff2)
                  -> return $ TApp (TCon (TyConWitness TwConPure))
                                   (TSum $ Sum.fromList kEffect  [eff1, eff2])

                 (   TApp (TCon (TyConWitness TwConEmpty)) clo1
                  ,  TApp (TCon (TyConWitness TwConEmpty)) clo2)
                  -> return $ TApp (TCon (TyConWitness TwConEmpty))
                                   (TSum $ Sum.fromList kClosure [clo1, clo2])

                 _ -> throw $ ErrorCannotJoin ww w1 t1 w2 t2

        -- embedded types
        WType t -> checkTypeM env t
        

-- | Take the type of a witness constructor.
typeOfWiCon :: WiCon -> Type n
typeOfWiCon wc
 = case wc of
        WiConPure     -> tPure  (tBot kEffect)
        WiConEmpty    -> tEmpty (tBot kClosure)

        WiConGlobal
         -> tForall kRegion $ \r -> tGlobal r

        WiConConst    
         -> tForall kRegion $ \r -> tConst r

        WiConMutable
         -> tForall kRegion $ \r -> tMutable r

        WiConLazy
         -> tForall kRegion $ \r -> tLazy r

        WiConManifest
         -> tForall kRegion $ \r -> tManifest r

        WiConUse
         -> tForall kRegion $ \r -> tGlobal r     `tImpl` (tEmpty $ tUse r)

        WiConRead
         -> tForall kRegion $ \r -> tConst  r     `tImpl` (tPure  $ tRead r)

        WiConAlloc
         -> tForall kRegion $ \r -> tConst  r     `tImpl` (tPure $ tAlloc r)

        WiConDistinct n
         -> tForalls (replicate n kRegion) $ \rs -> tDistinct rs


-- checkType ------------------------------------------------------------------
-- | Check a type in the exp checking monad.
checkTypeM :: Ord n => Env n -> Type n -> CheckM a n (Kind n)
checkTypeM env tt
 = case T.checkType env tt of
        Left err        -> throw $ ErrorType err
        Right k         -> return k

