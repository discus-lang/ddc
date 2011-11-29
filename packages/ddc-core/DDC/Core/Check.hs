
module DDC.Core.Check
        ( checkExp,     typeOfExp,     typeOfExp'
        , checkWitness, typeOfWitness, typeOfWitness'
        , typeOfWiCon
        , Error(..))
where
import DDC.Core.Exp
import DDC.Core.Pretty
import DDC.Base.Pretty                  ()
import DDC.Type.Compounds
import DDC.Type.Sum                     as T
import DDC.Type.Check.Env               (Env)
import DDC.Type.Check.Monad             (result, throw)
import qualified DDC.Type.Check         as T
import qualified DDC.Type.Check.Env     as Env
import qualified DDC.Type.Check.Monad   as G

type CheckM a n p   = G.CheckM (Error a n p)


-- Wrappers ---------------------------------------------------------------------------------------
-- | Take the kind of a type.
typeOfExp  :: Ord n => Exp a n p -> Either (Error a n p) (Type n)
typeOfExp xx 
        = result 
        $ do    (t, _eff, _clo) <- checkExpM Env.empty xx
                return t

-- | Take the kind of a type, or `error` if there isn't one.
typeOfExp' :: (Ord n, Pretty n) => Exp a n p -> Type n
typeOfExp' tt
 = case typeOfExp tt of
        Left err        -> error $ show $ ppr err
        Right k         -> k


-- | Check an expression, returning an error or its type, effect and closure.
checkExp 
        :: Ord n 
        => Env n -> Exp a n p
        -> Either (Error a n p)
                  (Type n, Effect n, Closure n)

checkExp env xx 
 = result
 $ do   (t, effs, clos) <- checkExpM env xx
        return  (t, TSum effs, TSum clos)
        

-- checkExp ---------------------------------------------------------------------------------------
-- | Check a type, returning its kind.
--   TODO: attach kinds to bound variables, and to sums.
--         check that existing annotations have the same kinds as from the environment.
--         add a function to check that a type has kind annots in the right places.
checkExpM 
        :: Ord n 
        => Env n -> Exp a n p
        -> CheckM a n p (Type n, TypeSum n, TypeSum n)

checkExpM env xx
 = case xx of
        -- variables.
        XVar _ u
         ->     return  ( typeOfBound u
                        , T.empty kEffect
                        , T.empty kClosure)
         

        -- data constructors.
        XCon _ u
         ->     return  ( typeOfBound u
                        , T.empty kEffect
                        , T.empty kClosure)


        -- lambda abstractions.
        XLam _ _b _x
         ->      return $ error "not finished"

{-         do  let t1          =  typeOfBind b
                k1              <- checkTypeM env t1
                (t2, e2, c2)    <- checkExpM  (Env.extend b env) x
                
                if isValueType t1 then 
                        return  ( tFun t1 t2 (TSum e2) (TSum c2)
                                , T.empty kEffect
                                , T.empty kClosure)
                 else if 
-}

        -- value-type application.
        XApp _ x1 (XType t2)
         -> do  (t1, effs1, clos1)   <- checkExpM  env x1
                k2                   <- checkTypeM env t2
                case t1 of
                 TForall b11 t12
                  | typeOfBind b11 == k2
                  -> return     ( t12           -- TODO: subst type into result
                                , effs1         -- TODO: subst type into result
                                , clos1 )       -- TODO: subst type into result

                  | otherwise   -> throw $ ErrorAppMismatch xx (typeOfBind b11) t2
                 _              -> throw $ ErrorAppNotFun   xx t1 t2


        -- value-witness application.
        XApp _ x1 (XWitness w2)
         -> do  (t1, effs1, clos1)   <- checkExpM env x1
                t2                   <- checkWitnessM env w2
                case t1 of
                 TApp (TApp (TCon (TyConWitness TwConImpl)) t11) t12
                  | t11 == t2   
                  -> return     ( t12
                                , effs1
                                , clos1 )

                  | otherwise   -> throw $ ErrorAppMismatch xx t11 t2
                 _              -> throw $ ErrorAppNotFun   xx t1 t2
                 

        -- value-value application.
        XApp _ x1 x2
         -> do  (t1, effs1, clos1)    <- checkExpM env x1
                (t2, effs2, clos2)    <- checkExpM env x2
                case t1 of
                 TApp (TApp (TApp (TApp (TCon (TyConComp TcConFun))
                        t11) t12) eff) clo
                  | t11 == t2   
                  , TSum effs   <- eff
                  , TSum clos   <- clo
                  -> return     ( t12
                                , effs1 `plus` effs2 `plus` effs
                                , clos1 `plus` clos2 `plus` clos)
                  | otherwise   -> throw $ ErrorAppMismatch xx t11 t2
                 _              -> throw $ ErrorAppNotFun xx t1 t2
 
        _ -> error "typeOfExp: not handled yet"


-- checkType -------------------------------------------------------------------------------------
-- | Check a type in the exp checking monad.
checkTypeM :: Ord n => Env n -> Type n -> CheckM a n p (Kind n)
checkTypeM env tt
 = case T.checkType env tt of
        Left err        -> throw $ ErrorType err
        Right k         -> return k


-- Witness ----------------------------------------------------------------------------------------
typeOfWitness :: Ord n => Witness n -> Either (Error a n p) (Type n)
typeOfWitness ww = result $ checkWitnessM Env.empty ww


-- | Take the kind of a type, or `error` if there isn't one.
typeOfWitness' :: (Ord n, Pretty n) => Witness n -> Type n
typeOfWitness' ww
 = case typeOfWitness ww of
        Left err        -> error $ show $ ppr err
        Right k         -> k


-- | Check an expression, returning an error or its type, effect and closure.
checkWitness
        :: Ord n 
        => Env n -> Witness n
        -> Either (Error a n p) (Type n)

checkWitness env xx
        = result $ checkWitnessM env xx


checkWitnessM 
        :: Ord n 
        => Env n -> Witness n
        -> CheckM a n p (Type n)

checkWitnessM _ ww
 = case ww of
        WCon wc
         -> return $ typeOfWiCon wc

        _ -> error "typeOfWitness: not handled yet"
        

-- | Take the type of a witness constructor.
typeOfWiCon :: WiCon -> Type n
typeOfWiCon wc
 = case wc of
        WiConPure     -> tPure  (tBot kEffect)
        WiConEmpty    -> tEmpty (tBot kClosure)

        WiConConst    
         -> tForall kRegion $ \r -> tConst r

        WiConMutable
         -> tForall kRegion $ \r -> tMutable r

        WiConLazy
         -> tForall kRegion $ \r -> tLazy r

        WiConDirect
         -> tForall kRegion $ \r -> tDirect r

        WiConRead
         -> tForall kRegion $ \r -> (tConst r) `tImpl`  (tPure  $ tRead r)

        WiConFree
         -> tForall kRegion $ \r -> (tConst r)  `tImpl` (tEmpty $ tFree r)

        WiConDistinct n
         -> tForalls (replicate n kRegion) $ \rs -> tDistinct rs


-- Error ------------------------------------------------------------------------------------------
-- | Type errors.
data Error a n p
        -- | Found a kind error when checking a type.
        = ErrorType
        { errorTypeError        :: T.Error n }

        -- | Types of parameter and arg don't match when checking application.
        | ErrorAppMismatch
        { errorChecking         :: Exp a n p
        , errorParamType        :: Type n
        , errorArgType          :: Type n }

        -- | Tried to apply a non function to an argument.
        | ErrorAppNotFun
        { errorChecking         :: Exp a n p
        , errorNotFunType       :: Type n
        , errorArgType          :: Type n }


instance (Eq n, Pretty n) => Pretty (Error a n p) where
 ppr err
  = case err of
        ErrorType err'  -> ppr err'
        
        ErrorAppMismatch tt t1 t2
         -> sep $ punctuate line 
                [ text "Core type mismatch in application."
                , text "                    type: " <> ppr t1
                , text "          does not match: " <> ppr t2
                , text "          in application: " <> ppr tt ]
         
        ErrorAppNotFun _tt t1 t2
         -> sep $ punctuate line
                [ text "Core type mismatch in application."
                , text " cannot apply expression"
                , text "                 of type: " <> ppr t2
                , text "         to non-function "
                , text "                 of type: " <> ppr t1 ]

        
