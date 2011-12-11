
-- | Type checker for the DDC core language.
module DDC.Core.Check
        ( checkExp,     typeOfExp,     typeOfExp'
        , checkWitness, typeOfWitness, typeOfWitness'
        , typeOfWiCon
        , Error(..))
where
import DDC.Core.Exp
import DDC.Core.Pretty
import DDC.Core.Check.CheckError
import DDC.Type.Transform
import DDC.Type.Operators.Trim
import DDC.Type.Universe
import DDC.Type.Compounds
import DDC.Type.Predicates
import DDC.Type.Sum                     as Sum
import DDC.Type.Env                     (Env)
import DDC.Type.Check.Monad             (result, throw)
import DDC.Base.Pretty                  ()
import qualified DDC.Type.Env           as Env
import qualified DDC.Type.Check         as T
import qualified DDC.Type.Check.Monad   as G
import Control.Monad

type CheckM a n   = G.CheckM (Error a n)


-- Wrappers ---------------------------------------------------------------------------------------
-- | Take the kind of a type.
typeOfExp 
        :: (Ord n, Pretty n)
        => Exp a n
        -> Either (Error a n) (Type n)
typeOfExp xx 
 = case checkExp Env.empty xx of
        Left err        -> Left err
        Right (t, _, _) -> Right t
        

-- | Take the kind of a type, or `error` if there isn't one.
typeOfExp' 
        :: (Pretty n, Ord n) 
        => Exp a n -> Type n
typeOfExp' tt
 = case checkExp Env.empty tt of
        Left err        -> error $ show $ ppr err
        Right (t, _, _) -> t


-- | Check an expression, returning an error or its type, effect and closure.
--   TODO: We trim the closure once at the top.
--         We don't want to trim at every level, bad complexity.
--         We prob want to trim types once at the bottom of the tree, to keep the 
--         overall size down, then once again at the top.
checkExp 
        :: (Ord n, Pretty n)
        => Env n -> Exp a n
        -> Either (Error a n)
                  (Type n, Effect n, Closure n)

checkExp env xx 
 = result
 $ do   (t, effs, clos) <- checkExpM env xx
        let clo_trimmed = trimClosure (TSum clos)
        return  
         (t, TSum effs, clo_trimmed)
        

-- checkExp ---------------------------------------------------------------------------------------
-- | Check a type, returning its kind.
--   TODO: attach kinds to bound variables, and to sums.
--         check that existing annotations have the same kinds as from the environment.
--         add a function to check that a type has kind annots in the right places.
checkExpM 
        :: (Ord n, Pretty n)
        => Env n -> Exp a n
        -> CheckM a n (Type n, TypeSum n, TypeSum n)

checkExpM env xx
 = case xx of
        -- variables, primitives and constructors.
        XVar _ u        
         ->     return  ( typeOfBound u
                        , Sum.empty kEffect
                        , Sum.singleton kClosure (tDeepShare $ typeOfBound u))

        XCon _ u
         ->     return  ( typeOfBound u
                        , Sum.empty kEffect
                        , Sum.singleton kClosure (tDeepShare $ typeOfBound u))

        -- value-type application.
        XApp _ x1 (XType t2)
         -> do  (t1, effs1, clos1)   <- checkExpM  env x1
                k2                   <- checkTypeM env t2
                case t1 of
                 TForall b11 t12
                  | typeOfBind b11 == k2
                  -> case takeSubstBoundOfBind b11 of
                      Just u    -> return ( substituteT u t2 t12
                                          , substituteT u t2 effs1
                                          , substituteT u t2 clos1)

                      Nothing   -> return (t12, effs1, clos1)

                  | otherwise   -> throw $ ErrorAppMismatch xx (typeOfBind b11) t2
                 _              -> throw $ ErrorAppNotFun   xx t1 t2

        -- value-witness application.
        XApp _ x1 (XWitness w2)
         -> do  (t1, effs1, clos1)   <- checkExpM     env x1
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
         -> do  (t1, effs1, clos1) <- checkExpM env x1
                (t2, effs2, clos2) <- checkExpM env x2
                case t1 of
                 TApp (TApp (TApp (TApp (TCon (TyConComp TcConFun)) t11) eff) _clo) t12
                  | t11 == t2   
                  , effs   <- Sum.fromList kEffect  [eff]
                  -> return     ( t12
                                , effs1 `plus` effs2 `plus` effs
                                , clos1 `plus` clos2)
                  | otherwise   -> throw $ ErrorAppMismatch xx t11 t2
                 _              -> throw $ ErrorAppNotFun xx t1 t2

        -- lambda abstractions.
        XLam _ b1 x2
         -> do  let t1          =  typeOfBind b1
                k1              <- checkTypeM env t1
                let u1          =  universeFromType2 k1

                -- We can't shadow level 1 binders because subsequent types will depend 
                -- on the original version.
                when (  u1 == Just UniverseSpec
                     && Env.memberBind b1 env)
                 $ throw $ ErrorLamReboundSpec xx b1

                -- Check the body.
                let env'        =  Env.extend b1 env
                (t2, e2, c2)    <- checkExpM  env' x2
                k2              <- checkTypeM env' t2

                -- The form of the function constructor depends on what universe we're dealing with.
                -- Note that only the computation abstraction can suspend visible effects.
                case universeFromType2 k1 of
                  Just UniverseComp
                   |  not $ isDataKind k1  -> throw $ ErrorLamBindNotData xx t1 k1
                   |  not $ isDataKind k2  -> throw $ ErrorLamBodyNotData xx b1 t2 k2 
                   |  otherwise
                   -> return ( tFun t1 (TSum e2) (TSum c2) t2
                             , Sum.empty kEffect
                             , Sum.empty kClosure)

                  Just UniverseWitness
                   | e2 /= Sum.empty kEffect -> throw $ ErrorLamNotPure     xx (TSum e2)
                   | not $ isDataKind k2   -> throw $ ErrorLamBodyNotData xx b1 t2 k2
                   | otherwise             -> return (tImpl t1 t2,   Sum.empty kEffect, Sum.empty kClosure)
                      
                  Just UniverseSpec
                   | e2 /= Sum.empty kEffect -> throw $ ErrorLamNotPure     xx (TSum e2)
                   | not $ isDataKind k2   -> throw $ ErrorLamBodyNotData xx b1 t2 k2
                   | otherwise             -> return (TForall b1 t2, Sum.empty kEffect, Sum.empty kClosure)

                  _ -> throw $ ErrorMalformedType xx k1


        -- let binding
        XLet{}  -> error "checkExp: XLet not done yet"
        
        -- case expression
        XCase{} -> error "checkExp: XCase not done yet"
        
        -- type cast
        XCast{} -> error "checkExp: XCase not done yet"
                
 
        _ -> error "typeOfExp: not handled yet"


-- checkType -------------------------------------------------------------------------------------
-- | Check a type in the exp checking monad.
checkTypeM :: Ord n => Env n -> Type n -> CheckM a n (Kind n)
checkTypeM env tt
 = case T.checkType env tt of
        Left err        -> throw $ ErrorType err
        Right k         -> return k


-- Witness ----------------------------------------------------------------------------------------
typeOfWitness :: Ord n => Witness n -> Either (Error a n) (Type n)
typeOfWitness ww = result $ checkWitnessM Env.empty ww


-- | Take the kind of a type, or `error` if there isn't one.
typeOfWitness' :: forall n. (Ord n, Pretty n) => Witness n -> Type n
typeOfWitness' ww
 = case typeOfWitness ww of
        Left err        -> error $ show $ ppr (err :: Error () n)
        Right k         -> k


-- | Check an expression, returning an error or its type, effect and closure.
checkWitness
        :: Ord n 
        => Env n -> Witness n
        -> Either (Error a n) (Type n)

checkWitness env xx
        = result $ checkWitnessM env xx


checkWitnessM 
        :: Ord n 
        => Env n -> Witness n
        -> CheckM a n (Type n)

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

        WiConShare
         -> tForall kRegion $ \r -> (tConst r)  `tImpl` (tEmpty $ tShare r)

        WiConDistinct n
         -> tForalls (replicate n kRegion) $ \rs -> tDistinct rs

