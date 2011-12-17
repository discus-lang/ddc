
-- | Type checker for the DDC core language.
module DDC.Core.Check
        ( checkExp,     typeOfExp,     typeOfExp'
        , checkWitness, typeOfWitness, typeOfWitness'
        , typeOfWiCon
        , Error(..))
where
import DDC.Core.Exp
import DDC.Core.Pretty
import DDC.Core.Collect.Free
import DDC.Core.Check.CheckError
import DDC.Type.Transform
import DDC.Type.Universe
import DDC.Type.Compounds
import DDC.Type.Predicates
import DDC.Type.Sum                     as Sum
import DDC.Type.Env                     (Env)
import DDC.Type.Check.Monad             (result, throw)
import DDC.Base.Pretty                  ()
import Data.Set                         (Set)
import qualified DDC.Type.Env           as Env
import qualified DDC.Type.Check         as T
import qualified DDC.Type.Check.Monad   as G
import qualified Data.Set               as Set
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
                  (Type n, Effect n, Set (Bound n))

checkExp env xx 
 = result
 $ do   (t, effs, fvs) <- checkExpM env xx
        return (t, TSum effs, fvs)
        

-- checkExp ---------------------------------------------------------------------------------------
-- | Check an expression, 
--   returning its type, effect and free value variables.
--   TODO: attach kinds to bound variables, and to sums.
--         check that existing annotations have the same kinds as from the environment.
--         add a function to check that a type has kind annots in the right places.
checkExpM 
        :: (Ord n, Pretty n)
        => Env n -> Exp a n
        -> CheckM a n (Type n, TypeSum n, Set (Bound n))

checkExpM env xx
 = case xx of
        -- variables and constructors ---------------------
        XVar _ u        
         ->     return  ( typeOfBound u
                        , Sum.empty kEffect
                        , Set.singleton u)

        XCon _ u
         ->     return  ( typeOfBound u
                        , Sum.empty kEffect
                        , Set.singleton u)


        -- application ------------------------------------
        -- value-type application.
        XApp _ x1 (XType t2)
         -> do  (t1, effs1, fvs1)    <- checkExpM  env x1
                k2                   <- checkTypeM env t2
                case t1 of
                 TForall b11 t12
                  | typeOfBind b11 == k2
                  -> case takeSubstBoundOfBind b11 of
                      Just u    -> return ( substituteT u t2 t12
                                          , substituteT u t2 effs1
                                          , fvs1)

                      Nothing   -> return (t12, effs1, fvs1)

                  | otherwise   -> throw $ ErrorAppMismatch xx (typeOfBind b11) t2
                 _              -> throw $ ErrorAppNotFun   xx t1 t2

        -- value-witness application.
        XApp _ x1 (XWitness w2)
         -> do  (t1, effs1, fvs1)    <- checkExpM     env x1
                t2                   <- checkWitnessM env w2
                case t1 of
                 TApp (TApp (TCon (TyConWitness TwConImpl)) t11) t12
                  | t11 == t2   
                  -> return     ( t12
                                , effs1
                                , fvs1 )

                  | otherwise   -> throw $ ErrorAppMismatch xx t11 t2
                 _              -> throw $ ErrorAppNotFun   xx t1 t2
                 
        -- value-value application.
        XApp _ x1 x2
         -> do  (t1, effs1, fvs1)    <- checkExpM env x1
                (t2, effs2, fvs2)    <- checkExpM env x2
                case t1 of
                 TApp (TApp (TApp (TApp (TCon (TyConComp TcConFun)) t11) eff) _clo) t12
                  | t11 == t2   
                  , effs   <- Sum.fromList kEffect  [eff]
                  -> return     ( t12
                                , effs1 `plus` effs2 `plus` effs
                                , fvs1 `Set.union` fvs2)
                  | otherwise   -> throw $ ErrorAppMismatch xx t11 t2
                 _              -> throw $ ErrorAppNotFun xx t1 t2


        -- lambda abstractions ----------------------------
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
                (t2, e2, fvs2)  <- checkExpM  env' x2
                k2              <- checkTypeM env' t2

                -- The form of the function constructor depends on what universe we're dealing with.
                -- Note that only the computation abstraction can suspend visible effects.
                case universeFromType2 k1 of
                  Just UniverseComp
                   |  not $ isDataKind k1     -> throw $ ErrorLamBindNotData xx t1 k1
                   |  not $ isDataKind k2     -> throw $ ErrorLamBodyNotData xx b1 t2 k2 
                   |  otherwise
                   -> return ( tFun t1 (TSum e2) (tBot kClosure) t2                             -- TODO: add closure
                             , Sum.empty kEffect
                             , case takeSubstBoundOfBind b1 of
                                Nothing -> fvs2
                                Just u  -> Set.delete u fvs2)

                  Just UniverseWitness
                   | e2 /= Sum.empty kEffect  -> throw $ ErrorLamNotPure     xx (TSum e2)
                   | not $ isDataKind k2      -> throw $ ErrorLamBodyNotData xx b1 t2 k2
                   | otherwise                -> return ( tImpl t1 t2
                                                        , Sum.empty kEffect
                                                        , fvs2)
                      
                  Just UniverseSpec
                   | e2 /= Sum.empty kEffect  -> throw $ ErrorLamNotPure     xx (TSum e2)
                   | not $ isDataKind k2      -> throw $ ErrorLamBodyNotData xx b1 t2 k2
                   | otherwise                -> return ( TForall b1 t2
                                                        , Sum.empty kEffect
                                                        , fvs2)

                  _ -> throw $ ErrorMalformedType xx k1


        -- let --------------------------------------------
        XLet _ (LLet b11 x12) x2
         -> do  -- Check the annotation
                 k11    <- checkTypeM env (typeOfBind b11)

                 -- The parser should ensure the bound variable always has data kind.
                 when (not $ isDataKind k11)
                  $ error $ "checkExpM: LLet does not bind a value variable."
                 
                 -- Check the right of the binding.
                 (t12, effs12, fvs12)  <- checkExpM env x12

                 -- The type of the binding must match that of the right
                 when (typeOfBind b11 /= t12)
                  $ throw $ ErrorLetMismatch xx b11 t12
                 
                 -- Check the body expression.
                 let env1       = Env.extend b11 env
                 (t2, effs2, fvs2)     <- checkExpM env1 x2
                 let fvs2'  = case takeSubstBoundOfBind b11 of
                                Nothing -> fvs2
                                Just u  -> Set.delete u fvs2
                 
                 -- TODO: We should be recording free vars instead of closure terms,
                 --       because we need to mask the bound 
                 return ( t2
                        , effs12 `plus` effs2
                        , fvs12 `Set.union` fvs2')


        -- letregion --------------------------------------
        XLet _ (LLetRegion b bs) x
         -- The parser should ensure the bound variable always has region kind.
         | not $ isRegionKind (typeOfBind b)
         -> error "checkExpM: LRegion does not bind a region variable."

         | otherwise
         -> case takeSubstBoundOfBind b of
             Nothing     -> checkExpM env x
             Just u
              -> do
                -- Check the region variable.
                checkTypeM env (typeOfBind b)
                let env1         = Env.extend b env

                -- Check the witness types.
                mapM_ (checkTypeM env1) $ map typeOfBind bs
                let env2         = Env.extends bs env1

                -- Check the body expression.
                (t, effs, fvs)  <- checkExpM env2 x

                -- The free variables of the body cannot contain the bound region.
                let fvsT         = free Env.empty t
                when (Set.member u fvsT)
                 $ throw $ ErrorLetRegionFree xx b t
                
                -- Delete effects on the bound region from the result.
                let effs'       = Sum.delete (tRead  (TVar u))
                                $ Sum.delete (tWrite (TVar u))
                                $ Sum.delete (tAlloc (TVar u))
                                $ effs
                                
                return (t, effs', fvs)


        -- withregion -------------------------------------
        XLet _ (LWithRegion u) x
         -- The evaluation function should ensure this is a handle.
         | not $ isRegionKind (typeOfBound u)
         -> error "checkExpM: LWithRegion does not contain a region handle"
         
         | otherwise
         -> do  -- Check the region handle.
                checkTypeM env (typeOfBound u)
                
                -- Check the body expression.
                (t, effs, fvs) <- checkExpM env x
                
                -- Delete effects on the bound region from the result.
                let tu          = TCon $ TyConBound u
                let effs'       = Sum.delete (tRead  tu)
                                $ Sum.delete (tWrite tu)
                                $ Sum.delete (tAlloc tu)
                                $ effs
                
                return (t, effs', fvs)
                

        -- case expression
        XCase{} -> error "checkExp: XCase not done yet"
        
        -- type cast --------------------------------------
        XCast _ (CastPurify w) x1
         -> do  tW              <- checkWitnessM env w
                (t1, effs, fvs) <- checkExpM env x1
                
                effs' <- case tW of
                          TApp (TCon (TyConWitness TwConPure)) effMask
                            -> return $ Sum.delete effMask effs
                          _ -> throw  $ ErrorWitnessNotPurity xx w tW

                return (t1, effs', fvs)
 
 
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
                  -> case takeSubstBoundOfBind b11 of
                      Just u    -> return $ substituteT u t2 t12
                      Nothing   -> return t12

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

        WiConDirect
         -> tForall kRegion $ \r -> tDirect r

        WiConShare
         -> tForall kRegion $ \r -> tGlobal r `tImpl` tConst r `tImpl` (tEmpty $ tUse r)

        WiConRead
         -> tForall kRegion $ \r -> tConst r `tImpl` (tPure  $ tRead r)

        WiConAlloc
         -> tForall kRegion $ \r -> tConst r `tImpl` (tPure $ tAlloc r)

        WiConDistinct n
         -> tForalls (replicate n kRegion) $ \rs -> tDistinct rs

