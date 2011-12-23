
-- | Type checker for the DDC core language.
module DDC.Core.Check.CheckExp
        ( checkExp
        , typeOfExp
        , typeOfExp'
        , checkExpM
        , CheckM(..)
        , TaggedClosure(..))
where
import DDC.Core.Exp
import DDC.Core.Pretty
import DDC.Core.Collect.Free
import DDC.Core.Check.CheckError
import DDC.Core.Check.CheckWitness
import DDC.Type.Operators.Trim
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
import qualified Data.Set               as Set
import Control.Monad


-- Wrappers -------------------------------------------------------------------
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
checkExp 
        :: (Ord n, Pretty n)
        => Env n -> Exp a n
        -> Either (Error a n)
                  (Type n, Effect n, Closure n)

checkExp env xx 
 = result
 $ do   (t, effs, clos) <- checkExpM env xx
        return  ( t
                , TSum effs
                , closureOfTaggedSet clos)
        

-- checkExp -------------------------------------------------------------------
-- | Check an expression, 
--   returning its type, effect and free value variables.
--   TODO: 
--    attach kinds to bound variables, and to sums.
--    check that existing annotations have the same kinds as from the
--    environment.
--    add a function to check that a type has kind annots in the right
-- places.
checkExpM 
        :: (Ord n, Pretty n)
        => Env n -> Exp a n
        -> CheckM a n (Type n, TypeSum n, Set (TaggedClosure n))


-- variables and constructors ---------------------
checkExpM _env (XVar _ u)
 = return  ( typeOfBound u
           , Sum.empty kEffect
           , Set.singleton $ taggedClosureOfValBound u)

checkExpM _env (XCon _ u)
 = return  ( typeOfBound u
           , Sum.empty kEffect
           , Set.empty)


-- application ------------------------------------
-- value-type application.
checkExpM env xx@(XApp _ x1 (XType t2))
 = do   (t1, effs1, clos1)      <- checkExpM  env x1
        k2                      <- checkTypeM env t2
        case t1 of
         TForall b11 t12
          | typeOfBind b11 == k2
          -> case takeSubstBoundOfBind b11 of
              Just u    -> return ( substituteT u t2 t12
                                  , substituteT u t2 effs1
                                  , clos1 `Set.union` taggedClosureOfTyArg t2)          -- TODO: do subst.

              Nothing   -> return ( t12
                                  , effs1
                                  , clos1 `Set.union` taggedClosureOfTyArg t2)

          | otherwise   -> throw $ ErrorAppMismatch xx (typeOfBind b11) t2
         _              -> throw $ ErrorAppNotFun   xx t1 t2


-- value-witness application.
checkExpM env xx@(XApp _ x1 (XWitness w2))
 =do  (t1, effs1, clos1)      <- checkExpM     env x1
      t2                      <- checkWitnessM env w2
      case t1 of
       TApp (TApp (TCon (TyConWitness TwConImpl)) t11) t12
        | t11 == t2   
        -> return (t12, effs1, clos1)

        | otherwise   -> throw $ ErrorAppMismatch xx t11 t2
       _              -> throw $ ErrorAppNotFun   xx t1 t2
                 

-- value-value application.
checkExpM env xx@(XApp _ x1 x2)
 = do   (t1, effs1, clos1)    <- checkExpM env x1
        (t2, effs2, clos2)    <- checkExpM env x2
        case t1 of
         TApp (TApp (TApp (TApp (TCon (TyConComp TcConFun)) t11) eff) _clo) t12
          | t11 == t2   
          , effs    <- Sum.fromList kEffect  [eff]
          -> return ( t12
                    , effs1 `plus` effs2 `plus` effs
                    , clos1 `Set.union` clos2)
          | otherwise   -> throw $ ErrorAppMismatch xx t11 t2
         _              -> throw $ ErrorAppNotFun xx t1 t2


-- lambda abstractions ----------------------------
checkExpM env xx@(XLam _ b1 x2)
 = do   let t1          =  typeOfBind b1
        k1              <- checkTypeM env t1
        let u1          =  universeFromType2 k1

        -- We can't shadow level 1 binders because subsequent types will depend 
        -- on the original version.
        when (  u1 == Just UniverseSpec
             && Env.memberBind b1 env)
         $ throw $ ErrorLamReboundSpec xx b1

        -- Check the body.
        let env'        =  Env.extend b1 env
        (t2, e2, clo2)  <- checkExpM  env' x2
        k2              <- checkTypeM env' t2

        -- The form of the function constructor depends on what universe we're
        -- dealing with. Note that only the computation abstraction can suspend
        -- visible effects.
        case universeFromType2 k1 of
         Just UniverseComp
          |  not $ isDataKind k1     -> throw $ ErrorLamBindNotData xx t1 k1
          |  not $ isDataKind k2     -> throw $ ErrorLamBodyNotData xx b1 t2 k2 
          |  otherwise
          -> let -- Mask closure terms due to locally bound value vars.
                 clos2_masked
                  = case takeSubstBoundOfBind b1 of
                     Just u -> Set.delete (taggedClosureOfValBound u) clo2
                     _      -> clo2

                 -- Trim the closure before we annotate the returned function
                 -- type with it.
                 clos2_captured
                  = trimClosure $ closureOfTaggedSet clos2_masked

             in  return ( tFun t1 (TSum e2) clos2_captured t2
                        , Sum.empty kEffect
                        , clos2_masked) 

         Just UniverseWitness
          | e2 /= Sum.empty kEffect  -> throw $ ErrorLamNotPure     xx (TSum e2)
          | not $ isDataKind k2      -> throw $ ErrorLamBodyNotData xx b1 t2 k2
          | otherwise                
          -> return ( tImpl t1 t2
                    , Sum.empty kEffect
                    , clo2) 
                      
         Just UniverseSpec
          | e2 /= Sum.empty kEffect  -> throw $ ErrorLamNotPure     xx (TSum e2)
          | not $ isDataKind k2      -> throw $ ErrorLamBodyNotData xx b1 t2 k2
          | otherwise                
          -> let 
                 -- Mask closure terms due to locally bound region vars.
                 clos2_masked 
                  = case takeSubstBoundOfBind b1 of
                     Just u -> Set.difference clo2 (taggedClosureOfTyArg (TVar u))
                     _      -> clo2

             in  return ( TForall b1 t2
                        , Sum.empty kEffect
                        , clos2_masked)

         _ -> throw $ ErrorMalformedType xx k1


-- let --------------------------------------------
checkExpM env xx@(XLet _ (LLet b11 x12) x2)
 = do   -- Check the annotation
        k11    <- checkTypeM env (typeOfBind b11)

        -- The parser should ensure the bound variable always has data kind.
        when (not $ isDataKind k11)
         $ error $ "checkExpM: LLet does not bind a value variable."
                         
        -- Check the right of the binding.
        (t12, effs12, clo12)  <- checkExpM env x12

        -- The type of the binding must match that of the right
        when (typeOfBind b11 /= t12)
         $ throw $ ErrorLetMismatch xx b11 t12
         
        -- Check the body expression.
        let env1  = Env.extend b11 env
        (t2, effs2, clo2)     <- checkExpM env1 x2

        -- Mask closure terms due to locally bound value vars.
        let clo2_masked
             = case takeSubstBoundOfBind b11 of
                Nothing -> clo2
                Just u  -> Set.delete (taggedClosureOfValBound u) clo2
        
        return ( t2
               , effs12 `plus` effs2
               , clo12 `Set.union` clo2_masked)


-- letregion --------------------------------------
checkExpM env xx@(XLet _ (LLetRegion b bs) x)
 -- The parser should ensure the bound variable always has region kind.
 | not $ isRegionKind (typeOfBind b)
 = error "checkExpM: LRegion does not bind a region variable."

 | otherwise
 = case takeSubstBoundOfBind b of
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
        (t, effs, clo)  <- checkExpM env2 x

        -- The free variables of the body cannot contain the bound region.
        let fvsT         = free Env.empty t
        when (Set.member u fvsT)
         $ throw $ ErrorLetRegionFree xx b t
        
        -- Delete effects on the bound region from the result.
        let effs'       = Sum.delete (tRead  (TVar u))
                        $ Sum.delete (tWrite (TVar u))
                        $ Sum.delete (tAlloc (TVar u))
                        $ effs

        -- Delete the bound region variable from the cloure.
        let clo_masked  = Set.delete (GBoundRgnVar u) clo
        
        return (t, effs', clo_masked)


-- withregion -----------------------------------
checkExpM env (XLet _ (LWithRegion u) x)
 -- The evaluation function should ensure this is a handle.
 | not $ isRegionKind (typeOfBound u)
 = error "checkExpM: LWithRegion does not contain a region handle"
 
 | otherwise
 = do  -- Check the region handle.
        checkTypeM env (typeOfBound u)
        
        -- Check the body expression.
        (t, effs, clo) <- checkExpM env x
        
        -- Delete effects on the bound region from the result.
        let tu          = TCon $ TyConBound u
        let effs'       = Sum.delete (tRead  tu)
                        $ Sum.delete (tWrite tu)
                        $ Sum.delete (tAlloc tu)
                        $ effs                

        return (t, effs', clo)
                

-- case expression ------------------------------
checkExpM _env XCase{} 
 = error "checkExp: XCase not done yet"
        
-- type cast -------------------------------------
checkExpM env xx@(XCast _ (CastPurify w) x1)
 = do   tW              <- checkWitnessM env w
        (t1, effs, clo) <- checkExpM env x1
                
        effs' <- case tW of
                  TApp (TCon (TyConWitness TwConPure)) effMask
                    -> return $ Sum.delete effMask effs
                  _ -> throw  $ ErrorWitnessNotPurity xx w tW

        return (t1, effs', clo)


-- some other thing -----------------------------
checkExpM _env _
 = error "typeOfExp: not handled yet"


-- checkType ------------------------------------------------------------------
-- | Check a type in the exp checking monad.
checkTypeM :: Ord n => Env n -> Type n -> CheckM a n (Kind n)
checkTypeM env tt
 = case T.checkType env tt of
        Left err        -> throw $ ErrorType err
        Right k         -> return k


-- TaggedClosure --------------------------------------------------------------
-- | A closure tagged with the bound variable that the closure term is due to.
data TaggedClosure n
        = GBoundVal    (Bound n) (TypeSum n)
        | GBoundRgnVar (Bound n)
        | GBoundRgnCon (Bound n)
        deriving Show


instance Eq n  => Eq (TaggedClosure n) where
 (==)    (GBoundVal u1 _)  (GBoundVal u2 _)     = u1 == u2
 (==)    (GBoundRgnVar u1) (GBoundRgnVar u2)    = u1 == u2
 (==)    (GBoundRgnCon u1) (GBoundRgnCon u2)    = u1 == u2
 (==)    _                 _                    = False
 

instance Ord n => Ord (TaggedClosure n) where
 compare g1 g2 = compare (ordify g1) (ordify g2)
  where 
        ordify gg
         = case gg of
                GBoundVal u _   -> (0, u) :: (Int, Bound n)
                GBoundRgnVar u  -> (1, u)
                GBoundRgnCon u  -> (2, u)


instance (Eq n, Pretty n) => Pretty (TaggedClosure n) where
 ppr cc
  = case cc of
        GBoundVal    u clos -> text "CLOVAL   " <+> ppr u <+> text ":" <+> ppr clos
        GBoundRgnVar u      -> text "CLORGNVAR" <+> ppr u
        GBoundRgnCon u      -> text "CLORGNCON" <+> ppr u


-- | Convert a tagged clousure to a regular closure by dropping the tag variables.
closureOfTagged :: TaggedClosure n -> Closure n
closureOfTagged gg
 = case gg of
        GBoundVal _ clos  -> TSum $ clos
        GBoundRgnVar u    -> tUse (TVar u)
        GBoundRgnCon u    -> tUse (TCon (TyConBound u))


-- | Convert a set of tagged closures to a regular closure by dropping the
--   tag variables.
closureOfTaggedSet :: Ord n => Set (TaggedClosure n) -> Closure n
closureOfTaggedSet clos
        = TSum  $ Sum.fromList kClosure 
                $ map closureOfTagged 
                $ Set.toList clos


-- | Take the tagged closure of a value variable.
taggedClosureOfValBound :: (Ord n, Pretty n) => Bound n -> TaggedClosure n
taggedClosureOfValBound u
        = GBoundVal u 
        $ Sum.singleton kClosure 
        $ trimClosure $ tDeepUse $ typeOfBound u


-- | Take the tagged closure of a type argument.
taggedClosureOfTyArg :: (Ord n, Pretty n) => Type n -> Set (TaggedClosure n)
taggedClosureOfTyArg tt
 = case tt of
        TVar u
         |   isRegionKind (typeOfBound u)
         ->  Set.singleton $ GBoundRgnVar u

        TCon (TyConBound u)
         |   isRegionKind (typeOfBound u)
         ->  Set.singleton $ GBoundRgnCon u

        _ -> Set.empty

