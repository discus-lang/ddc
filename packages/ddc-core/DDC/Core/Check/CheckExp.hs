
-- | Type checker for the DDC core language.
module DDC.Core.Check.CheckExp
        ( checkExp
        , typeOfExp
        , typeOfExp'
        , checkExpM
        , CheckM(..)
        , TaggedClosure(..))
where
import DDC.Core.DataDef
import DDC.Core.Predicates
import DDC.Core.Compounds
import DDC.Core.Exp
import DDC.Core.Pretty
import DDC.Core.Collect.Free
import DDC.Core.Check.CheckError
import DDC.Core.Check.CheckWitness
import DDC.Core.Check.TaggedClosure
import DDC.Type.Transform.Crush
import DDC.Type.Transform.Trim
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
import Data.List                        as L
import Data.Maybe


-- Wrappers -------------------------------------------------------------------
-- | Take the kind of a type.
typeOfExp 
        :: (Ord n, Pretty n)
        => DataDefs n
        -> Exp a n
        -> Either (Error a n) (Type n)
typeOfExp defs xx 
 = case checkExp defs Env.empty xx of
        Left err        -> Left err
        Right (t, _, _) -> Right t
        

-- | Take the kind of a type, or `error` if there isn't one.
typeOfExp' 
        :: (Pretty n, Ord n) 
        => DataDefs n -> Exp a n -> Type n
typeOfExp' defs tt
 = case checkExp defs Env.empty tt of
        Left err        -> error $ show $ ppr err
        Right (t, _, _) -> t


-- | Check an expression, returning an error or its type, effect and closure.
checkExp 
        :: (Ord n, Pretty n)
        => DataDefs n
        -> Env n 
        -> Exp a n
        -> Either (Error a n)
                  (Type n, Effect n, Closure n)

checkExp defs env xx 
 = result
 $ do   (t, effs, clos) <- checkExpM defs env xx
        return  ( t
                , TSum effs
                , closureOfTaggedSet clos)
        

-- checkExp -------------------------------------------------------------------
-- | Check an expression, 
--   returning its type, effect and free value variables.
checkExpM 
        :: (Ord n, Pretty n)
        => DataDefs n           -- ^ Data type definitions.
        -> Env n                -- ^ Type environment.
        -> Exp a n              -- ^ Expression to check.
        -> CheckM a n (Type n, TypeSum n, Set (TaggedClosure n))


-- variables and constructors ---------------------
checkExpM _defs env (XVar _ u)
 = do   let tBound  = typeOfBound u
        let mtEnv   = Env.lookup u env

        let mkResult
             -- When annotation on the bound is bot,
             --  then use the type from the environment.
             | Just tEnv    <- mtEnv
             , isBot tBound
             = return tEnv

             -- The bound has an explicit type annotation,
             --  which matches the one from the environment.
             | Just tEnv    <- mtEnv
             , tBound == tEnv
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

        return  ( tResult
                , Sum.empty kEffect
                , Set.singleton $ taggedClosureOfValBound u)


checkExpM _defs _env (XCon _ u)
      = return  ( typeOfBound u
                , Sum.empty kEffect
                , Set.empty)


-- application ------------------------------------
-- value-type application.
checkExpM defs env xx@(XApp _ x1 (XType t2))
 = do   (t1, effs1, clos1)      <- checkExpM  defs env x1
        k2                      <- checkTypeM env t2
        case t1 of
         TForall b11 t12
          | typeOfBind b11 == k2
          -> return ( substituteT b11 t2 t12
                    , substituteT b11 t2 effs1
                    , clos1 `Set.union` taggedClosureOfTyArg t2)

          | otherwise   -> throw $ ErrorAppMismatch xx (typeOfBind b11) t2
         _              -> throw $ ErrorAppNotFun   xx t1 t2


-- value-witness application.
checkExpM defs env xx@(XApp _ x1 (XWitness w2))
 =do  (t1, effs1, clos1)      <- checkExpM     defs env x1
      t2                      <- checkWitnessM env w2
      case t1 of
       TApp (TApp (TCon (TyConWitness TwConImpl)) t11) t12
        | t11 == t2   
        -> return (t12, effs1, clos1)

        | otherwise   -> throw $ ErrorAppMismatch xx t11 t2
       _              -> throw $ ErrorAppNotFun   xx t1 t2
                 

-- value-value application.
checkExpM defs env xx@(XApp _ x1 x2)
 = do   (t1, effs1, clos1)    <- checkExpM defs env x1
        (t2, effs2, clos2)    <- checkExpM defs env x2
        case t1 of
         TApp (TApp (TApp (TApp (TCon (TyConComp TcConFun)) t11) eff) _clo) t12
          | t11 == t2   
          , effs    <- Sum.fromList kEffect  [eff]
          -> return ( t12
                    , effs1 `Sum.union` effs2 `Sum.union` effs
                    , clos1 `Set.union` clos2)
          | otherwise   -> throw $ ErrorAppMismatch xx t11 t2
         _              -> throw $ ErrorAppNotFun xx t1 t2


-- lambda abstractions ----------------------------
checkExpM defs env xx@(XLam _ b1 x2)
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
        (t2, e2, clo2)  <- checkExpM  defs env' x2
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
checkExpM defs env xx@(XLet _ (LLet b11 x12) x2)
 = do   -- Check the right of the binding.
        (t12, effs12, clo12)  <- checkExpM defs env x12

        -- Check binder annotation against the type we inferred for the right.
        (b11', k11')    <- checkLetBindOfTypeM xx env t12 b11

        -- The right of the binding should have data kind.
        when (not $ isDataKind k11')
         $ throw $ ErrorLetBindingNotData xx b11' k11'
          
        -- Check the body expression.
        let env1  = Env.extend b11' env
        (t2, effs2, clo2)     <- checkExpM defs env1 x2

        -- The body should have data kind.
        k2              <- checkTypeM env t2
        when (not $ isDataKind k2)
         $ throw $ ErrorLetBodyNotData xx t2 k2

        -- Mask closure terms due to locally bound value vars.
        let clo2_masked
             = case takeSubstBoundOfBind b11' of
                Nothing -> clo2
                Just u  -> Set.delete (taggedClosureOfValBound u) clo2

        return ( t2
               , effs12 `Sum.union` effs2
               , clo12  `Set.union` clo2_masked)


-- letrec -----------------------------------------
checkExpM defs env xx@(XLet _ (LRec bxs) xBody)
 = do   
        let (bs, xs)    = unzip bxs

        -- Check all the annotations.
        ks              <- mapM (checkTypeM env) $  map typeOfBind bs

        -- Check all the annots have data kind.
        zipWithM_ (\b k
         -> when (not $ isDataKind k)
                $ throw $ ErrorLetBindingNotData xx b k)
                bs ks

        -- All right hand sides need to be lambdas.
        forM_ xs $ \x 
         -> when (not $ isXLam x)
                $ throw $ ErrorLetrecBindingNotLambda xx x

        -- All variables are in scope in all right hand sides.
        let env'        = Env.extends bs env

        -- Check the right hand sides.
        (tsRight, _effssBinds, clossBinds) 
                <- liftM unzip3 $ mapM (checkExpM defs env') xs

        -- Check annots on binders against inferred types of the bindings.
        zipWithM_ (\b t
                -> if typeOfBind b /= t
                        then throw $ ErrorLetMismatch xx b t
                        else return ())
                bs tsRight

        -- Check the body expression.
        (tBody, effsBody, closBody) 
                <- checkExpM defs env' xBody

        -- The body type must have data kind.
        kBody   <- checkTypeM env' tBody
        when (not $ isDataKind kBody)
         $ throw $ ErrorLetBodyNotData xx tBody kBody

        -- Mask closure terms due to locally bound value vars.
        let maskClo clo
             = foldr (\b c -> case takeSubstBoundOfBind b of
                               Nothing -> c
                               Just u  -> Set.delete (taggedClosureOfValBound u) c)
                     clo bs

        return  ( tBody
                , effsBody
                , maskClo $ Set.unions (closBody : clossBinds))


-- letregion --------------------------------------
checkExpM defs env xx@(XLet _ (LLetRegion b bs) x)
 = case takeSubstBoundOfBind b of
     Nothing     -> checkExpM defs env x
     Just u
      -> do
        -- Check the type on the region binder.
        let k   = typeOfBind b
        checkTypeM env k

        -- The binder must have region kind.
        when (not $ isRegionKind k)
         $ throw $ ErrorLetRegionNotRegion xx b k

        -- We can't shadow region binders because we might have witnesses
        -- in the environment that conflict with the ones created here.
        when (Env.memberBind b env)
         $ throw $ ErrorLetRegionRebound xx b
        
        -- Check type correctness of the witness types.
        let env1         = Env.extend b env
        mapM_ (checkTypeM env1) $ map typeOfBind bs

        -- Check that the witnesses bound here are for the region,
        -- and they don't conflict with each other.
        checkWitnessBindsM xx u bs

        -- Check the body expression.
        let env2            = Env.extends bs env1
        (tBody, effs, clo)  <- checkExpM defs env2 x

        -- The body type must have data kind.
        kBody               <- checkTypeM env2 tBody
        when (not $ isDataKind kBody)
         $ throw $ ErrorLetBodyNotData xx tBody kBody

        -- The bound region variable cannot be free in the body type.
        let fvsT         = free Env.empty tBody
        when (Set.member u fvsT)
         $ throw $ ErrorLetRegionFree xx b tBody
        
        -- Delete effects on the bound region from the result.
        let effs'       = Sum.delete (tRead  (TVar u))
                        $ Sum.delete (tWrite (TVar u))
                        $ Sum.delete (tAlloc (TVar u))
                        $ effs

        -- Delete the bound region variable from the closure.
        let clo_masked  = Set.delete (GBoundRgnVar u) clo
        
        return (tBody, effs', clo_masked)


-- withregion -----------------------------------
checkExpM defs env xx@(XLet _ (LWithRegion u) x)
 = do   -- Check the type on the region handle.
        let k   = typeOfBound u
        checkTypeM env k

        -- The handle must have region kind.
        when (not $ isRegionKind k)
         $ throw $ ErrorWithRegionNotRegion xx u k
        
        -- Check the body expression.
        (tBody, effs, clo) <- checkExpM defs env x

        -- The body type must have data kind.
        kBody              <- checkTypeM env tBody
        when (not $ isDataKind kBody)
         $ throw $ ErrorLetBodyNotData xx tBody kBody
        
        -- Delete effects on the bound region from the result.
        let tu          = TCon $ TyConBound u
        let effs'       = Sum.delete (tRead  tu)
                        $ Sum.delete (tWrite tu)
                        $ Sum.delete (tAlloc tu)
                        $ effs
        
        -- Delete the bound region handle from the closure.
        let clo_masked  = Set.delete (GBoundRgnCon u) clo

        return (tBody, effs', clo_masked)
                

-- case expression ------------------------------
checkExpM defs env xx@(XCase _ xDiscrim alts)
 = do
        -- Check the discriminant.
        (tDiscrim, effsDiscrim, closDiscrim) 
         <- checkExpM defs env xDiscrim

        -- Split the type into the type constructor names and type parameters.
        -- Also check that it's algebraic data, and not a function or effect
        -- type etc. 
        (nTyCon, tsArgs)
         <- case takeTyConApps tDiscrim of
                Just (tc, ts)
                 | TyConBound (UName n t) <- tc
                 , takeResultKind t == kData
                 -> return (n, ts)
                      
                 | TyConBound (UPrim n t) <- tc
                 , takeResultKind t == kData
                 -> return (n, ts)

                _ -> throw $ ErrorCaseDiscrimNotAlgebraic xx tDiscrim

        -- Get the mode of the data type, 
        --   this tells us how many constructors there are.
        mode    
         <- case lookupModeOfDataType nTyCon defs of
             Nothing -> throw $ ErrorCaseDiscrimTypeUndeclared xx tDiscrim
             Just m  -> return m

        -- Check the alternatives.
        (ts, effss, closs)     
                <- liftM unzip3 
                $  mapM (checkAltM xx defs env tDiscrim tsArgs) alts

        -- All alternative result types must be identical.
        let (tAlt : _)  = ts
        forM_ ts $ \tAlt' 
         -> when (tAlt /= tAlt') 
             $ throw $ ErrorCaseAltResultMismatch xx tAlt tAlt'

        -- Check for overlapping alternatives.
        let pats                = [p | AAlt p _ <- alts]
        let psDefaults          = filter isPDefault pats
        let nsCtorsMatched      = mapMaybe takeCtorNameOfAlt alts

        -- Alts overlapping because there are multiple defaults.
        when (length psDefaults > 1)
         $ throw $ ErrorCaseOverlapping xx

        -- Alts overlapping because the same ctor is used multiple times.
        when (length (nub nsCtorsMatched) /= length nsCtorsMatched )
         $ throw $ ErrorCaseOverlapping xx

        -- Check for alts overlapping because a default is not last.
        -- Also check there is at least one alternative.
        (case pats of
          [] -> throw $ ErrorCaseNoAlternatives xx

          _  |  or $ map isPDefault $ init pats 
             -> throw $ ErrorCaseOverlapping xx

             |  otherwise
             -> return ())

        -- Check the alternatives are exhaustive.
        (case mode of

          -- Small types have some finite number of constructors.
          DataModeSmall nsCtors
           -- If there is a default alternative then we've covered all the
           -- possibiliies. We know this we've also checked for overlap.
           | any isPDefault [p | AAlt p _ <- alts]
           -> return ()

           -- Look for unmatched constructors.
           | nsCtorsMissing <- nsCtors \\ nsCtorsMatched
           , not $ null nsCtorsMissing
           -> throw $ ErrorCaseNonExhaustive xx nsCtorsMissing

           -- All constructors were matched.
           | otherwise 
           -> return ()

          -- Large types have an effectively infinite number of constructors
          -- (like integer literals), so there needs to be a default alt.
          DataModeLarge 
           | any isPDefault [p | AAlt p _ <- alts] -> return ()
           | otherwise  
           -> throw $ ErrorCaseNonExhaustiveLarge xx)

        let effsMatch    
                = Sum.singleton kEffect 
                $ crushT $ tHeadRead tDiscrim

        return  ( tAlt
                , Sum.unions kEffect (effsDiscrim : effsMatch : effss)
                , Set.unions         (closDiscrim : closs) )


-- type cast -------------------------------------
-- Purify an effect, given a witness that it is pure.
checkExpM defs env xx@(XCast _ (CastPurify w) x1)
 = do   tW              <- checkWitnessM env w
        (t1, effs, clo) <- checkExpM defs env x1
                
        effs' <- case tW of
                  TApp (TCon (TyConWitness TwConPure)) effMask
                    -> return $ Sum.delete effMask effs
                  _ -> throw  $ ErrorWitnessNotPurity xx w tW

        return (t1, effs', clo)

-- Forget a closure, given a witness that it is empty.
checkExpM defs env xx@(XCast _ (CastForget w) x1)
 = do   tW               <- checkWitnessM env w
        (t1, effs, clos) <- checkExpM defs env x1

        clos' <- case tW of
                  TApp (TCon (TyConWitness TwConEmpty)) cloMask
                    -> return $ maskFromTaggedSet 
                                        (Sum.singleton kClosure cloMask)
                                        clos

                  _ -> throw $ ErrorWitnessNotEmpty xx w tW

        return (t1, effs, clos')


-- some other thing -----------------------------
checkExpM _defs _env _
 = error "typeOfExp: not handled yet"


-------------------------------------------------------------------------------
-- | Check a case alternative.
checkAltM 
        :: (Pretty n, Ord n) 
        => Exp a n              -- ^ Whole case expression, for error messages.
        -> DataDefs n           -- ^ Data type definitions.
        -> Env n                -- ^ Type environment.
        -> Type n               -- ^ Type of discriminant.
        -> [Type n]             -- ^ Args to type constructor of discriminant.
        -> Alt a n              -- ^ Alternative to check.
        -> CheckM a n (Type n, TypeSum n, Set (TaggedClosure n))

checkAltM _xx defs env _tDiscrim _tsArgs (AAlt PDefault xBody)
        = checkExpM defs env xBody

checkAltM xx defs env tDiscrim tsArgs (AAlt (PData uCon bsArg) xBody)
 = do   
        -- Take the type of the constructor and instantiate it with the 
        -- type arguments we got from the discriminant. 
        -- If the ctor type doesn't instantiate then it won't have enough foralls 
        -- on the front, which should have been checked by the def checker.
        let tCtor       = typeOfBound uCon
        tCtor_inst      
         <- case instantiateTs tCtor tsArgs of
             Nothing -> throw $ ErrorCaseCannotInstantiate xx tDiscrim tCtor
             Just t  -> return t
        
        -- Split the constructor type into the field and result types.
        let (tsFields_ctor, tResult) 
                        = takeTFunArgResult tCtor_inst

        -- The result type of the constructor must match the discriminant type.
        --  If it doesn't then the constructor in the pattern probably isn't for
        --  the discriminant type.
        when (tDiscrim /= tResult)
         $ throw $ ErrorCaseDiscrimTypeMismatch xx tDiscrim tResult

        -- There must be at least as many fields as variables in the pattern.
        -- It's ok to bind less fields than provided by the constructor.
        when (length tsFields_ctor < length bsArg)
         $ throw $ ErrorCaseTooManyBinders xx uCon 
                        (length tsFields_ctor)
                        (length bsArg)

        -- Merge the field types we get by instantiating the constructor
        -- type with possible annotations from the source program.
        -- If the annotations don't match, then we throw an error.
        tsFields        <- zipWithM (mergeAnnot xx)
                            (map typeOfBind bsArg)
                            tsFields_ctor        

        -- Extend the environment with the field types.
        let bsArg_subst = zipWith replaceTypeOfBind tsFields bsArg
        let env'        = Env.extends bsArg_subst env
        
        -- Check the body in this new environment.
        (tBody, effsBody, closBody) <- checkExpM defs env' xBody

        -- Mask bound variables from closure.
        let closBody_masked
             = foldr (\b c -> case takeSubstBoundOfBind b of
                               Nothing -> c
                               Just u  -> Set.delete (taggedClosureOfValBound u) c)
                     closBody bsArg

        return (tBody, effsBody, closBody_masked)


-- | Merge a type annotation on a pattern field with a type we get by
--   instantiating the constructor type.
mergeAnnot :: Eq n => Exp a n -> Type n -> Type n -> CheckM a n (Type n)
mergeAnnot xx tAnnot tActual
        -- Annotation is bottom, so just use the real type.
        | isBot tAnnot      = return tActual

        -- Annotation matches actual type, all good.
        | tAnnot == tActual = return tActual

        -- Annotation does not match actual type.
        | otherwise       
        = throw $ ErrorCaseFieldTypeMismatch xx tAnnot tActual


-------------------------------------------------------------------------------
-- | Check the set of witness bindings bound in a letregion for conflicts.
checkWitnessBindsM :: Ord n => Exp a n -> Bound n -> [Bind n] -> CheckM a n ()
checkWitnessBindsM xx nRegion bsWits
 = mapM_ (checkWitnessBindM xx nRegion bsWits) bsWits


checkWitnessBindM 
        :: Ord n 
        => Exp a n
        -> Bound n              -- ^ Region variable bound in the letregion.
        -> [Bind n]             -- ^ Other witness bindings in the same set.
        -> Bind  n              -- ^ The witness binding to check.
        -> CheckM a n ()

checkWitnessBindM xx uRegion bsWit bWit
 = let btsWit   
        = [(typeOfBind b, b) | b <- bsWit]

       -- Check the argument of a witness type is for the region we're
       -- introducing here.
       checkWitnessArg t
        = case t of
            TVar u'
             | uRegion /= u'    -> throw $ ErrorLetRegionWitnessOther xx uRegion bWit
             | otherwise        -> return ()

            TCon (TyConBound u')
             | uRegion /= u'    -> throw $ ErrorLetRegionWitnessOther xx uRegion bWit
             | otherwise        -> return ()

            -- The parser should ensure the right of a witness is a 
            -- constructor or variable.
            _ -> throw $ ErrorLetRegionWitnessInvalid xx bWit

   in  case typeOfBind bWit of
        TApp (TCon (TyConWitness TwConGlobal))  t2
         -> checkWitnessArg t2

        TApp (TCon (TyConWitness TwConConst))   t2
         | Just bConflict <- L.lookup (tMutable t2) btsWit
         -> throw $ ErrorLetRegionWitnessConflict xx bWit bConflict
         | otherwise    -> checkWitnessArg t2

        TApp (TCon (TyConWitness TwConMutable)) t2
         | Just bConflict <- L.lookup (tConst t2)   btsWit
         -> throw $ ErrorLetRegionWitnessConflict xx bWit bConflict
         | otherwise    -> checkWitnessArg t2

        TApp (TCon (TyConWitness TwConLazy))    t2
         | Just bConflict <- L.lookup (tManifest t2)  btsWit
         -> throw $ ErrorLetRegionWitnessConflict xx bWit bConflict
         | otherwise    -> checkWitnessArg t2

        TApp (TCon (TyConWitness TwConManifest))  t2
         | Just bConflict <- L.lookup (tLazy t2)    btsWit
         -> throw $ ErrorLetRegionWitnessConflict xx bWit bConflict
         | otherwise    -> checkWitnessArg t2

        _ -> throw $ ErrorLetRegionWitnessInvalid xx bWit


-------------------------------------------------------------------------------
-- | Check a type in the exp checking monad.
checkTypeM :: Ord n => Env n -> Type n -> CheckM a n (Kind n)
checkTypeM env tt
 = case T.checkType env tt of
        Left err        -> throw $ ErrorType err
        Right k         -> return k


-------------------------------------------------------------------------------
-- | Check the type annotation of a let bound variable against the type
--   inferred for the right of the binding.
--   If the annotation is Bot then we just replace the annotation,
--   otherwise it must match that for the right of the binding.
checkLetBindOfTypeM 
        :: (Eq n, Ord n) 
        => Exp a n -> Env n -> Type n -> Bind n 
        -> CheckM a n (Bind n, Kind n)

checkLetBindOfTypeM xx env tRight b
        -- If the annotation is Bot then just replace it.
        | isBot (typeOfBind b)
        = do    k       <- checkTypeM env tRight
                return  ( replaceTypeOfBind tRight b 
                        , k)

        -- The type of the binder must match that of the right of the binding.
        | typeOfBind b /= tRight
        = throw $ ErrorLetMismatch xx b tRight

        | otherwise
        = do    k       <- checkTypeM env (typeOfBind b)
                return (b, k)

     
