{-# LANGUAGE ViewPatterns #-}
-- | Type checker for the Disciple Core language.
module DDC.Core.Check.CheckExp
        ( Config (..)
        , AnTEC  (..)
        , checkExp
        , typeOfExp
        , CheckM
        , checkExpM
        , TaggedClosure(..))
where
import DDC.Core.Predicates
import DDC.Core.Compounds
import DDC.Core.Collect
import DDC.Core.Pretty
import DDC.Core.Exp
import DDC.Core.Check.Error
import DDC.Core.Check.CheckDaCon
import DDC.Core.Check.CheckWitness
import DDC.Core.Check.TaggedClosure
import DDC.Type.Transform.SubstituteT
import DDC.Type.Transform.Crush
import DDC.Type.Transform.Trim
import DDC.Type.Transform.Instantiate
import DDC.Type.Transform.LowerT
import DDC.Type.DataDef
import DDC.Type.Equiv
import DDC.Type.Universe
import DDC.Type.Sum                     as Sum
import DDC.Type.Env                     (Env)
import DDC.Type.Check.Monad             (result, throw)
import Data.Set                         (Set)
import qualified DDC.Type.Env           as Env
import qualified DDC.Type.Check         as T
import qualified Data.Set               as Set
import Control.Monad
import Data.List                        as L
import Data.Maybe
import Data.Typeable


-- Annot ----------------------------------------------------------------------
data AnTEC a n
        = AnTEC
        { annotType     :: Type    n
        , annotEffect   :: Effect  n
        , annotClosure  :: Closure n 
        , annotTail     :: a }
        deriving (Show, Typeable)

instance Pretty (AnTEC a n) where
 ppr _ = text "AnTEC"        


-- Wrappers -------------------------------------------------------------------
-- | Type check an expression. 
--
--   If it's good, you get a new version with types attached to all the bound
--   variables, as well its the type, effect and closure. 
--
--   If it's bad, you get a description of the error.
--
--   The returned expression has types attached to all variable occurrences, 
--   so you can call `typeOfExp` on any open subterm.
checkExp 
        :: (Ord n, Show n, Pretty n)
        => Config n             -- ^ Static config.
        -> Env n                -- ^ Kind environment.
        -> Env n                -- ^ Type environment.
        -> Exp a n              -- ^ Expression to check.
        -> Either (Error a n)
                  ( Exp (AnTEC a n) n
                  , Type n
                  , Effect n
                  , Closure n)

checkExp config kenv tenv xx 
 = result
 $ do   (xx', t, effs, clos) <- checkExpM config kenv tenv xx
        return  ( xx'
                , t
                , TSum effs
                , closureOfTaggedSet clos)


-- | Like `checkExp`, but only return the value type of an expression.
typeOfExp 
        :: (Ord n, Pretty n, Show n)
        => Config n             -- ^ Static config.
        -> Env n                -- ^ Kind environment
        -> Env n                -- ^ Type environment.
        -> Exp a n              -- ^ Expression to check.
        -> Either (Error a n) (Type n)
typeOfExp config kenv tenv xx 
 = case checkExp config kenv tenv xx of
        Left err           -> Left err
        Right (_, t, _, _) -> Right t


-- checkExp -------------------------------------------------------------------
-- | Like `checkExp` but using the `CheckM` monad to handle errors.
checkExpM 
        :: (Show n, Pretty n, Ord n)
        => Config n             -- ^ Static config.
        -> Env n                -- ^ Kind environment.
        -> Env n                -- ^ Type environment.
        -> Exp a n              -- ^ Expression to check.
        -> CheckM a n 
                ( Exp (AnTEC a n) n
                , Type n
                , TypeSum n
                , Set (TaggedClosure n))

checkExpM config kenv tenv xx
 = checkExpM' config kenv tenv xx
{-
 = do (xx', t, eff, clo) <- checkExpM' config kenv tenv xx
      trace (renderIndent $ vcat 
                [ text "checkExpM:  " <+> ppr xx 
                , text "        ::  " <+> ppr t 
                , text "        :!: " <+> ppr eff
                , text "        :$: " <+> ppr clo
                , text ""])
         $ return (xx', t, eff, clo)
-}
-- variables ------------------------------------
checkExpM' _config _kenv tenv (XVar a u)
 = case Env.lookup u tenv of
        Nothing -> throw $ ErrorUndefinedVar u UniverseData
        Just t  
         -> returnX a 
                (\z -> XVar z u)
                t
                (Sum.empty kEffect)
                (Set.singleton $ taggedClosureOfValBound t u)


-- constructors ---------------------------------
checkExpM' config _kenv _tenv xx@(XCon a dc)
 = do   
        -- All data constructors need to have valid type annotations.
        when (isBot $ daConType dc)
         $ throw $ ErrorUndefinedCtor xx

        -- Check that the constructor is in the data type declarations.
        checkDaConM config xx dc

        let tResult     = typeOfDaCon dc
        returnX a
                (\z -> XCon z dc)
                tResult
                (Sum.empty kEffect)
                (Set.empty)


-- application ------------------------------------
-- value-type application.
--
-- Note: We don't need to substitute into the effect of x1 (effs1)
--       because the body of a type abstraction is required to be pure.
-- 
--       We don't need to substitute into the closure either, because
--       the bound type variable is not visible outside the abstraction.
--       thus we can't be sharing objects that have it in its type.
--
checkExpM' config kenv tenv xx@(XApp a x1 (XType t2))
 = do   (x1', t1, effs1, clos1) <- checkExpM  config kenv tenv x1

        -- Check the type argument.
        k2                      <- checkTypeM config kenv t2

        -- Take any Use annots from a region arg.
        --   This always matches because we just checked 't2'
        let Just t2_clo         = taggedClosureOfTyArg kenv t2

        case t1 of
         TForall b11 t12
          | typeOfBind b11 == k2
          -> returnX a
                (\z -> XApp z x1' (XType t2))
                (substituteT b11 t2 t12)
                effs1   
                (clos1 `Set.union` t2_clo)

          | otherwise   -> throw $ ErrorAppMismatch xx (typeOfBind b11) t2
         _              -> throw $ ErrorAppNotFun   xx t1 t2


-- value-witness application.
checkExpM' config kenv tenv xx@(XApp a x1 (XWitness w2))
 = do   (x1', t1, effs1, clos1) <- checkExpM     config kenv tenv x1
        t2                      <- checkWitnessM config kenv tenv w2
        case t1 of
         TApp (TApp (TCon (TyConWitness TwConImpl)) t11) t12
          | t11 `equivT` t2   
          -> returnX a
                (\z -> XApp z x1' (XWitness w2))
                t12 effs1 clos1

          | otherwise   -> throw $ ErrorAppMismatch xx t11 t2
         _              -> throw $ ErrorAppNotFun   xx t1 t2
                 

-- value-value application.
checkExpM' config kenv tenv xx@(XApp a x1 x2)
 = do   (x1', t1, effs1, clos1)    <- checkExpM config kenv tenv x1
        (x2', t2, effs2, clos2)    <- checkExpM config kenv tenv x2

        -- Note: we don't need to use the closure of the function because
        --       all of its components will already be part of clos1 above.
        case t1 of
         TApp (TApp (TApp (TApp (TCon (TyConSpec TcConFun)) t11) eff) _clo) t12
          | t11 `equivT` t2   
          , effs    <- Sum.fromList kEffect  [eff]
          -> returnX a
                (\z -> XApp z x1' x2')
                t12
                (effs1 `Sum.union` effs2 `Sum.union` effs)
                (clos1 `Set.union` clos2)

          | otherwise   -> throw $ ErrorAppMismatch xx t11 t2
         _              -> throw $ ErrorAppNotFun xx t1 t2


-- spec abstraction -----------------------------
checkExpM' config kenv tenv xx@(XLAM a b1 x2)
 = do   let t1            = typeOfBind b1
        _                 <- checkTypeM config kenv t1

        -- Check the body
        let kenv'         = Env.extend b1 kenv
        let tenv'         = Env.lift   1  tenv
        (x2', t2, e2, c2) <- checkExpM  config kenv' tenv' x2
        k2                <- checkTypeM config kenv' t2

        when (Env.memberBind b1 kenv)
         $ throw $ ErrorLamShadow xx b1

        -- The body of a spec abstraction must be pure.
        when (e2 /= Sum.empty kEffect)
         $ throw $ ErrorLamNotPure xx (TSum e2)

        -- The body of a spec abstraction must have data kind.
        when (not $ isDataKind k2)
         $ throw $ ErrorLamBodyNotData xx b1 t2 k2

        -- Mask closure terms due to locally bound region vars.
        let c2_cut      = Set.fromList
                        $ mapMaybe (cutTaggedClosureT b1)
                        $ Set.toList c2

        returnX a
                (\z -> XLAM z b1 x2')
                (TForall b1 t2)
                (Sum.empty kEffect)
                c2_cut
         

-- function abstractions ------------------------
checkExpM' config kenv tenv xx@(XLam a b1 x2)
 = do   let t1               =  typeOfBind b1
        k1                   <- checkTypeM config kenv t1

        -- Check the body.
        let tenv'            =  Env.extend b1 tenv
        (x2', t2, e2, c2)    <- checkExpM  config kenv tenv' x2   
        k2                   <- checkTypeM config kenv t2

        -- The form of the function constructor depends on what universe the 
        -- binder is in.
        case universeFromType2 k1 of
         Just UniverseData
          |  not $ isDataKind k1     -> throw $ ErrorLamBindNotData xx t1 k1
          |  not $ isDataKind k2     -> throw $ ErrorLamBodyNotData xx b1 t2 k2 
          |  otherwise
          -> let 
                 -- Cut closure terms due to locally bound value vars.
                 -- This also lowers deBruijn indices in un-cut closure terms.
                 c2_cut  = Set.fromList
                         $ mapMaybe (cutTaggedClosureX b1)
                         $ Set.toList c2

                 -- Trim the closure before we annotate the returned function
                 -- type with it. 
                 --  This should always succeed because trimClosure only returns
                 --  Nothing if the closure is miskinded, and we've already
                 --  allready checked that.
                 Just c2_captured
                  | configSuppressClosures config
                  = Just $ tBot kClosure

                  | otherwise
                  = trimClosure $ closureOfTaggedSet c2_cut

             in  returnX a
                        (\z -> XLam z b1 x2')
                        (tFun t1 (TSum e2) c2_captured t2)
                        (Sum.empty kEffect)
                        c2_cut

         Just UniverseWitness
          | e2 /= Sum.empty kEffect  -> throw $ ErrorLamNotPure     xx (TSum e2)
          | not $ isDataKind k2      -> throw $ ErrorLamBodyNotData xx b1 t2 k2
          | otherwise                
          -> returnX a
                (\z -> XLam z b1 x2')
                (tImpl t1 t2)
                (Sum.empty kEffect)
                c2

         _ -> throw $ ErrorMalformedType xx k1


-- let --------------------------------------------
checkExpM' config kenv tenv xx@(XLet a lts x2)
 | case lts of
        LLet{}  -> True
        LRec{}  -> True
        _       -> False

 = do
        -- Check the bindings
        (lts', bs', effs12, clo12)
                <- checkLetsM xx config kenv tenv lts

        -- Check the body expression.
        let tenv1  = Env.extends bs' tenv
        (x2', t2, effs2, c2)    <- checkExpM config kenv tenv1 x2

        -- The body should have data kind.
        k2       <- checkTypeM config kenv t2
        when (not $ isDataKind k2)
         $ throw $ ErrorLetBodyNotData xx t2 k2

        -- Mask closure terms due to locally bound value vars.
        let c2_cut      = Set.fromList
                        $ mapMaybe (cutTaggedClosureXs bs')
                        $ Set.toList c2

        returnX a
                (\z -> XLet z lts' x2')
                t2
                (effs12 `Sum.union` effs2)
                (clo12  `Set.union` c2_cut)


-- letregion --------------------------------------
checkExpM' config kenv tenv xx@(XLet a (LLetRegions b bs) x)
 = case (takeSubstBoundsOfBinds b) of
     Nothing -> checkExpM config kenv tenv x     
     Just us
      -> do
        -- Check the type on the region binders.
        let ks   = map typeOfBind b
        mapM_ (checkTypeM config kenv) ks

        -- The binders must have region kind.
        when (any (not . isRegionKind) ks) 
         $ throw $ ErrorLetRegionsNotRegion xx b ks

        -- We can't shadow region binders because we might have witnesses
        -- in the environment that conflict with the ones created here.
        let rebounds = filter (flip Env.memberBind kenv) b
        when (not $ null rebounds)
         $ throw $ ErrorLetRegionsRebound xx rebounds
        
        -- Check the witness types.
        let kenv'       = Env.extends b kenv
        let tenv'       = Env.lift 1 tenv
        mapM_ (checkTypeM config kenv') $ map typeOfBind bs

        -- Check that the witnesses bound here are for the region,
        -- and they don't conflict with each other.
        checkWitnessBindsM kenv xx us bs

        -- Check the body expression.
        let tenv2       = Env.extends bs tenv'
        (xBody', tBody, effs, clo)  <- checkExpM config kenv' tenv2 x

        -- The body type must have data kind.
        kBody           <- checkTypeM config kenv' tBody
        when (not $ isDataKind kBody)
         $ throw $ ErrorLetBodyNotData xx tBody kBody

        -- The bound region variable cannot be free in the body type.
        let fvsT         = freeT Env.empty tBody
        when (any (flip Set.member fvsT) us)
         $ throw $ ErrorLetRegionFree xx b tBody
        
        -- Delete effects on the bound region from the result.
        let delEff es u = Sum.delete (tRead  (TVar u))
                        $ Sum.delete (tWrite (TVar u))
                        $ Sum.delete (tAlloc (TVar u))
                        $ es
        let effs'       = foldl delEff effs us 

        -- Delete the bound region variable from the closure.
        -- Mask closure terms due to locally bound region vars.
        let cutClo c r  = mapMaybe (cutTaggedClosureT r) c
        let c2_cut      = Set.fromList 
                        $ foldl cutClo (Set.toList clo) b

        returnX a
                (\z -> XLet z (LLetRegions b bs) xBody')
                (lowerT 1 tBody)
                (lowerT 1 effs')
                c2_cut


-- withregion -----------------------------------
checkExpM' config kenv tenv xx@(XLet a (LWithRegion u) x)
 = do
        -- The handle must have region kind.
        (case Env.lookup u kenv of
          Nothing -> throw $ ErrorUndefinedVar u UniverseSpec

          Just k  |  not $ isRegionKind k
                  -> throw $ ErrorWithRegionNotRegion xx u k

          _       -> return ())
        
        -- Check the body expression.
        (xBody', tBody, effs, clo) 
               <- checkExpM config kenv tenv x

        -- The body type must have data kind.
        kBody  <- checkTypeM config kenv tBody
        when (not $ isDataKind kBody)
         $ throw $ ErrorLetBodyNotData xx tBody kBody
        
        -- Delete effects on the bound region from the result.
        let tu          = TCon $ TyConBound u kRegion
        let effs'       = Sum.delete (tRead  tu)
                        $ Sum.delete (tWrite tu)
                        $ Sum.delete (tAlloc tu)
                        $ effs
        
        -- Delete the bound region handle from the closure.
        let clo_masked  = Set.delete (GBoundRgnCon u) clo

        returnX a
                (\z -> XLet z (LWithRegion u) xBody')
                tBody
                effs'
                clo_masked
                

-- case expression ------------------------------
checkExpM' config kenv tenv xx@(XCase a xDiscrim alts)
 = do   
        -- Check the discriminant.
        (xDiscrim', tDiscrim, effsDiscrim, closDiscrim) 
         <- checkExpM config kenv tenv xDiscrim

        -- Split the type into the type constructor names and type parameters.
        -- Also check that it's algebraic data, and not a function or effect
        -- type etc. 
        (mmode, tsArgs)
         <- case takeTyConApps tDiscrim of
                Just (tc, ts)
                 | TyConSpec TcConUnit         <- tc
                 -> return ( Just (DataModeSmall [])
                           , [] )
                                -- TODO: the DataModeSmall should hold DaCons instead of
                                --       names, as we don't have a name for Unit.

                 | TyConBound (UName nTyCon) k <- tc
                 , takeResultKind k == kData
                 -> return ( lookupModeOfDataType nTyCon (configDataDefs config)
                           , ts )
                      
                 | TyConBound (UPrim nTyCon _) k <- tc
                 , takeResultKind k == kData
                 -> return ( lookupModeOfDataType nTyCon (configDataDefs config)
                           , ts )

                _ -> throw $ ErrorCaseDiscrimNotAlgebraic xx tDiscrim

        -- Get the mode of the data type, 
        --   this tells us how many constructors there are.
        mode    
         <- case mmode of
             Nothing -> throw $ ErrorCaseDiscrimTypeUndeclared xx tDiscrim
             Just m  -> return m

        -- Check the alternatives.
        (alts', ts, effss, closs)     
                <- liftM unzip4
                $  mapM (checkAltM xx config kenv tenv tDiscrim tsArgs) alts

        -- There must be at least one alternative
        when (null ts)
         $ throw $ ErrorCaseNoAlternatives xx

        -- All alternative result types must be identical.
        let (tAlt : _)  = ts
        forM_ ts $ \tAlt' 
         -> when (not $ equivT tAlt tAlt') 
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
                $ crushEffect $ tHeadRead tDiscrim

        returnX a
                (\z -> XCase z xDiscrim' alts')
                tAlt
                (Sum.unions kEffect (effsDiscrim : effsMatch : effss))
                (Set.unions         (closDiscrim : closs))


-- type cast -------------------------------------
-- Weaken an effect, adding in the given terms.
checkExpM' config kenv tenv xx@(XCast a (CastWeakenEffect eff) x1)
 = do
        -- Check the effect term.
        kEff    <- checkTypeM config kenv eff
        when (not $ isEffectKind kEff)
         $ throw $ ErrorMaxeffNotEff xx eff kEff

        -- Check the body.
        (x1', t1, effs, clo)    <- checkExpM config kenv tenv x1
        let c'                  = CastWeakenEffect eff

        returnX a
                (\z -> XCast z c' x1')
                t1
                (Sum.insert eff effs)
                clo


-- Weaken a closure, adding in the given terms.
checkExpM' config kenv tenv (XCast a (CastWeakenClosure xs) x1)
 = do
        -- Check the contained expressions.
        (xs', closs)
                <- liftM unzip
                $ mapM (checkArgM config kenv tenv) xs

        -- Check the body.
        (x1', t1, effs, clos)   <- checkExpM config kenv tenv x1
        let c'                  = CastWeakenClosure xs'

        returnX a
                (\z -> XCast z c' x1')
                t1
                effs
                (Set.unions (clos : closs))


-- Purify an effect, given a witness that it is pure.
checkExpM' config kenv tenv xx@(XCast a (CastPurify w) x1)
 = do
        tW                   <- checkWitnessM config kenv tenv w
        (x1', t1, effs, clo) <- checkExpM     config kenv tenv x1
                
        effs' <- case tW of
                  TApp (TCon (TyConWitness TwConPure)) effMask
                    -> return $ Sum.delete effMask effs
                  _ -> throw  $ ErrorWitnessNotPurity xx w tW

        let c'  = CastPurify w

        returnX a
                (\z -> XCast z c' x1')
                t1 effs' clo


-- Forget a closure, given a witness that it is empty.
checkExpM' config kenv tenv xx@(XCast a (CastForget w) x1)
 = do   
        tW                    <- checkWitnessM config kenv tenv w
        (x1', t1, effs, clos) <- checkExpM     config kenv tenv x1

        clos' <- case tW of
                  TApp (TCon (TyConWitness TwConEmpty)) cloMask
                    -> return $ maskFromTaggedSet 
                                        (Sum.singleton kClosure cloMask)
                                        clos

                  _ -> throw $ ErrorWitnessNotEmpty xx w tW

        let c'  = CastForget w

        returnX a
                (\z -> XCast z c' x1')
                t1 effs clos'


-- Type and witness expressions can only appear as the arguments 
-- to  applications.
checkExpM' _config _kenv _tenv xx@(XType _)
        = throw $ ErrorNakedType xx 

checkExpM' _config _kenv _tenv xx@(XWitness _)
        = throw $ ErrorNakedWitness xx

checkExpM' _ _ _ _
        = error "checkExpM: bogus warning killer"


-- | Like `checkExp` but we allow naked types and witnesses.
checkArgM 
        :: (Show n, Pretty n, Ord n)
        => Config n             -- ^ Static config.
        -> Env n                -- ^ Kind environment.
        -> Env n                -- ^ Type environment.
        -> Exp a n              -- ^ Expression to check.
        -> CheckM a n 
                ( Exp (AnTEC a n) n
                , Set (TaggedClosure n))

checkArgM config kenv tenv xx
 = case xx of
        XType t
         -> do  checkTypeM config kenv t
                let Just clo = taggedClosureOfTyArg kenv t

                return  ( XType t
                        , clo)

        XWitness w
         -> do  checkWitnessM config kenv tenv w
                return  ( XWitness w
                        , Set.empty)

        _ -> do
                (xx', _, _, clos) <- checkExpM config kenv tenv xx
                return  ( xx'
                        , clos)


-- | Helper function for building the return value of checkExpM'
--   It builts the AnTEC annotation and attaches it to the new AST node,
--   as well as returning the current effect and closure in the appropriate
--   form as part of the tuple.
returnX :: Ord n 
        => a
        -> (AnTEC a n -> Exp (AnTEC a n) n)
        -> Type n 
        -> TypeSum n
        -> Set (TaggedClosure n)
        -> CheckM a n 
                ( Exp (AnTEC a n) n
                , Type n
                , TypeSum n
                , Set (TaggedClosure n))

returnX a f t es cs
 = let  e       = TSum es
        c       = closureOfTaggedSet cs
   in   return  (f (AnTEC t e c a)
                , t, es, cs)


-------------------------------------------------------------------------------
-- | Check some let bindings.
checkLetsM 
        :: (Show n, Pretty n, Ord n)
        => Exp a n              -- ^ Enclosing expression, for error messages.
        -> Config n             -- ^ Static config.
        -> Env n                -- ^ Kind environment.
        -> Env n                -- ^ Type environment.
        -> Lets a n
        -> CheckM a n
                ( Lets (AnTEC a n) n
                , [Bind n]
                , TypeSum n
                , Set (TaggedClosure n))

checkLetsM xx config kenv tenv (LLet mode b11 x12)
 = do   
        -- Check the right of the binding.
        (x12', t12, effs12, clo12)  
         <- checkExpM config kenv tenv x12

        -- Check binder annotation against the type we inferred for the right.
        (b11', k11')    
         <- checkLetBindOfTypeM xx config kenv tenv t12 b11

        -- The right of the binding should have data kind.
        when (not $ isDataKind k11')
         $ throw $ ErrorLetBindingNotData xx b11' k11'
          
        -- Check purity and emptiness for lazy bindings.
        (case mode of
          LetStrict     -> return ()
          LetLazy _
           -> do let eff12' = TSum effs12
                 when (not $ isBot eff12')
                  $ throw $ ErrorLetLazyNotPure xx b11 eff12'

                 let clo12' = closureOfTaggedSet clo12
                 when (not $ isBot clo12')
                  $ throw $ ErrorLetLazyNotEmpty xx b11 clo12')

        -- Check region witness for lazy bindings.
        (case mode of
          LetStrict     -> return ()

          -- Type of lazy binding has no head region, like Unit and (->).
          LetLazy Nothing
           -> do case takeDataTyConApps t12 of
                  Just (_tc, t1 : _)
                   ->  do k1 <- checkTypeM config kenv t1
                          when (isRegionKind k1)
                           $ throw $ ErrorLetLazyNoWitness xx b11 t12

                  _ -> return ()

          -- Type of lazy binding might have a head region,
          -- so we need a Lazy witness for it.
          LetLazy (Just wit)
           -> do tWit        <- checkWitnessM config kenv tenv wit
                 let tWitExp =  case takeDataTyConApps t12 of
                                 Just (_tc, tR : _ts) -> tLazy tR
                                 _                    -> tHeadLazy t12

                 when (not $ equivT tWit tWitExp)
                  $ throw $ ErrorLetLazyWitnessTypeMismatch 
                                 xx b11 tWit t12 tWitExp)
        
        return  ( LLet mode b11' x12'
                , [b11']
                , effs12
                , clo12)

-- letrec ---------------------------------------
checkLetsM xx config kenv tenv (LRec bxs)
 = do   
        let (bs, xs)    = unzip bxs

        -- Check all the annotations.
        ks              <- mapM (checkTypeM config kenv) 
                        $  map typeOfBind bs

        -- Check all the annots have data kind.
        zipWithM_ (\b k
         -> when (not $ isDataKind k)
                $ throw $ ErrorLetBindingNotData xx b k)
                bs ks

        -- All right hand sides need to be lambdas.
        forM_ xs $ \x 
         -> when (not $ (isXLam x || isXLAM x))
                $ throw $ ErrorLetrecBindingNotLambda xx x

        -- All variables are in scope in all right hand sides.
        let tenv'       = Env.extends bs tenv

        -- Check the right hand sides.
        (xsRight', tsRight, _effssBinds, clossBinds) 
                <- liftM unzip4 $ mapM (checkExpM config kenv tenv') xs

        -- Check annots on binders against inferred types of the bindings.
        zipWithM_ (\b t
                -> if not $ equivT (typeOfBind b) t
                        then throw $ ErrorLetMismatch xx b t
                        else return ())
                bs tsRight

        -- Cut closure terms due to locally bound value vars.
        let clos_cut 
                = Set.fromList
                $ mapMaybe (cutTaggedClosureXs bs)
                $ Set.toList 
                $ Set.unions clossBinds

        return  ( LRec (zip bs xsRight')
                , zipWith replaceTypeOfBind tsRight bs
                , Sum.empty kEffect
                , clos_cut)


checkLetsM _xx _config _kenv _tenv _lts
        = error "checkLetsM: not done yet"


-------------------------------------------------------------------------------
-- | Check a case alternative.
checkAltM 
        :: (Show n, Pretty n, Ord n) 
        => Exp a n              -- ^ Whole case expression, for error messages.
        -> Config n             -- ^ Data type definitions.
        -> Env n                -- ^ Kind environment.
        -> Env n                -- ^ Type environment.
        -> Type n               -- ^ Type of discriminant.
        -> [Type n]             -- ^ Args to type constructor of discriminant.
        -> Alt a n              -- ^ Alternative to check.
        -> CheckM a n 
                ( Alt (AnTEC a n) n
                , Type n
                , TypeSum n
                , Set (TaggedClosure n))

checkAltM _xx config kenv tenv _tDiscrim _tsArgs (AAlt PDefault xBody)
 = do   (xBody', tBody, effBody, cloBody)
                <- checkExpM config kenv tenv xBody

        return  ( AAlt PDefault xBody'
                , tBody
                , effBody
                , cloBody)

checkAltM xx config kenv tenv tDiscrim tsArgs (AAlt (PData dc bsArg) xBody)
 = do   
        -- Take the type of the constructor and instantiate it with the 
        -- type arguments we got from the discriminant. 
        -- If the ctor type doesn't instantiate then it won't have enough foralls 
        -- on the front, which should have been checked by the def checker.
        let tCtor = daConType dc

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
        when (not $ equivT tDiscrim tResult)
         $ throw $ ErrorCaseDiscrimTypeMismatch xx tDiscrim tResult

        -- There must be at least as many fields as variables in the pattern.
        -- It's ok to bind less fields than provided by the constructor.
        when (length tsFields_ctor < length bsArg)
         $ throw $ ErrorCaseTooManyBinders xx dc
                        (length tsFields_ctor)
                        (length bsArg)

        -- Merge the field types we get by instantiating the constructor
        -- type with possible annotations from the source program.
        -- If the annotations don't match, then we throw an error.
        tsFields        <- zipWithM (mergeAnnot xx)
                            (map typeOfBind bsArg)
                            tsFields_ctor        

        -- Extend the environment with the field types.
        let bsArg'      = zipWith replaceTypeOfBind tsFields bsArg
        let tenv'       = Env.extends bsArg' tenv
        
        -- Check the body in this new environment.
        (xBody', tBody, effsBody, closBody)
                <- checkExpM config kenv tenv' xBody

        -- Cut closure terms due to locally bound value vars.
        -- This also lowers deBruijn indices in un-cut closure terms.
        let closBody_cut 
                = Set.fromList
                $ mapMaybe (cutTaggedClosureXs bsArg')
                $ Set.toList closBody

        return  ( AAlt (PData dc bsArg') xBody'
                , tBody
                , effsBody
                , closBody_cut)


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
checkWitnessBindsM :: (Show n, Ord n) => Env n -> Exp a n -> [Bound n] -> [Bind n] -> CheckM a n ()
checkWitnessBindsM kenv xx nRegions bsWits
 = mapM_ (checkWitnessBindM kenv xx nRegions bsWits) bsWits


checkWitnessBindM 
        :: (Show n, Ord n)
        => Env n
        -> Exp a n
        -> [Bound n]            -- ^ Region variables bound in the letregion.
        -> [Bind n]             -- ^ Other witness bindings in the same set.
        -> Bind  n              -- ^ The witness binding to check.
        -> CheckM a n ()

checkWitnessBindM kenv xx uRegions bsWit bWit
 = let btsWit   
        = [(typeOfBind b, b) | b <- bsWit]

       -- Check the argument of a witness type is for the region we're
       -- introducing here.
       checkWitnessArg t
        = case t of
            TVar u'
             | all (/= u') uRegions -> throw $ ErrorLetRegionsWitnessOther xx uRegions bWit
             | otherwise            -> return ()

            TCon (TyConBound u' _)
             | all (/= u') uRegions -> throw $ ErrorLetRegionsWitnessOther xx uRegions bWit
             | otherwise            -> return ()
            
            -- The parser should ensure the right of a witness is a 
            -- constructor or variable.
            _ -> throw $ ErrorLetRegionWitnessInvalid xx bWit
            
       isBound t
        = case t of
            TVar u'                | Env.member u' kenv -> True
            TCon (TyConBound u' _) | Env.member u' kenv -> True
            _                                           -> False 
       
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
         
        (takeTyConApps -> Just (TyConWitness (TwConDistinct _), ts))
         | ts /= (nub ts) -> throw $ ErrorLetRegionWitnessInvalid xx bWit         
         | notBounds <- filter (not . isBound) ts
         , not $ null notBounds
         -> mapM_ checkWitnessArg notBounds
         | otherwise      -> throw $ ErrorLetRegionWitnessFree    xx bWit
         
        _ -> throw $ ErrorLetRegionWitnessInvalid xx bWit


-------------------------------------------------------------------------------
-- | Check a type in the exp checking monad.
checkTypeM :: (Ord n, Show n, Pretty n) 
           => Config n 
           -> Env n 
           -> Type n 
           -> CheckM a n (Kind n)

checkTypeM config kenv tt
 = case T.checkType (configDataDefs config) kenv tt of
        Left err        -> throw $ ErrorType err
        Right k         -> return k


-------------------------------------------------------------------------------
-- | Check the type annotation of a let bound variable against the type
--   inferred for the right of the binding.
--   If the annotation is Bot then we just replace the annotation,
--   otherwise it must match that for the right of the binding.
checkLetBindOfTypeM 
        :: (Eq n, Ord n, Show n, Pretty n) 
        => Exp a n 
        -> Config n             -- Data type definitions.
        -> Env n                -- Kind environment. 
        -> Env n                -- Type environment.
        -> Type n 
        -> Bind n 
        -> CheckM a n (Bind n, Kind n)

checkLetBindOfTypeM xx config kenv _tenv tRight b
        -- If the annotation is Bot then just replace it.
        | isBot (typeOfBind b)
        = do    k       <- checkTypeM config kenv tRight
                return  ( replaceTypeOfBind tRight b 
                        , k)

        -- The type of the binder must match that of the right of the binding.
        | not $ equivT (typeOfBind b) tRight
        = throw $ ErrorLetMismatch xx b tRight

        | otherwise
        = do    k       <- checkTypeM config kenv (typeOfBind b)
                return (b, k)
