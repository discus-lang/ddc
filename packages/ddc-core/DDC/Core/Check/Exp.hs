-- | Type checker for the Disciple Core language.
module DDC.Core.Check.Exp
        ( Config (..)
        , tableOfConfig
        , AnTEC  (..)
        , checkExp
        , typeOfExp
        , CheckM
        , checkExpM
        , TaggedClosure(..))
where
import DDC.Core.Check.Exp.VarCon
import DDC.Core.Check.Exp.Abs
import DDC.Core.Check.Exp.App
import DDC.Core.Check.Exp.Let
import DDC.Core.Check.Exp.Base

import DDC.Core.Transform.Reannotate
import DDC.Type.Transform.Crush
import DDC.Type.Transform.Instantiate
import DDC.Type.Sum                     as Sum
import qualified DDC.Type.Env           as Env
import qualified Data.Set               as Set
import qualified Data.Map               as Map
import DDC.Data.ListUtils
import Data.List                        as L


-- Table ----------------------------------------------------------------------
tableOfConfig :: Config n -> Table a n
tableOfConfig config
        = Table
        { tableConfig           = config
        , tableCheckExp         = checkExpM
        , tableCheckVarCon      = checkVarCon
        , tableCheckApp         = checkApp
        , tableCheckAbs         = checkAbs
        , tableCheckLet         = checkLet
        , tableCheckCase        = error "nup"
        , tableCheckCast        = error "nup" }


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
--
--   The kinds and types of primitives are added to the environments 
--   automatically, you don't need to supply these as part of the 
--   starting environments.
--
checkExp 
        :: (Ord n, Show n, Pretty n)
        => Config n             -- ^ Static configuration.
        -> KindEnv n            -- ^ Starting Kind environment.
        -> TypeEnv n            -- ^ Starting Type environment.
        -> Exp a n              -- ^ Expression to check.
        -> Maybe (Type n)       -- ^ Expected type, if any.
        -> Either (Error a n)
                  ( Exp (AnTEC a n) n
                  , Type n
                  , Effect n
                  , Closure n)

checkExp !config !kenv !tenv !xx !tXX
 = result
 $ do   (xx', t, effs, clos) 
                <- checkExpM (tableOfConfig config)
                        (Env.union kenv (configPrimKinds config))
                        (Env.union tenv (configPrimTypes config))
                        xx tXX
        return  ( xx'
                , t
                , TSum effs
                , closureOfTaggedSet clos)


-- | Like `checkExp`, but only return the value type of an expression.
typeOfExp 
        :: (Ord n, Pretty n, Show n)
        => Config n             -- ^ Static configuration.
        -> KindEnv n            -- ^ Starting Kind environment
        -> TypeEnv n            -- ^ Starting Type environment.
        -> Exp a n              -- ^ Expression to check.
        -> Maybe  (Type n)      -- ^ Expected type, if any.
        -> Either (Error a n) (Type n)

typeOfExp !config !kenv !tenv !xx !tXX
 = case checkExp config kenv tenv xx tXX of
        Left err           -> Left err
        Right (_, t, _, _) -> Right t


-- checkExp --------------------a-----------------------------------------------
-- | Like `checkExp` but using the `CheckM` monad to handle errors.
checkExpM 
        :: (Show n, Pretty n, Ord n)
        => Table a n            -- ^ Static config.
        -> Env n                -- ^ Kind environment.
        -> Env n                -- ^ Type environment.
        -> Exp a n              -- ^ Expression to check.
        -> Maybe (Type n)       -- ^ Expected type, if any.
        -> CheckM a n 
                ( Exp (AnTEC a n) n
                , Type n
                , TypeSum n
                , Set (TaggedClosure n))

checkExpM !config !kenv !tenv !xx !tXX
 = checkExpM' config kenv tenv xx tXX

checkExpM' !table !kenv !tenv !xx@XVar{} !tXX
 = tableCheckVarCon table table kenv tenv xx tXX

checkExpM' !table !kenv !tenv !xx@XCon{} !tXX
 = tableCheckVarCon table table kenv tenv xx tXX

checkExpM' !table !kenv !tenv !xx@XApp{} !tXX
 = tableCheckApp    table table kenv tenv xx tXX

checkExpM' !table !kenv !tenv !xx@XLam{} !tXX
 = tableCheckAbs    table table kenv tenv xx tXX

checkExpM' !table !kenv !tenv !xx@XLAM{} !tXX
 = tableCheckAbs    table table kenv tenv xx tXX

checkExpM' !table !kenv !tenv !xx@XLet{} !tXX
 = tableCheckLet    table table kenv tenv xx tXX


-- case expression ------------------------------
checkExpM' !table !kenv !tenv xx@(XCase a xDiscrim alts) _
 = do   let config      = tableConfig table

        -- Check the discriminant.
        (xDiscrim', tDiscrim, effsDiscrim, closDiscrim) 
         <- checkExpM table kenv tenv xDiscrim Nothing

        -- Split the type into the type constructor names and type parameters.
        -- Also check that it's algebraic data, and not a function or effect
        -- type etc. 
        (mmode, tsArgs)
         <- case takeTyConApps tDiscrim of
                Just (tc, ts)
                 | TyConSpec TcConUnit         <- tc
                 -> return ( Just (DataModeSmall [])
                           , [] )
                        -- ISSUE #269: Refactor DataModeSmall to hold DaCons
                        --  instead of names. The DataModeSmall should hold
                        --  DaCons instead of names, as we don't have a name
                        --  for Unit.

                 | TyConBound (UName nTyCon) k <- tc
                 , takeResultKind k == kData
                 -> return ( lookupModeOfDataType nTyCon (configDataDefs config)
                           , ts )
                      
                 | TyConBound (UPrim nTyCon _) k <- tc
                 , takeResultKind k == kData
                 -> return ( lookupModeOfDataType nTyCon (configDataDefs config)
                           , ts )

                _ -> throw $ ErrorCaseScrutineeNotAlgebraic xx tDiscrim

        -- Get the mode of the data type, 
        --   this tells us how many constructors there are.
        mode    
         <- case mmode of
             Nothing -> throw $ ErrorCaseScrutineeTypeUndeclared xx tDiscrim
             Just m  -> return m

        -- Check the alternatives.
        (alts', ts, effss, closs)     
                <- liftM unzip4
                $  mapM (checkAltM xx table kenv tenv tDiscrim tsArgs) alts

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

          _  |  Just patsInit <- takeInit pats
             ,  or $ map isPDefault $ patsInit
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
checkExpM' !table !kenv !tenv xx@(XCast a (CastWeakenEffect eff) x1) _
 = do   let config      = tableConfig table

        -- Check the effect term.
        (eff', kEff)    <- checkTypeM config kenv eff 
        when (not $ isEffectKind kEff)
         $ throw $ ErrorWeakEffNotEff xx eff' kEff

        -- Check the body.
        (x1', t1, effs, clo)    <- checkExpM table kenv tenv x1 Nothing
        let c'                  = CastWeakenEffect eff'

        returnX a
                (\z -> XCast z c' x1')
                t1
                (Sum.insert eff' effs)
                clo


-- Weaken a closure, adding in the given terms.
checkExpM' !table !kenv !tenv (XCast a (CastWeakenClosure xs) x1) _
 = do   
        -- Check the contained expressions.
        (xs', closs)
                <- liftM unzip
                $ mapM (\x -> checkArgM table kenv tenv x Nothing) xs

        -- Check the body.
        (x1', t1, effs, clos)   <- checkExpM table kenv tenv x1 Nothing
        let c'                  = CastWeakenClosure xs'

        returnX a
                (\z -> XCast z c' x1')
                t1
                effs
                (Set.unions (clos : closs))


-- Purify an effect, given a witness that it is pure.
checkExpM' !table !kenv !tenv xx@(XCast a (CastPurify w) x1) _
 = do   let config      = tableConfig table

        (w', tW)        <- checkWitnessM config kenv tenv w
        let wTEC        = reannotate fromAnT w'

        (x1', t1, effs, clo)
         <- checkExpM table kenv tenv x1 Nothing
                
        effs' <- case tW of
                  TApp (TCon (TyConWitness TwConPure)) effMask
                    -> return $ Sum.delete effMask effs
                  _ -> throw  $ ErrorWitnessNotPurity xx w tW

        let c'  = CastPurify wTEC

        returnX a
                (\z -> XCast z c' x1')
                t1 effs' clo


-- Forget a closure, given a witness that it is empty.
checkExpM' !table !kenv !tenv xx@(XCast a (CastForget w) x1) _
 = do   let config      = tableConfig table

        (w', tW)        <- checkWitnessM config kenv tenv w        
        let wTEC        = reannotate fromAnT w'

        (x1', t1, effs, clos)  
              <- checkExpM table kenv tenv x1 Nothing

        clos' <- case tW of
                  TApp (TCon (TyConWitness TwConEmpty)) cloMask
                    -> return $ maskFromTaggedSet 
                                        (Sum.singleton kClosure cloMask)
                                        clos

                  _ -> throw $ ErrorWitnessNotEmpty xx w tW

        let c'  = CastForget wTEC

        returnX a
                (\z -> XCast z c' x1')
                t1 effs clos'


-- Suspend a computation,
-- capturing its effects in a computation type.
checkExpM' !config !kenv !tenv (XCast a CastSuspend x1) _
 = do   
        (x1', t1, effs, clos) 
                <- checkExpM config kenv tenv x1 Nothing

        let tS  = tApps (TCon (TyConSpec TcConSusp))
                        [TSum effs, t1]

        returnX a
                (\z -> XCast z CastSuspend x1')
                tS (Sum.empty kEffect) clos


-- Run a suspended computation,
-- releasing its effects into the environment.
checkExpM' !config !kenv !tenv xx@(XCast a CastRun x1) _
 = do   
        (x1', t1, effs, clos) 
                <- checkExpM config kenv tenv x1 Nothing

        case t1 of
         TApp (TApp (TCon (TyConSpec TcConSusp)) eff2) tA 
          -> returnX a
                (\z -> XCast z CastRun x1')
                tA 
                (Sum.union effs (Sum.singleton kEffect eff2))
                clos

         _ -> throw $ ErrorRunNotSuspension xx t1

-- Type and witness expressions can only appear as the arguments 
-- to  applications.
checkExpM' !_config !_kenv !_tenv xx@(XType _) _
        = throw $ ErrorNakedType xx 

checkExpM' !_config !_kenv !_tenv xx@(XWitness _) _
        = throw $ ErrorNakedWitness xx



-- | Like `checkExp` but we allow naked types and witnesses.
checkArgM 
        :: (Show n, Pretty n, Ord n)
        => Table a n             -- ^ Static config.
        -> Env n                -- ^ Kind environment.
        -> Env n                -- ^ Type environment.
        -> Exp a n              -- ^ Expression to check.
        -> Maybe (Type n)       -- ^ Expected type, if any.
        -> CheckM a n 
                ( Exp (AnTEC a n) n
                , Set (TaggedClosure n))

checkArgM !table !kenv !tenv !xx !_
 = case xx of
        XType t
         -> do  checkTypeM (tableConfig table) kenv t
                let Just clo = taggedClosureOfTyArg kenv t

                return  ( XType t
                        , clo)

        XWitness w
         -> do  (w', _) <- checkWitnessM (tableConfig table) kenv tenv w
                return  ( XWitness (reannotate fromAnT w')
                        , Set.empty)

        _ -> do
                (xx', _, _, clos) 
                        <- checkExpM table kenv tenv xx Nothing
                return  ( xx'
                        , clos)

-------------------------------------------------------------------------------
-- | Check a case alternative.
checkAltM 
        :: (Show n, Pretty n, Ord n) 
        => Exp a n              -- ^ Whole case expression, for error messages.
        -> Table a n            -- ^ Checker table.
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

checkAltM !_xx !table !kenv !tenv !_tDiscrim !_tsArgs (AAlt PDefault xBody)
 = do   (xBody', tBody, effBody, cloBody)
                <- checkExpM table kenv tenv xBody Nothing

        return  ( AAlt PDefault xBody'
                , tBody
                , effBody
                , cloBody)

checkAltM !xx !table !kenv !tenv !tDiscrim !tsArgs (AAlt (PData dc bsArg) xBody)
 = do   let config      = tableConfig table
        let Just aCase  = takeAnnotOfExp xx

        -- If the data constructor isn't defined then the spread 
        --  transform won't have given it a proper type.
        --  Note that we can't simply check whether the constructor is in the
        --  environment because literals like 42# never are.
        tCtor
         <- case dc of
             DaConUnit   -> return tUnit
             DaConPrim{} -> return $ daConType dc
     
             DaConBound n
              -- Types of algebraic data ctors should be in the defs table.
              |  Just ctor <- Map.lookup n (dataDefsCtors $ configDataDefs config)
              -> return $ typeOfDataCtor ctor

              | otherwise
              -> throw  $ ErrorUndefinedCtor $ XCon aCase dc

        -- Take the type of the constructor and instantiate it with the 
        -- type arguments we got from the discriminant. 
        -- If the ctor type doesn't instantiate then it won't have enough foralls 
        -- on the front, which should have been checked by the def checker.
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
         $ throw $ ErrorCaseScrutineeTypeMismatch xx tDiscrim tResult

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
                <- checkExpM table kenv tenv' xBody Nothing

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
mergeAnnot !xx !tAnnot !tActual
        -- Annotation is bottom, so just use the real type.
        | isBot tAnnot      = return tActual

        -- Annotation matches actual type, all good.
        | tAnnot == tActual = return tActual

        -- Annotation does not match actual type.
        | otherwise       
        = throw $ ErrorCaseFieldTypeMismatch xx tAnnot tActual


