
module DDC.Core.Check.Exp.Case
        (checkCase)
where
import DDC.Core.Check.Exp.Base
import qualified DDC.Type.Sum   as Sum
import qualified DDC.Type.Env   as Env
import qualified Data.Set       as Set
import qualified Data.Map       as Map
import Data.List                as L


checkCase :: Checker a n

-- case expression ------------------------------
checkCase !table !kenv !tenv xx@(XCase a xDiscrim alts) _
 = do   let config      = tableConfig table

        -- Check the discriminant.
        (xDiscrim', tDiscrim, effsDiscrim, closDiscrim) 
         <- tableCheckExp table table kenv tenv xDiscrim Nothing

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

                _ -> throw $ ErrorCaseScrutineeNotAlgebraic a xx tDiscrim

        -- Get the mode of the data type, 
        --   this tells us how many constructors there are.
        mode    
         <- case mmode of
             Nothing -> throw $ ErrorCaseScrutineeTypeUndeclared a xx tDiscrim
             Just m  -> return m

        -- Check the alternatives.
        (alts', ts, effss, closs)     
                <- liftM unzip4
                $  mapM (checkAltM xx table kenv tenv tDiscrim tsArgs) alts

        -- There must be at least one alternative
        when (null ts)
         $ throw $ ErrorCaseNoAlternatives a xx

        -- All alternative result types must be identical.
        let (tAlt : _)  = ts
        forM_ ts $ \tAlt' 
         -> when (not $ equivT tAlt tAlt') 
             $ throw $ ErrorCaseAltResultMismatch a xx tAlt tAlt'

        -- Check for overlapping alternatives.
        let pats                = [p | AAlt p _ <- alts]
        let psDefaults          = filter isPDefault pats
        let nsCtorsMatched      = mapMaybe takeCtorNameOfAlt alts

        -- Alts were overlapping because there are multiple defaults.
        when (length psDefaults > 1)
         $ throw $ ErrorCaseOverlapping a xx

        -- Alts were overlapping because the same ctor is used multiple times.
        when (length (nub nsCtorsMatched) /= length nsCtorsMatched )
         $ throw $ ErrorCaseOverlapping a xx

        -- Check for alts overlapping because a default is not last.
        -- Also check there is at least one alternative.
        (case pats of
          [] -> throw $ ErrorCaseNoAlternatives a xx

          _  |  Just patsInit <- takeInit pats
             ,  or $ map isPDefault $ patsInit
             -> throw $ ErrorCaseOverlapping a xx

             |  otherwise
             -> return ())

        -- Check that alternatives are exhaustive.
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
           -> throw $ ErrorCaseNonExhaustive a xx nsCtorsMissing

           -- All constructors were matched.
           | otherwise 
           -> return ()

          -- Large types have an effectively infinite number of constructors
          -- (like integer literals), so there needs to be a default alt.
          DataModeLarge 
           | any isPDefault [p | AAlt p _ <- alts] -> return ()
           | otherwise  
           -> throw $ ErrorCaseNonExhaustiveLarge a xx)

        let effsMatch    
                = Sum.singleton kEffect 
                $ crushEffect $ tHeadRead tDiscrim

        returnX a
                (\z -> XCase z xDiscrim' alts')
                tAlt
                (Sum.unions kEffect (effsDiscrim : effsMatch : effss))
                (Set.unions         (closDiscrim : closs))

checkCase _ _ _ _ _
        = error "ddc-core.checkCase: no match"


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
 = do   
        -- Check the right of the alternative.
        (xBody', tBody, effBody, cloBody)
                <- tableCheckExp table table kenv tenv xBody Nothing

        return  ( AAlt PDefault xBody'
                , tBody
                , effBody
                , cloBody)

checkAltM !xx !table !kenv !tenv !tDiscrim !tsArgs (AAlt (PData dc bsArg) xBody)
 = do   let config      = tableConfig table
        let Just a      = takeAnnotOfExp xx

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
              -> throw  $ ErrorUndefinedCtor a $ XCon a dc

        -- Take the type of the constructor and instantiate it with the 
        -- type arguments we got from the discriminant. 
        -- If the ctor type doesn't instantiate then it won't have enough foralls 
        -- on the front, which should have been checked by the def checker.
        tCtor_inst      
         <- case instantiateTs tCtor tsArgs of
             Nothing -> throw $ ErrorCaseCannotInstantiate a xx tDiscrim tCtor
             Just t  -> return t
        
        -- Split the constructor type into the field and result types.
        let (tsFields_ctor, tResult) 
                        = takeTFunArgResult tCtor_inst

        -- The result type of the constructor must match the discriminant type.
        --  If it doesn't then the constructor in the pattern probably isn't for
        --  the discriminant type.
        when (not $ equivT tDiscrim tResult)
         $ throw $ ErrorCaseScrutineeTypeMismatch a xx tDiscrim tResult

        -- There must be at least as many fields as variables in the pattern.
        -- It's ok to bind less fields than provided by the constructor.
        when (length tsFields_ctor < length bsArg)
         $ throw $ ErrorCaseTooManyBinders a xx dc
                        (length tsFields_ctor)
                        (length bsArg)

        -- Merge the field types we get by instantiating the constructor
        -- type with possible annotations from the source program.
        -- If the annotations don't match, then we throw an error.
        tsFields        <- zipWithM (mergeAnnot a xx)
                            (map typeOfBind bsArg)
                            tsFields_ctor        

        -- Extend the environment with the field types.
        let bsArg'      = zipWith replaceTypeOfBind tsFields bsArg
        let tenv'       = Env.extends bsArg' tenv
        
        -- Check the body in this new environment.
        (xBody', tBody, effsBody, closBody)
                <- tableCheckExp table table kenv tenv' xBody Nothing

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
mergeAnnot :: Eq n => a -> Exp a n -> Type n -> Type n -> CheckM a n (Type n)
mergeAnnot !a !xx !tAnnot !tActual
        -- Annotation is bottom, so just use the real type.
        | isBot tAnnot      = return tActual

        -- Annotation matches actual type, all good.
        | tAnnot == tActual = return tActual

        -- Annotation does not match actual type.
        | otherwise       
        = throw $ ErrorCaseFieldTypeMismatch a xx tAnnot tActual

