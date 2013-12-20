
module DDC.Core.Check.Judge.Type.Case
        (checkCase)
where
import DDC.Core.Check.Judge.Type.Base
import qualified DDC.Type.Sum   as Sum
import qualified Data.Set       as Set
import qualified Data.Map       as Map
import Data.List                as L


checkCase :: Checker a n

-- case expression ------------------------------
checkCase !table !ctx0 xx@(XCase a xDiscrim alts) mode
 = do   let config      = tableConfig table

        -- There must be at least one alternative, even if there are no data
        -- constructors. The rest of the checking code assumes this, and will
        -- throw an unhelpful error if there are no alternatives.
        when (null alts)
         $ throw $ ErrorCaseNoAlternatives a xx

        -- Decide what mode to use when checking the discriminant.
        (modeDiscrim, ctx1)     
         <- takeDiscrimCheckModeFromAlts a table ctx0 mode alts

        -- Check the discriminant.
        (xDiscrim', tDiscrim, effsDiscrim, closDiscrim, ctxDiscrim) 
         <- tableCheckExp table table ctx1 xDiscrim modeDiscrim

        -- Split the type into the type constructor names and type parameters.
        -- Also check that it's algebraic data, and not a function or effect
        -- type etc. 
        (mDataMode, tsArgs)
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
        dataMode    
         <- case mDataMode of
             Nothing -> throw $ ErrorCaseScrutineeTypeUndeclared a xx tDiscrim
             Just m  -> return m

        -- Check the alternatives.
        --  We ignore the returned context because the order of alternatives
        --  should not matter for type inference.
        (alts', tAlt, effss, closs, ctx')
         <- checkAltsM a xx table tDiscrim tsArgs mode alts ctxDiscrim

        -- Check for overlapping alternatives.
        checkAltsOverlapping a xx alts

        -- Check that alternatives are exhaustive.
        checkAltsExhaustive a xx dataMode alts

        let effsMatch    
                = Sum.singleton kEffect 
                $ crushEffect $ tHeadRead tDiscrim

        ctrace  $ vcat
                [ text "* Case"
                , text "  modeDiscrim" <+> ppr modeDiscrim
                , indent 2 $ ppr ctxDiscrim 
                , empty ]

        returnX a
                (\z -> XCase z xDiscrim' alts')
                tAlt
                (Sum.unions kEffect (effsDiscrim : effsMatch : effss))
                (Set.unions         (closDiscrim : closs))
                ctx'

checkCase _ _ _ _
        = error "ddc-core.checkCase: no match"


-- | Decide what type checker mode to use when checking the discriminant
--   of a case expression. 
--
--   With plain type reconstruction then we also reconsruct the discrim type.
--
--   With bidirectional checking we use the type of the patterns as 
--   the expected type when checking the discriminant.
--
takeDiscrimCheckModeFromAlts
        :: Ord n
        => a
        -> Table a n
        -> Context n
        -> Mode n               -- ^ Mode for checking enclosing case expression.
        -> [Alt a n]            -- ^ Alternatives in the case expression.
        -> CheckM a n 
                ( Mode n
                , Context n)

takeDiscrimCheckModeFromAlts a table ctx mode alts
 | Recon        <- mode
 = return (Recon, ctx)

 | otherwise
 = do
        let pats     = map patOfAlt alts
        tsPats       <- liftM catMaybes $ mapM (dataTypeOfPat a table) pats

        -- TODO: check that multiple pattern types are equivalent.

        case tsPats of
         -- We only have a default pattern, 
         -- so will need to synthesise the type of the discrim without
         -- an expected type.
         [] 
          -> return (Synth, ctx)

         tPat : _    
          | Just (bs, tBody) <- takeTForalls tPat
          -> do  
                -- existentials for all of the type parameters.
                is        <- mapM  (\_ -> newExists kData) bs
                let ts     = map typeOfExists is
                let ctx'   = foldl (flip pushExists) ctx is
                let tBody' = substituteTs (zip bs ts) tBody
                return (Check tBody', ctx')
                
          | otherwise
          -> return (Check tPat, ctx)


-------------------------------------------------------------------------------
-- | Check some case alternatives.
--   TODO: merge this into checkAltM so we don't duplicate the type sig noise.
checkAltsM
        :: (Show n, Pretty n, Ord n)
        => a
        -> Exp a n              -- ^ Whole case expression, for error messages.
        -> Table a n            -- ^ Checker table.
        -> Type n               -- ^ Type of discriminant.
        -> [Type n]             -- ^ Args to type constructor of discriminant.
        -> Mode n               -- ^ Check mode for the alternatives.
        -> [Alt a n]            -- ^ Alternatives to check.
        -> Context n            -- ^ Context to check the alternatives in.
        -> CheckM a n
                ( [Alt (AnTEC a n) n]      -- Checked alternatives.
                , Type n                   -- Type of alternative results.
                , [TypeSum n]              -- Alternative effects.
                , [Set (TaggedClosure n)]  -- Alternative closures
                , Context n)

checkAltsM !a !xx !table !tDiscrim !tsArgs !mode !alts0 !ctx
 = checkAltsM1 alts0 ctx
 
 where 
  checkAltsM1 [] ctx0
   = do iA       <- newExists kData
        let tA   =  typeOfExists iA        
        let ctx1 =  pushExists iA ctx0
        return ([], tA, [], [], ctx1)

  checkAltsM1 (alt : alts) ctx0
   = do (alt',  tAlt,  eAlt, cAlt, ctx1)
         <- checkAltM   alt ctx0

        (alts', tAlts, esAlts, csAlts, ctx2)
         <- checkAltsM1 alts ctx1

        ctx3    <- makeEq a (ErrorCaseAltResultMismatch a xx tAlt tAlts)
                            ctx2 tAlt tAlts

        return  ( alt'  : alts'
                , tAlt
                , eAlt  : esAlts
                , cAlt  : csAlts
                , ctx3)

  checkAltM   (AAlt PDefault xBody) !ctx0
   = do   
        -- Check the right of the alternative.
        (xBody', tBody, effBody, cloBody, ctx1)
                <- tableCheckExp table table ctx0 xBody mode

        return  ( AAlt PDefault xBody'
                , tBody
                , effBody
                , cloBody
                , ctx1)

  checkAltM (AAlt (PData dc bsArg) xBody) !ctx0
   = do -- Get the constructor type associated with this pattern.
        Just tCtor <- ctorTypeOfPat a table (PData dc bsArg)
         
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
        let bsArg'         = zipWith replaceTypeOfBind tsFields bsArg
        let (ctx1, posArg) = markContext ctx0
        let ctxArg         = pushTypes bsArg' ctx1
        
        -- Check the body in this new environment.
        (xBody', tBody, effsBody, closBody, ctxBody)
                <- tableCheckExp table table ctxArg xBody mode

        -- Cut closure terms due to locally bound value vars.
        -- This also lowers deBruijn indices in un-cut closure terms.
        let closBody_cut 
                = Set.fromList
                $ mapMaybe (cutTaggedClosureXs bsArg')
                $ Set.toList closBody

        -- Pop the argument types from the context.
        let ctx_cut 
                = popToPos posArg ctxBody

        -- We're returning the new context for kicks,
        -- but the caller doesn't use it because we don't want the order of 
        -- alternatives to matter for type inference.
        return  ( AAlt (PData dc bsArg') xBody'
                , tBody
                , effsBody
                , closBody_cut
                , ctx_cut)


-- | Merge a type annotation on a pattern field with a type we get by
--   instantiating the constructor type.
mergeAnnot 
        :: Eq n 
        => a -> Exp a n -> Type n -> Type n -> CheckM a n (Type n)

mergeAnnot !a !xx !tAnnot !tActual
        -- Annotation is bottom, so use the given type.
        | isBot tAnnot      
        = return tActual

        -- Annotation matches actual type, all good.
        | tAnnot == tActual 
        = return tActual

        -- Annotation does not match actual type.
        | otherwise       
        = throw $ ErrorCaseFieldTypeMismatch a xx tAnnot tActual


-- Ctor Types -----------------------------------------------------------------
-- | Get the constructor type associated with a pattern, or Nothing for the
--   default pattern. If the data constructor isn't defined then the spread 
--   transform won't have given it a proper type.
--   Note that we can't simply check whether the constructor is in the
---  environment because literals like 42# never are.
ctorTypeOfPat
        :: Ord n 
        => a
        -> Table a n
        -> Pat n
        -> CheckM a n (Maybe (Type n))

ctorTypeOfPat a table (PData dc _)
 = case dc of
        DaConUnit   -> return $ Just $ tUnit
        DaConPrim{} -> return $ Just $ daConType dc
     
        DaConBound n
         -- Types of algebraic data ctors should be in the defs table.
         |  Just ctor <- Map.lookup n 
                                $ dataDefsCtors 
                                $ configDataDefs $ tableConfig table
         -> return $ Just $ typeOfDataCtor ctor

         | otherwise
         -> throw  $ ErrorUndefinedCtor a $ XCon a dc

ctorTypeOfPat _a _table PDefault
 = return Nothing


-- | Get the data type associated with a pattern,
--   or Nothing for the default pattern.
--
--   Yields  the data type with outer quantifiers for its type parametrs.
--    For example, given pattern (Cons x xs), return (forall [a : Data]. List a)
--
dataTypeOfPat 
        :: Ord n 
        => a
        -> Table a n
        -> Pat n
        -> CheckM a n (Maybe (Type n))

dataTypeOfPat a table pat
 = do   mtCtor      <- ctorTypeOfPat a table pat

        case mtCtor of
         Nothing    -> return Nothing
         Just tCtor -> return $ Just $ eat [] tCtor

 where  eat bs tt
         = case tt of  
                TForall b t        -> eat (bs ++ [b]) t
                TApp{}
                 |  Just (_t1, t2) <- takeTFun tt
                 -> eat bs t2
                _                  -> foldr TForall tt bs


-- Checks ---------------------------------------------------------------------
-- | Check for overlapping alternatives, 
--   and throw an error in the `CheckM` monad if there are any.
checkAltsOverlapping
        :: Eq n
        => a                    -- ^ Annotation for error messages.
        -> Exp a n              -- ^ Expression for error messages.
        -> [Alt a n]            -- ^ Alternatives to check.
        -> CheckM a n ()

checkAltsOverlapping a xx alts
 = do   let pats                = [p | AAlt p _ <- alts]
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
        case pats of
          [] -> throw $ ErrorCaseNoAlternatives a xx

          _  |  Just patsInit <- takeInit pats
             ,  or $ map isPDefault $ patsInit
             -> throw $ ErrorCaseOverlapping a xx

             |  otherwise
             -> return ()


-- | Check that the alternatives are exhaustive,
--   and throw and error if they're not.
checkAltsExhaustive
        :: Eq n
        => a                    -- ^ Annotation for error messages.
        -> Exp a n              -- ^ Expression for error messages.
        -> DataMode n           -- ^ Mode of data type.
                                --   Tells us how many data constructors to expect.
        -> [Alt a n]            -- ^ Alternatives to check.
        -> CheckM a n ()

checkAltsExhaustive a xx mode alts
 = do   let nsCtorsMatched      = mapMaybe takeCtorNameOfAlt alts
        
        -- Check that alternatives are exhaustive.
        case mode of

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
           -> throw $ ErrorCaseNonExhaustiveLarge a xx

