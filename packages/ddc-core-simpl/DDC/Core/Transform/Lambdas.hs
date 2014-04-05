
module DDC.Core.Transform.Lambdas
        (lambdasModule)
where
import DDC.Core.Fragment
import DDC.Core.Collect.Support
import DDC.Core.Transform.SubstituteXX
import DDC.Type.Collect.FreeT
import DDC.Core.Exp.AnnotCtx
import DDC.Core.Context
import DDC.Core.Module
import DDC.Core.Compounds
import DDC.Core.Exp
import DDC.Base.Pretty
import DDC.Base.Name
import Data.Function
import Data.List
import Data.Set                         (Set)
import Data.Map                         (Map)
import qualified DDC.Core.Check         as Check
import qualified DDC.Type.Env           as Env
import qualified Data.Set               as Set
import qualified Data.Map               as Map
import Data.Monoid
import Data.Maybe


---------------------------------------------------------------------------------------------------
-- | Perform lambda lifting in a module.
lambdasModule 
        :: ( Show a, Pretty a
           , Show n, Pretty n, Ord n, CompoundName n)
        => Profile n
        -> Module a n -> Module a n

lambdasModule profile mm
 = let  
        -- Take the top-level environment of the module.
        defs    = moduleDataDefs mm
        kenv    = moduleKindEnv  mm
        tenv    = moduleTypeEnv  mm
        c       = Context kenv tenv (CtxTop defs kenv tenv)

        x'      = lambdasLoopX profile c $ moduleBody mm

   in   beautifyModule
         $ mm { moduleBody = x' }


---------------------------------------------------------------------------------------------------
-- | Result of lambda lifter recursion.
data Result a n
        = Result 
           Bool                 -- Whether we've made progress in this pass.
           [(Bind n, Exp a n)]  -- Lifted bindings

instance Ord n => Monoid (Result a n) where
 mempty
  = Result False []
 
 mappend (Result p1 lts1) (Result p2 lts2)
  = Result (p1 || p2) (lts1 ++ lts2)


-- Exp --------------------------------------------------------------------------------------------
lambdasLoopX
         :: (Show n, Show a, Pretty n, Pretty a, CompoundName n, Ord n)
         => Profile n           -- ^ Language profile.
         -> Context a n         -- ^ Enclosing context.
         -> Exp a n             -- ^ Expression to perform lambda lifting on.
         -> Exp a n             --   Replacement expression.
        
lambdasLoopX p c xx
 = let  (xx1, Result progress _)   = lambdasX p c xx
   in   if progress then lambdasLoopX p c xx1
                    else xx1

-- | Perform a single pass of lambda lifting in an expression.
lambdasX :: (Show n, Show a, Pretty n, Pretty a, CompoundName n, Ord n)
         => Profile n           -- ^ Language profile.
         -> Context a n         -- ^ Enclosing context.
         -> Exp a n             -- ^ Expression to perform lambda lifting on.
         -> ( Exp a n           --   Replacement expression
            , Result a n)       --   Lifter result.
         
lambdasX p c xx
 = case xx of
        XVar{}  -> (xx, mempty)
        XCon{}  -> (xx, mempty)
         
        XLAM a b x0
         -> enterLAM c a b x0 $ \c' x
         -> let (x', r)      = lambdasX p c' x
                xx'          = XLAM a b x'
                Result _ bxs = r
                
                -- Decide whether to lift this lambda to top-level.
                -- We lifted nexted lambdas in multiple passes, 
                --  doing it all at once would take more work.
                liftMe       =  isLiftyContext (contextCtx c) 
                             && null bxs
                
            in if liftMe
                then  let us'   = supportEnvFlags
                                $ support Env.empty Env.empty (XLAM a b x')
                          
                          (xCall, bLifted, xLifted)
                                = liftLambda p c us' True a b x'

                      in  ( xCall
                          , Result True (bxs ++ [(bLifted, xLifted)]))

                else (xx', r)

        XLam a b x0
         -> enterLam c a b x0 $ \c' x
         -> let (x', r)      = lambdasX p c' x
                xx'          = XLam a b x'
                Result _ bxs = r
                
                -- Decide whether to lift this lambda to top-level.
                -- We lifted nexted lambdas in multiple passes, 
                --  doing it all at once would take more work.
                liftMe       =  isLiftyContext (contextCtx c)
                             && null bxs

            in if liftMe
                then  let us'   = supportEnvFlags
                                $ support Env.empty Env.empty (XLam a b x')

                          (xCall, bLifted, xLifted)
                                = liftLambda p c us' False a b x'
                      
                      in  ( xCall
                          , Result True (bxs ++ [(bLifted, xLifted)]))
                else (xx', r)

        XApp a x1 x2
         -> let (x1', r1)       = enterAppLeft  c a x1 x2 (lambdasX p)
                (x2', r2)       = enterAppRight c a x1 x2 (lambdasX p)
            in  ( XApp a x1' x2'
                , mappend r1 r2)
                
        XLet a lts x
         -> let (lts', r1)      = lambdasLets  p c a x lts 
                (x',   r2)      = enterLetBody c a lts x  (lambdasX p)
            in  ( XLet a lts' x'
                , mappend r1 r2)
                
        XCase a x alts
         -> let (x',    r1)     = enterCaseScrut c a x alts (lambdasX p)
                (alts', r2)     = lambdasAlts  p c a x [] alts
            in  ( XCase a x' alts'
                , mappend r1 r2)

        XCast a cc x
         ->     lambdasCast p c a cc x
                
        XType{}         -> (xx, mempty)
        XWitness{}      -> (xx, mempty)


-- Lets -------------------------------------------------------------------------------------------
-- | Perform lambda lifting in some let-bindings.
lambdasLets
        :: (Show a, Show n, Ord n, Pretty n, Pretty a, CompoundName n)
        => Profile n -> Context a n
        -> a -> Exp a n
        -> Lets a n
        -> (Lets a n, Result a n)
           
lambdasLets p c a xBody lts
 = case lts of
 
        LLet b x
         -> let (x', r)   = enterLetLLet c a b x xBody (lambdasX p)
            in  (LLet b x', r)

        LRec bxs
         -> let (bxs', r) = lambdasLetRec p c a [] bxs xBody
            in  (LRec bxs', r)
                
        LPrivate{}      -> (lts, mempty)
        LWithRegion{}   -> (lts, mempty)


-- LetRec -----------------------------------------------------------------------------------------
-- | Perform lambda lifting in the right of a single let-rec binding.
lambdasLetRec 
        :: (Show a, Show n, Ord n, Pretty n, Pretty a, CompoundName n)
        => Profile n -> Context a n
        -> a -> [(Bind n, Exp a n)] -> [(Bind n, Exp a n)] -> Exp a n
        -> ([(Bind n, Exp a n)], Result a n)

lambdasLetRec _ _ _ _ [] _
        = ([], mempty)

lambdasLetRec p c a bxsAcc ((b, x) : bxsMore) xBody
 = let  (x',   r1) = enterLetLRec  c a bxsAcc b x bxsMore xBody (lambdasX p)

   in   case contextCtx c of

         -- If we're at top-level then drop lifted bindings here.
         CtxTop{}
          -> let  (bxs', Result p2 bxs2) 
                        = lambdasLetRec p c a ((b, x') : bxsAcc) bxsMore xBody
                  Result p1 bxsLifted = r1
             in   ( bxsLifted ++ ((b, x') : bxs')
                  , Result (p1 || p2) bxs2)

         _
          -> let  (bxs', r2) = lambdasLetRec p c a ((b, x') : bxsAcc) bxsMore xBody
             in   ( (b, x') : bxs'
                  , mappend r1 r2)


-- Alts -------------------------------------------------------------------------------------------
-- | Perform lambda lifting in the right of a single alternative.
lambdasAlts 
        :: (Show a, Show n, Ord n, Pretty n, Pretty a, CompoundName n)
        => Profile n -> Context a n
        -> a -> Exp a n -> [Alt a n] -> [Alt a n]
        -> ([Alt a n], Result a n)
           
lambdasAlts _ _ _ _ _ []
        = ([], mempty)

lambdasAlts p c a xScrut altsAcc (AAlt w x : altsMore)
 = let  (x', r1)    = enterCaseAlt   c a xScrut altsAcc w x altsMore (lambdasX p)
        (alts', r2) = lambdasAlts  p c a xScrut (AAlt w x' : altsAcc) altsMore
   in   ( AAlt w x' : alts'
        , mappend r1 r2)


-- Cast -------------------------------------------------------------------------------------------
-- | Perform lambda lifting in the body of a Cast expression.
lambdasCast
        :: (Show a, Show n, Ord n, Pretty n, Pretty a, CompoundName n)
        => Profile n -> Context a n
        -> a -> Cast a n -> Exp a n
        -> (Exp a n, Result a n)

lambdasCast p c a cc x
  = case cc of
        CastWeakenEffect{}    
         -> let (x', r) = enterCastBody c a cc x  (lambdasX p)
            in  ( XCast a cc x', r)

        -- ISSUE #331: Lambda lifter doesn't work with closure typing.
        -- The closure typing system is a mess, and doing this
        -- properly would be be hard work, so we just don't bother.
        -- Lambda lifting won't work with the Eval or Lite fragments.
        CastWeakenClosure{}
         ->    error "ddc-core-simpl.lambdas: closures not handled."

        CastPurify{}
         -> let (x', r) = enterCastBody c a cc x  (lambdasX p)
            in  ( XCast a cc x', r)

        CastForget{}
         -> let (x', r) = enterCastBody  c a cc x (lambdasX p)
            in  ( XCast a cc x', r)

        CastBox 
         -> let (x', r) = enterCastBody  c a cc x (lambdasX p)
            in  (XCast a cc x',  r)
        

        CastRun 
         -> let (x', r)  = enterCastBody c a cc x (lambdasX p)
            in  (XCast a cc x',  r)


---------------------------------------------------------------------------------------------------
-- | Check if this is a context that we should lift lambda abstractions out of.
isLiftyContext :: Ctx a n -> Bool
isLiftyContext ctx
 = case ctx of
        -- Don't lift out of the top-level context.
        -- There's nowhere else to lift to.
        CtxTop{}        -> False
        CtxLetLLet{}    -> not $ isTopLetCtx ctx
        CtxLetLRec{}    -> not $ isTopLetCtx ctx

        -- Don't lift if we're inside more lambdas.
        --  We want to lift the whole binding group together.
        CtxLAM{}        -> False
        CtxLam{}        -> False
   
        -- We can't do code generation for abstractions in these contexts,
        -- so they need to be lifted.
        CtxAppLeft{}    -> True
        CtxAppRight{}   -> True
        CtxLetBody{}    -> True
        CtxCaseScrut{}  -> True
        CtxCaseAlt{}    -> True
        CtxCastBody{}   -> True


---------------------------------------------------------------------------------------------------
-- | Construct the call site, and new lifted binding for a lambda lifted
--   abstraction.
liftLambda 
        :: (Show n, Pretty n, Ord n, CompoundName n, Pretty a)
        => Profile n            -- ^ Language profile.
        -> Context a n          -- ^ Context of the original abstraction.
        -> Set (Bool, Bound n)  -- ^ Free variables in the body of the abstraction.
        -> Bool                 -- ^ Whether this is a type-abstraction.
        -> a -> Bind n -> Exp a n
        -> (  Exp a n
            , Bind n, Exp a n)

liftLambda p c fusFree isTypeLam a bParam xBody
 = let  ctx     = contextCtx c
        kenv    = contextKindEnv c
        tenv    = contextTypeEnv c

        -- The complete abstraction that we're lifting out.
        xLambda = if isTypeLam 
                        then XLAM a bParam xBody
                        else XLam a bParam xBody

        -- Name of the enclosing top-level binding.
        Just nTop   = takeTopNameOfCtx ctx

        -- Bound corresponding to the parameter of the abstraction.
        Just uParam = takeSubstBoundOfBind bParam

        -- Names of other supers bound at top-level.
        nsSuper     = takeTopLetEnvNamesOfCtx ctx

        -- Name of the new lifted binding.
        nLifted     = extendName nTop ("Lift_" ++ encodeCtx ctx)
        uLifted     = UName nLifted


        -- Build the type checker configuration for this context.
        (defs, _, _)    = topOfCtx (contextCtx c)        
        config          = Check.configOfProfile p
        config'         = config { Check.configDataDefs
                                        = mappend defs (Check.configDataDefs config) }

        -- Function to get the type of an expression in this context.
        -- If there are type errors in the input program then some 
        -- either the lambda lifter is broken or some other transform
        -- has messed up.
        typeOfExp x
         = case Check.typeOfExp 
                        config' (contextKindEnv c) (contextTypeEnv c)
                        x
            of  Left err
                 -> error $ renderIndent $ vcat
                          [ text "ddc-core-simpl.liftLambda: type error in lifted expression"
                          , ppr err]
                Right t -> t


        -- Decide whether we want to bind the given variable as a new parameter
        -- on the lifted super. We don't need to bind primitives, other supers,
        -- or the variable bound by the abstraction itself.
        keepVar fu@(f, u)
         | (False, UName n) <- fu      = not $ Set.member n nsSuper
         | (_,     UPrim{}) <- fu      = False
         | f == isTypeLam, u == uParam = False
         | otherwise                   = True

        fusFree_filtered
                = filter keepVar
                $ Set.toList fusFree


        -- Join in the types of the free variables.
        joinType (f, u)
         = case f of
            True    | Just t <- Env.lookup u kenv
                    -> ((f, u), t)

            False   | Just t <- Env.lookup u tenv
                    -> ((f, u), t)
                
            _       -> error $ unlines
                        [ "ddc-core-simpl.joinType: cannot find type of free var."
                        , show (f, u) ]

        futsFree_types
                = map joinType fusFree_filtered


        -- Add in type variables that are free in the types of free value variables.
        -- We need to bind these as well in the new super.
        expandFree ((f, u), t)
         | False <- f   =  [(f, u)]
                        ++ [(True, ut) | ut  <- Set.toList
                                             $  freeVarsT Env.empty t]
         | otherwise    =  [(f, u)]
    
        fusFree_body    =  [(True, ut) | ut  <- Set.toList 
                                             $  freeVarsT Env.empty $ typeOfExp xLambda]

        futsFree_expandFree
                =  map joinType
                $  Set.toList $ Set.fromList
                $  (concatMap expandFree $ futsFree_types)
                ++ fusFree_body

        -- Sort free vars so the type variables come out the front.
        futsFree
                = sortBy (compare `on` (not . fst . fst))
                $ futsFree_expandFree


        -- At the call site, apply all the free variables of the lifted
        -- function as new arguments.    
        makeArg  (True,  u) = XType a (TVar u)
        makeArg  (False, u) = XVar a u
        
        xCall   = xApps a (XVar a uLifted)
                $ map makeArg $ map fst futsFree


        -- ISSUE #330: Lambda lifter doesn't work with anonymous binders.
        -- For the lifted abstraction, wrap it in new lambdas to bind all of
        -- its free variables. 
        makeBind ((True,  (UName n)), t) = (True,  BName n t)
        makeBind ((False, (UName n)), t) = (False, BName n t)
        makeBind fut
                = error $ "ddc-core-simpl.liftLamba: unhandled binder " ++ show fut
        
        -- Make the new super.
        bsParam = map makeBind futsFree

        xLifted = makeXLamFlags a bsParam xLambda


        -- Get the type of the bound expression, which we need when building
        -- the type of the new super.
        tLifted = typeOfExp xLifted
        bLifted = BName nLifted tLifted
        
    in  ( xCall
        , bLifted, xLifted)


---------------------------------------------------------------------------------------------------
-- | Beautify the names of lifted lamdba abstractions.
--   The lifter itself names new abstractions after the context they come from.
--   This is an easy way of generating unique names, but the names are too
--   verbose to want to show to users, or put in the symbol table of the
--   resulting binary.
--   
--   The beautifier renames the bindings of lifted abstractions to
--    fun$L0, fun$L1 etc, where 'fun' is the name of the top-level binding
--   it was lifted out of.
--
beautifyModule 
        :: forall a n. (Ord n, Show n, CompoundName n)
        => Module a n -> Module a n

beautifyModule mm
 = mm { moduleBody = beautifyX $ moduleBody mm }

 where
  -- If the given binder is for an abstraction that we have lifted, 
  -- then produce a new nice name for it.
  makeRenamer 
        :: Map n Int -> Bind n 
        -> (Map n Int, Maybe (n, (n, Type n)))
  makeRenamer acc b
        | BName n t         <- b
        , Just (nBase, str) <- splitName n
        , isPrefixOf "Lift_" str
        = case Map.lookup nBase acc of
           Nothing  -> ( Map.insert nBase 0 acc
                       , Just (  extendName nBase str
                              , (extendName nBase ("L" ++ show (0 :: Int)), t)))

           Just n'  -> ( Map.insert nBase (n' + 1) acc
                       , Just (  extendName nBase str
                              , (extendName nBase ("L" ++ show (n' + 1)),   t)))

        | otherwise = (acc, Nothing)

  -- Beautify bindings.
  beautifyBXs a bxs
   = let bsRenames   :: [(n, (n, Type n))]
         bsRenames   = catMaybes $ snd
                     $ mapAccumL makeRenamer (Map.empty :: Map n Int)
                     $ map fst bxs

         bxsSubsts   :: [(Bind n, Exp a n)]
         bxsSubsts   =  [ (BName n t, XVar a (UName n'))
                                | (n, (n', t))  <- bsRenames]   

         renameBind (b, x)
                | BName n t     <- b
                , Just  (n', _) <- lookup n bsRenames
                = (BName n' t, x)
                
                | otherwise = (b, x)
            
     in  map (\(b, x) -> (b, substituteXXs bxsSubsts x))
          $ map renameBind bxs

  -- Beautify bindings in top-level let-expressions.
  beautifyX xx
   = case xx of
        XLet a (LRec bxs) xBody
         -> let bxs'    = beautifyBXs a bxs
            in  XLet a (LRec bxs')  (beautifyX xBody)

        XLet a (LLet b x) xBody
         -> let [(b', x')] = beautifyBXs a [(b, x)]
            in  XLet a (LLet b' x') (beautifyX xBody)

        _ -> xx

