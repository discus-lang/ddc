
module DDC.Core.Transform.Lambdas
        ( lambdasModule
        , evalState
        , newVar)
where
import DDC.Core.Transform.Lambdas.Lift
import DDC.Core.Transform.Lambdas.Base
import DDC.Core.Fragment
import DDC.Core.Collect.Support
import DDC.Core.Transform.SubstituteXX
import DDC.Core.Module
import DDC.Core.Exp.Annot.Context
import DDC.Core.Exp.Annot.Ctx
import DDC.Core.Exp.Annot
import DDC.Type.Transform.SubstituteT
import DDC.Base.Pretty
import DDC.Base.Name
import qualified DDC.Type.Env                   as Env
import qualified DDC.Type.DataDef               as DataDef
import qualified DDC.Core.Env.EnvX              as EnvX
import qualified Data.Set                       as Set
import qualified Data.Map                       as Map
import Data.Maybe


---------------------------------------------------------------------------------------------------
-- | Perform lambda lifting in a module.
lambdasModule 
        :: ( Show a, Pretty a
           , Show n, Pretty n, Ord n, CompoundName n)
        => Profile n
        -> Module a n 
        -> S (Module a n)

lambdasModule profile mm
 = do
        -- Take the top-level environment of the module.
        let env = moduleEnvX 
                        (profilePrimKinds    profile)
                        (profilePrimTypes    profile)
                        (profilePrimDataDefs profile)
                        mm

        let c   = Context env (CtxTop env)
        x'      <- lambdasLoopX profile c $ moduleBody mm

        return 
         $ mm { moduleBody = x' }


---------------------------------------------------------------------------------------------------
-- | Result of lambda lifter recursion.
data Result a n
        = Result
        { -- | Whether we've made any progress in this pass.
          _resultProgress       :: Bool        

          -- | Bindings that we've already lifted out, 
          --   and should be added at top-level.
        , _resultBindings       :: [(Bind n, Exp a n)]
        }


instance Monoid (Result a n) where
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
         -> S (Exp a n)         --   Replacement expression.
        
lambdasLoopX p c xx
 = do   (xx1, Result progress _)
         <- lambdasTopX p c xx

        if progress 
         then lambdasLoopX p c xx1
         else return xx1

-- | Handle the expression that defines the body of the module
--   separately. The body is a letrec that contains the top level
--   bindings, and we don't want to lift them out further.
lambdasTopX p c xx
 = case xx of
        XLet a lts xBody
         -> do  (lts', r1) <- lambdasTopLets p c a xBody lts 
                (x',   r2) <- enterLetBody   c a lts xBody (lambdasTopX p)
                return  ( foldr (XLet a) x' lts'
                        , mappend r1 r2)

        _ -> lambdasX p c xx


-- | Handle the top-level group of bindings.
lambdasTopLets p c a xBody lts
 = case lts of
        LRec bxs
          -> do (bxs', r)  <- lambdasLetRec p c a [] bxs xBody
                return ([LRec bxs'], r)

        _ -> lambdasLets p c a xBody lts


---------------------------------------------------------------------------------------------------
-- | Perform a single pass of lambda lifting in an expression.
lambdasX :: (Show n, Show a, Pretty n, Pretty a, CompoundName n, Ord n)
         => Profile n           -- ^ Language profile.
         -> Context a n         -- ^ Enclosing context.
         -> Exp a n             -- ^ Expression to perform lambda lifting on.
         -> S ( Exp a n         --   Replacement expression
              , Result a n)     --   Lifter result.
         
lambdasX p c xx
 = case xx of
        XVar{}  
         -> return (xx, mempty)

        XCon{}  
         -> return (xx, mempty)
         
        -- Lift type lambdas to top-level.
        XLAM a b x0
         -> enterLAM c a b x0 $ \c' x
         -> do  (x', r)         <- lambdasX p c' x
                let xx'          = XLAM a b x'
                let Result _ bxs = r
                
                -- Decide whether to lift this lambda to top-level.
                -- If there are multiple nested lambdas then we want to lift
                -- the whole group at once, rather than lifting each one 
                -- individually.
                let liftMe       = isLiftyContext (contextCtx c)   && null bxs
                if liftMe
                 then do
                        let us'   = supportEnvFlags
                                  $ support Env.empty Env.empty xx'
                          
                        (xCall, bLifted, xLifted)
                         <- liftLambda p c us' a [(True, b)] x'

                        return  ( xCall
                                , Result True (bxs ++ [(bLifted, xLifted)]))

                 else   return  (xx', r)


        -- Lift value lambdas to top-level.
        XLam a b x0
         -> enterLam c a b x0 $ \c' x
         -> do  (x', r)      <- lambdasX p c' x
                let xx'          = XLam a b x'
                let Result _ bxs = r
                
                -- Decide whether to lift this lambda to top-level.
                let liftMe       =  isLiftyContext (contextCtx c)  && null bxs

                if liftMe
                 then do
                        let us' = supportEnvFlags
                                $ support Env.empty Env.empty xx'

                        (xCall, bLifted, xLifted)
                         <- liftLambda p c us' a [(False, b)] x'
                      
                        return  ( xCall
                                , Result True (bxs ++ [(bLifted, xLifted)]))

                 else   return  (xx', r)


        -- Lift suspensions to top-level.
        --   These behave like zero-arity lambda expressions,
        --   they suspend evaluation but do not abstract over a value.
        XCast a cc@CastBox x0
         -> enterCastBody c a cc x0 $ \c' x
         -> do  (x', r)      <- lambdasX p c' x
                let xx'          = XCast a CastBox x'
                let Result _ bxs = r

                -- Decide whether to lift this box to top-level.
                let liftMe       =  isLiftyContext (contextCtx c)  && null bxs

                if liftMe 
                 then do
                        let us' = supportEnvFlags
                                $ support Env.empty Env.empty xx'

                        (xCall, bLifted, xLifted)
                         <- liftLambda p c us' a [] (XCast a CastBox x')

                        return  ( xCall
                                , Result True (bxs ++ [(bLifted, xLifted)]))

                 else    return  (xx', r)


        -- Eta-expand partially applied data contructors.
        XApp a x1 x2
         | (xCon@(XCon _a (DaConBound nCon)), xsArg)   <- takeXApps1 x1 x2
         -> do  let ctx     = contextCtx c
                let envX    = topOfCtx ctx
                let defs    = EnvX.envxDataDefs envX

                case Map.lookup nCon (DataDef.dataDefsCtors defs) of
                 Just dataCtor
                  -- We check for partial application on the outside of a
                  -- set of nested applications.
                  |  case ctx of
                        CtxAppLeft{}    -> False
                        _               -> True

                  -- We're expecting an argument for each of the type and term parameters.
                  ,  arityT <- length $ DataDef.dataCtorTypeParams dataCtor
                  ,  arityX <- length $ DataDef.dataCtorFieldTypes dataCtor
                  ,  arity  <- arityT + arityX

                  -- See if we have the correct number of arguments.
                  ,  args  <- length xsArg
                  ,  arity /= args
                  -> do
                        -- Make binders for the new type parameters.
                        (bsT, usT)
                                <- fmap unzip
                                $  mapM (\(i, t) -> newVar (show i) t)
                                $  [ (i, typeOfBind b)
                                        | i <- [0..arityT - 1]
                                        | b <- DataDef.dataCtorTypeParams dataCtor ]

                        let sub = zip (DataDef.dataCtorTypeParams dataCtor) 
                                      (map TVar usT)

                        -- Make binders for the new term parameters.
                        (bsX, usX)
                                <- fmap unzip
                                $  mapM (\(i, t) -> newVar (show i) (substituteTs sub t))
                                $  [ (i, t)
                                        | i <- [0 .. arityX - 1]
                                        | t <- DataDef.dataCtorFieldTypes dataCtor ]

                        -- Transform all the arguments.
                        let downArg xArg = enterAppRight c a x1 xArg (lambdasX p)
                        (xsArg', rs)    <- fmap unzip $ mapM downArg xsArg

                        -- Build application of our new abstraction.
                        return  ( xApps a
                                        ( xLAMs a bsT $ xLams a bsX 
                                                $  xApps a xCon
                                                $  [ XType a (TVar u) | u <- usT]
                                                ++ [ XVar a u         | u <- usX])
                                        xsArg'

                                , mconcat rs)

                 _
                  -> do (x1', r1) <- enterAppLeft  c a x1 x2 (lambdasX p)
                        (x2', r2) <- enterAppRight c a x1 x2 (lambdasX p)
                        return  ( XApp a x1' x2'
                                , mappend r1 r2)


        -- Boilerplate.
        XApp a x1 x2
         -> do  (x1', r1)   <- enterAppLeft  c a x1 x2 (lambdasX p)
                (x2', r2)   <- enterAppRight c a x1 x2 (lambdasX p)
                return  ( XApp a x1' x2'
                        , mappend r1 r2)
                
        XLet a lts x
         -> do  (lts', r1)  <- lambdasLets  p c a x lts 
                (x',   r2)  <- enterLetBody c a lts x  (lambdasX p)
                return  ( foldr (XLet a) x' lts'
                        , mappend r1 r2)
                
        XCase a x alts
         -> do  (x',    r1) <- enterCaseScrut c a x alts (lambdasX p)
                (alts', r2) <- lambdasAlts  p c a x [] alts
                return  ( XCase a x' alts'
                        , mappend r1 r2)

        XCast a cc x
         ->     lambdasCast p c a cc x
                
        XType{}         
         ->     return  (xx, mempty)

        XWitness{}      
         ->     return  (xx, mempty)


-- Lets -------------------------------------------------------------------------------------------
-- | Perform lambda lifting in some let-bindings.
lambdasLets
        :: (Show a, Show n, Ord n, Pretty n, Pretty a, CompoundName n)
        => Profile n -> Context a n
        -> a -> Exp a n
        -> Lets a n
        -> S ([Lets a n], Result a n)
           
lambdasLets p c a xBody lts
 = case lts of
        LLet b x
         -> do  (x', r)   <- enterLetLLet c a b x xBody (lambdasX p)
                return  ([LLet b x'], r)

        LRec bxs
         -> do  (bxs', r) <- lambdasLetRecLiftAll p c a bxs
                return  (map (uncurry LLet) bxs', r)

        LPrivate{}
         ->     return  ([lts], mempty)


-- LetRec -----------------------------------------------------------------------------------------
-- | Perform lambda lifting in the right of a single let-rec binding.
lambdasLetRec 
        :: (Show a, Show n, Ord n, Pretty n, Pretty a, CompoundName n)
        => Profile n -> Context a n
        -> a -> [(Bind n, Exp a n)] -> [(Bind n, Exp a n)] -> Exp a n
        -> S ([(Bind n, Exp a n)], Result a n)

lambdasLetRec _ _ _ _ [] _
        = return ([], mempty)

lambdasLetRec p c a bxsAcc ((b, x) : bxsMore) xBody
 = do   (x',   r1) 
         <- enterLetLRec  c a bxsAcc b x bxsMore xBody (lambdasX p)

        case contextCtx c of

         -- If we're at top-level then drop lifted bindings here.
         CtxTop{}
          -> do (bxs', Result p2 bxs2) 
                 <- lambdasLetRec p c a ((b, x') : bxsAcc) bxsMore xBody
                let Result p1 bxsLifted = r1
                return  ( bxsLifted ++ ((b, x') : bxs')
                        , Result (p1 || p2) bxs2 )

         _
          -> do (bxs', r2) 
                 <- lambdasLetRec p c a ((b, x') : bxsAcc) bxsMore xBody
                return  ( (b, x') : bxs'
                        , mappend r1 r2 )


-- | When all the bindings in a letrec are lambdas, lift them all together.
lambdasLetRecLiftAll
        :: (Show a, Show n, Ord n, Pretty n, Pretty a, CompoundName n)
        => Profile n -> Context a n
        -> a
        -> [(Bind n, Exp a n)]
        -> S ([(Bind n, Exp a n)], Result a n)

lambdasLetRecLiftAll p c a bxs
 = let  -- Wrap the context in the letrec
        ctx before b x after
         = enterLetLRec c a before b x after x (\c' _ -> c')
   in do

       -- The union of free variables of all the mutually recursive bindings must be used,
       -- as any lifted function may call the other lifted functions.
       let us   = Set.unions
                $ map (supportEnvFlags . support Env.empty Env.empty)
                $ map snd bxs

       -- However, the functions we are lifting should not be treated as free variables
       let us'  = Set.filter 
                        (\(_, bo) -> not $ any (boundMatchesBind bo . fst) bxs)
                $ us

       -- Lift each of the bindings in its own context.
       --  We allow the group of bindings to contain ones with outer lambda
       --  abstractions that need to be lifted, along with non-functional
       --  bindings that stay in place.
       let lift _before [] 
             = return []

           lift before ((b, x) : after)
            = case takeXLamFlags x of
                -- The right of this binding has an outer abstraction, 
                -- so we need to lift it to top-level.
                Just (lams, xx)
                 -> do  let c'  =  ctx before b x after
                        l'      <- liftLambda p c' us' a lams xx
                        ls      <- lift (before ++ [(b,x)]) after
                        return  $ Right (b, l') : ls

                -- The right of this binding does not have any outer
                -- abstractions, so we can leave it in place.
                Nothing
                 -> do  ls      <- lift (before ++ [(b, x)]) after
                        return  $ Left (b, x)   : ls

       ls   <-  lift [] bxs

       -- The call to each lifted function
       let stripCall (Left  (b, x))          = (b, x)
           stripCall (Right (b, (xC, _, _))) = (b, xC)
       let calls = map stripCall ls

       -- Substitute the original name of the recursive function with a call to its new name,
       -- including passing along any free variables.
       -- Here, we need to unwrap the newly-created lambdas for the free variables,
       -- as capture-avoiding substitution would rename them - the opposite of what we want.
       let sub x = case takeXLamFlags x of
                    Just (lams, xx) -> makeXLamFlags a lams (substituteXXs calls xx)
                    Nothing         ->                       substituteXXs calls x

       -- The result bindings to add at the top-level, with all the new names substituted in
       let stripResult (Left _)                 = Nothing
           stripResult (Right (_, (_, bL, xL))) = Just (bL, sub xL)

       let res  = mapMaybe stripResult ls

       return (calls, Result True res)



-- Alts -------------------------------------------------------------------------------------------
-- | Perform lambda lifting in the right of a single alternative.
lambdasAlts 
        :: (Show a, Show n, Ord n, Pretty n, Pretty a, CompoundName n)
        => Profile n -> Context a n
        -> a -> Exp a n -> [Alt a n] -> [Alt a n]
        -> S ([Alt a n], Result a n)
           
lambdasAlts _ _ _ _ _ []
        = return ([], mempty)

lambdasAlts p c a xScrut altsAcc (AAlt w x : altsMore)
 = do   (x', r1)    <- enterCaseAlt   c a xScrut altsAcc w x altsMore (lambdasX p)
        (alts', r2) <- lambdasAlts  p c a xScrut (AAlt w x' : altsAcc) altsMore
        return  ( AAlt w x' : alts'
                , mappend r1 r2)


-- Cast -------------------------------------------------------------------------------------------
-- | Perform lambda lifting in the body of a Cast expression.
lambdasCast
        :: (Show a, Show n, Ord n, Pretty n, Pretty a, CompoundName n)
        => Profile n -> Context a n
        -> a -> Cast a n -> Exp a n
        -> S (Exp a n, Result a n)

lambdasCast p c a cc x
  = case cc of
        CastWeakenEffect{}    
         -> do  (x', r) <- enterCastBody c a cc x  (lambdasX p)
                return ( XCast a cc x', r)

        CastPurify{}
         -> do  (x', r) <- enterCastBody c a cc x  (lambdasX p)
                return (XCast a cc x',  r)

        CastBox 
         -> do  (x', r) <- enterCastBody  c a cc x (lambdasX p)
                return (XCast a cc x',  r)
       
        CastRun 
         -> do  (x', r) <- enterCastBody c a cc x (lambdasX p)
                return (XCast a cc x',  r)


