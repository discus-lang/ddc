
module DDC.Core.Transform.Lambdas
        (lambdasModule)
where
import DDC.Core.Collect.Support
import DDC.Core.Exp.AnnotCtx
import DDC.Core.Context
import DDC.Core.Module
import DDC.Core.Compounds
import DDC.Core.Exp
import DDC.Base.Pretty
import DDC.Base.Name
import Data.Set                 (Set)
import qualified DDC.Type.Env   as Env
import qualified Data.Set       as Set
import Debug.Trace
import Data.Monoid

-- TODO Normalize names of lifted bindings.
--      Add free vars as parameters to lifted binding, and at call site.
--      Fix type of lifted binding, will need to take AnTEC version of AST
--      Test for multiply nested lambdas.

---------------------------------------------------------------------------------------------------
-- | Perform lambda lifting in a module.
--   TODO: Split 'StringName' from Pretty .. should not require full pretty printer.
lambdasModule 
        :: (Show a, Show n, Ord n, Pretty n, CompoundName n)
        => Module a n -> Module a n

lambdasModule mm
 = let  
        -- Take the top-level environment of the module.
        kenv    = moduleKindEnv mm
        tenv    = moduleTypeEnv mm
        c       = Context kenv tenv (CtxTop kenv tenv)

        (x', _) = lambdasX c $ moduleBody mm

   in   mm { moduleBody = x' }


---------------------------------------------------------------------------------------------------
-- | Result of lambda lifter recursion.
data Result a n
        = Result [(Bind n, Exp a n)]    -- Lifted bindings

instance Ord n => Monoid (Result a n) where
 mempty
  = Result []
 
 mappend (Result lts1) (Result lts2)
  = Result (lts1 ++ lts2)


-- Exp --------------------------------------------------------------------------------------------
-- | Perform lambda lifting in an expression.
--   When leaving a lambda abs, when leaving inner most one then chop it.
--
--   TODO: handle case where free vars in a lambda have anonymous names
--         When passing up free vars, also pass up the type so we can use
--         it at the binding position.
--
--   TODO: lifting multiple nested lambdas all at once might be a headache as we
--         need to add the new top-level super to the environment.
--         If problematic then just call the single level lifter multiple times.
--
lambdasX :: (Show n, Show a, Pretty n, CompoundName n, Ord n)
         => Context a n         -- ^ Enclosing context.
         -> Exp a n             -- ^ Expression to perform lambda lifting on.
         -> ( Exp a n           --   Replacement expression
            , Result a n)       --   Lifter result.
         
lambdasX c xx
 = case xx of
        XVar{}  -> (xx, mempty)
        XCon{}  -> (xx, mempty)
         
        XLAM a b x0
         -> enterLAM c a b x0 $ \c' x
         -> let (x', r)    = lambdasX c' x
                xx'        = XLAM a b x'
                liftMe     = isLiftyContext (contextCtx c)
                Result bxs = r

            in if liftMe
                then  let us'   = supportEnvFlags
                                $ support Env.empty Env.empty (XLAM a b x')
                          
                          (xCall, bLifted, xLifted)
                                = liftLambda c us' True a b x'

                      in  ( xCall
                          , Result (bxs ++ [(bLifted, xLifted)]))

                else (xx', r)

        XLam a b x0
         -> enterLam c a b x0 $ \c' x
         -> let (x', r)    = lambdasX c' x
                xx'        = XLam a b x'
                liftMe     = isLiftyContext (contextCtx c)
                Result bxs = r

            in if liftMe
                then  let us'   = supportEnvFlags
                                $ support Env.empty Env.empty (XLam a b x')

                          (xCall, bLifted, xLifted)
                                = liftLambda c us' False a b x'
                      
                      in  ( xCall
                          , Result (bxs ++ [(bLifted, xLifted)]))
                else (xx', r)

        XApp a x1 x2
         -> let (x1', r1)       = enterAppLeft  c a x1 x2 lambdasX
                (x2', r2)       = enterAppRight c a x1 x2 lambdasX
            in  ( XApp a x1' x2'
                , mappend r1 r2)
                
        XLet a lts x
         -> let (lts', r1)      = lambdasLets  c a x lts 
                (x',   r2)      = enterLetBody c a lts x lambdasX
            in  ( XLet a lts' x'
                , mappend r1 r2)
                
        XCase a x alts
         -> let (x',    r1)     = enterCaseScrut c a x alts lambdasX
                (alts', r2)     = lambdasAlts c a x [] alts
            in  ( XCase a x' alts'
                , mappend r1 r2)

        XCast a cc x
         ->     lambdasCast c a cc x
                
        XType{}
         ->     (xx, Result [])

        XWitness{}
         ->     (xx, Result [])


-- Lets -------------------------------------------------------------------------------------------
-- | Perform lambda lifting in some let-bindings.
lambdasLets
        :: (Show a, Show n, Ord n, Pretty n, CompoundName n)
        => Context a n
        -> a -> Exp a n
        -> Lets a n
        -> (Lets a n, Result a n)
           
lambdasLets c a xBody lts
 = case lts of
 
        LLet b x
         -> let (x', r)   = enterLetLLet c a b x xBody lambdasX
            in  (LLet b x', r)

        LRec bxs
         -> let (bxs', r) = lambdasLetRec c a [] bxs xBody
            in  (LRec bxs', r)
                
        LPrivate{}
         ->     (lts, Result [])

        LWithRegion{}
         ->     (lts, Result [])


-- LetRec -----------------------------------------------------------------------------------------
-- | Perform lambda lifting in the right of a single let-rec binding.
lambdasLetRec 
        :: (Show a, Show n, Ord n, Pretty n, CompoundName n)
        => Context a n
        -> a -> [(Bind n, Exp a n)] -> [(Bind n, Exp a n)] -> Exp a n
        -> ([(Bind n, Exp a n)], Result a n)

lambdasLetRec _ _ _ [] _
        = ([], Result [])

lambdasLetRec c a bxsAcc ((b, x) : bxsMore) xBody
 = let  (x',   r1) = enterLetLRec  c a bxsAcc b x bxsMore xBody lambdasX

   in   case contextCtx c of

         -- If we're at top-level then drop lifted bindings here.
         CtxTop{}
          -> let  (bxs', r2) = lambdasLetRec c a ((b, x') : bxsAcc) bxsMore xBody
                  Result bxsLifted = r1
             in   ( bxsLifted ++ ((b, x') : bxs')
                  , r2)

         _
          -> let  (bxs', r2) = lambdasLetRec c a ((b, x') : bxsAcc) bxsMore xBody
             in   ( (b, x') : bxs'
                  , mappend r1 r2)


-- Alts -------------------------------------------------------------------------------------------
-- | Perform lambda lifting in the right of a single alternative.
lambdasAlts 
        :: (Show a, Show n, Ord n, Pretty n, CompoundName n)
        => Context a n
        -> a -> Exp a n -> [Alt a n] -> [Alt a n]
        -> ([Alt a n], Result a n)
           
lambdasAlts _ _ _ _ []
        = ([], Result [])

lambdasAlts c a xScrut altsAcc (AAlt w x : altsMore)
 = let  (x', r1)    = enterCaseAlt c a xScrut altsAcc w x altsMore lambdasX
        (alts', r2) = lambdasAlts  c a xScrut (AAlt w x' : altsAcc) altsMore
   in   ( AAlt w x' : alts'
        , mappend r1 r2)


-- Cast -------------------------------------------------------------------------------------------
-- | Perform lambda lifting in the body of a Cast expression.
lambdasCast
        :: (Show a, Show n, Ord n, Pretty n, CompoundName n)
        => Context a n
        -> a -> Cast a n -> Exp a n
        -> (Exp a n, Result a n)

lambdasCast c a cc x
  = case cc of
        CastWeakenEffect{}    
         -> let (x', r) = enterCastBody c a cc x lambdasX
            in  ( XCast a cc x'
                , mappend (Result []) r)

        -- TODO: The closure typing system is a mess, and doing this
        --       properly would be be hard work, so we just don't bother.
        --       Lambda lifting won't work with the Eval or Lite fragments.
        CastWeakenClosure{}
         ->    error "ddc-core-simpl.lambdas: closures not handled."

        CastPurify{}
         -> let (x', r) = enterCastBody c a cc x lambdasX
            in  ( XCast a cc x'
                , mappend (Result []) r)

        CastForget{}
         -> let (x', r) = enterCastBody  c a cc x lambdasX
            in  ( XCast a cc x'
                , mappend (Result []) r)

        CastBox 
         -> let (x', r) = enterCastBody  c a cc x lambdasX
            in  (XCast a cc x', r)
        

        CastRun 
         -> let (x', r)  = enterCastBody c a cc x lambdasX
            in  (XCast a cc x', r)


---------------------------------------------------------------------------------------------------
-- | Check if this is a context that we should lift lambda abstractions out of.
isLiftyContext :: Ctx a n -> Bool
isLiftyContext ctx
 = case ctx of
        -- Don't lift out of the top-level context.
        -- There's nowhere else to lift to.
        --   TODO: handle chain of let bindings from top-level
        CtxTop{}                        -> False
        CtxLetLLet CtxTop{} _ _ _       -> False
        CtxLetLRec CtxTop{} _ _ _ _ _   -> False

        -- Don't lift if we're inside more lambdas.
        --  We want to lift the whole binding group together.
        CtxLAM{}        -> False
        CtxLam{}        -> False

        
        -- Abstraction was let-bound, but not at top-level.
        CtxLetLLet{}    -> True
        CtxLetLRec{}    -> True
        
        -- We can't do code generation for abstractions in these contexts.
        CtxAppLeft{}    -> True
        CtxAppRight{}   -> True
        CtxLetBody{}    -> True
        CtxCaseScrut{}  -> True
        CtxCaseAlt{}    -> True
        CtxCastBody{}   -> True


---------------------------------------------------------------------------------------------------
-- | Construct the call site, and new lifted binding for a lambda lifted
--   abstraction.
--
--   TODO: get types of free vars from the current context.
--   TODO: put new type binders first.

liftLambda 
        :: (Show n, Ord n, CompoundName n)
        => Context a n          -- ^ Context of the original abstraction.
        -> Set (Bool, Bound n)  -- ^ Free variables in the body of the abstraction.
        -> Bool                 -- ^ Whether this is a type-abstraction.
        -> a -> Bind n -> Exp a n
        -> (  Exp a n
            , Bind n, Exp a n)

liftLambda c usFree isTypeLam a b x
 = let  ctx     = contextCtx c
        
        -- Name of the enclosing top-level binding.
        Just nTop   = takeTopNameOfCtx ctx

        -- Name of the new lifted binding/
        nLifted = extendName nTop (encodeCtx ctx)
        uLifted = UName nLifted
        bLifted = BName nLifted tUnit      -- TODO: real type

        -- When binding free varaibles, we don't want to include the one bound
        -- by the current abstraction, or other supers bound at top-level.
        nsSuper = takeTopLetEnvNamesOfCtx ctx
        Just uB = takeSubstBoundOfBind b
        usFree' = Set.filter
                        (\(_, u) -> case u of
                                UName n -> not $ Set.member n nsSuper
                                _       -> True)
                $ Set.delete (isTypeLam, uB) usFree

        -- At the call site, apply all the free variables of the lifted
        -- function as new arguments.    
        makeArg  (True,  u)       = XType a (TVar u)
        makeArg  (False, u)       = XVar a u
        
        xsArg   = map makeArg $ Set.toList usFree'
        xCall   = xApps a (XVar a uLifted) xsArg

        -- For the lifted abstraction, wrap it in new lambdas to bind
        -- all of its free variables.
        makeBind (True,  u@(UName n))
                | Just t       <- Env.lookup u (contextKindEnv c)
                = (True,  BName n t)
        
        makeBind (False, u@(UName n))
                | Just t       <- Env.lookup u (contextTypeEnv c)
                = (False, BName n t)

        makeBind (f, b')
                = error $ "makeBind: nope " ++ show (f, b')

        bsParam = map makeBind 
                $ Set.toList 
                $ Set.filter (\(_, u) -> case u of
                                            UPrim{} -> False
                                            _       -> True)
                $ usFree'
        
        xInner  = if isTypeLam 
                        then XLAM a b x
                        else XLam a b x

        xLifted = makeXLamFlags a bsParam xInner

    in  trace (show nsSuper)
         $ ( xCall
           , bLifted, xLifted)


