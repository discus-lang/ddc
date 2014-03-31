
module DDC.Core.Transform.Lambdas
        (lambdasModule)
where
import DDC.Core.Collect.Support
import DDC.Core.Exp.AnnotCtx
import DDC.Core.Context
import DDC.Core.Module
import DDC.Core.Compounds
import DDC.Core.Exp
import DDC.Type.Collect
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
        c       = Context kenv tenv CtxTop

        (x', _) = lambdasX c $ moduleBody mm

   in   mm { moduleBody = x' }


---------------------------------------------------------------------------------------------------
-- | Result of lambda lifter recursion.
data Result a n
        = Result (Set (Bool, Bound n))  -- Free variables
                 [(Bind n, Exp a n)]    -- Lifted bindings

instance Ord n => Monoid (Result a n) where
 mempty
  = Result Set.empty []
 
 mappend (Result s1 lts1) (Result s2 lts2)
  = Result (Set.union s1 s2) (lts1 ++ lts2)


-- Exp --------------------------------------------------------------------------------------------
-- | Perform lambda lifting in an expression.
--   When leaving a lambda abs, when leaving inner most one then chop it.
--   TODO: handle case where free vars in a lambda have anonymous names
--         When passing up free vars, also pass up the type so we can use
--         it at the binding position.
--
--  TODO: pass up free type variables in type sigs,
--        eg on the types of lambda and let binders.
--        might have \(x : Thing a). ()
--
lambdasX :: (Show n, Show a, Pretty n, CompoundName n, Ord n)
         => Context a n         -- ^ Enclosing context.
         -> Exp a n             -- ^ Expression to perform lambda lifting on.
         -> ( Exp a n           -- Replacement expression
            , Result a n)       -- Lifter result.
         
lambdasX c xx
 = case xx of
        XVar _ u
         -> let tenv    = contextTypeEnv c
                usFree  
                 | Env.member u tenv    = Set.singleton (False, u)
                 | otherwise            = Set.empty

            in  (xx, Result usFree [])

        XCon{}          
         ->     (xx, mempty)

        XLAM a b x0
         -> enterLAM c a b x0 $ \c' x
         -> let (x', r) = lambdasX c' x
                xx'     = XLAM a b x'
                liftMe  = isLiftyContext (contextCtx c)

            in trace (unlines ["* LAM LEAVE " ++ show liftMe])
                $ if liftMe
                  then let 

                        -- Make a binder for the new top-level function.
                        --  TODO: get the real type 
                        ctx       = contextCtx c
                        Just nTop = takeTopNameOfCtx ctx
                        nLifted   = extendName nTop (encodeCtx ctx)
                        uLifted   = UName nLifted
                        bLifted   = BName nLifted tUnit        -- TODO: real type
                           
                        -- Wrap the lifted expression in new lambdas to bind
                        -- all of the free variables.
                        Just nB   = takeNameOfBind b
                        Result us bxs = r
                        makeBind (True,  UName n) = (True,  BName n tUnit)
                        makeBind (False, UName n) = (False, BName n tUnit)
                        makeBind _                = error "makeBind: nope"
                        bs      = map makeBind $ Set.toList 
                                $ Set.delete (True, UName nB) us
                        xFun    = makeXLamFlags a bs (XLAM a b x')
 
                       in  ( XVar a uLifted
                           , Result us (bxs ++ [(bLifted, xFun)]) )
                  else (xx', r)

        XLam a b x0
         -> enterLam c a b x0 $ \c' x
         -> let (x', r) = lambdasX c' x
                xx'     = XLam a b x'
                liftMe  = isLiftyContext (contextCtx c)

            in trace (unlines ["* Lam LEAVE " ++ show liftMe])
                $ if liftMe
                  then let 

                           -- TODO: shift this shared stuff into own funtion.
                           ctx       = contextCtx c
                           Just nTop = takeTopNameOfCtx ctx
                           nLifted   = extendName nTop (encodeCtx ctx)
                           uLifted   = UName nLifted
                           bLifted   = BName nLifted tUnit      -- TODO: real type
                           
                           -- Wrap the lifted expression in new lambdas to bind
                           -- all of the free variables.
                           Just nB   = takeNameOfBind b
                           Result us bxs = r
                           usFree    = Set.delete (False, UName nB) us

                           -- TODO: we're in the scope of the free vars, so can look up 
                           -- their types here.
                           makeBind (True,  UName n) = (True,  BName n tUnit)
                           makeBind (False, UName n) = (False, BName n tUnit)
                           makeBind _                = error "makeBind: nope"
                           bsParam   = map makeBind $ Set.toList usFree
                           xFun      = makeXLamFlags a bsParam (XLam a b x')

                           makeArg  (True,  u)  = XType a (TVar u)
                           makeArg  (False, u)  = XVar a u
                           xsArg     = map makeArg $ Set.toList usFree
                           xApp      = xApps a (XVar a uLifted) xsArg

                       in  ( xApp
                           , Result us (bxs ++ [(bLifted, xFun)]) )
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
                
        XType _ t
         -> let kenv    = contextKindEnv c

                -- Get the free variables in this type.
                us      = Set.map (\u -> (True, u))  $ freeVarsT kenv t
            in  (xx, Result us [])

        XWitness _ w
         -> let 
                kenv    = contextKindEnv c
                tenv    = contextTypeEnv c

                -- Get the free variables in this witness.
                supp    = support kenv tenv w

                us      = Set.unions
                        [ Set.map (\u -> (True,  u)) $ supportSpVar supp
                        , Set.map (\u -> (False, u)) $ supportWiVar supp
                        , Set.map (\u -> (False, u)) $ supportDaVar supp ]

            in  (xx, Result us [])


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
                
        LPrivate bsR mParent bsW
         -> let kenv    = contextKindEnv c
                
                -- Free type variables in the witness bindings.
                kenv'   = Env.extends bsR kenv
                usW     = Set.unions 
                        $ map (freeVarsT kenv')
                        $ map typeOfBind bsW

                usParent 
                 = case mParent of
                        Nothing -> Set.empty
                        Just t  -> freeVarsT kenv t

                us      = Set.map (\u -> (True, u)) 
                        $ Set.union usW usParent
            in  (lts, Result us [])

        LWithRegion _
         ->     (lts, Result Set.empty [])


-- LetRec -----------------------------------------------------------------------------------------
-- | Perform lambda lifting in the right of a single let-rec binding.
lambdasLetRec 
        :: (Show a, Show n, Ord n, Pretty n, CompoundName n)
        => Context a n
        -> a -> [(Bind n, Exp a n)] -> [(Bind n, Exp a n)] -> Exp a n
        -> ([(Bind n, Exp a n)], Result a n)

lambdasLetRec _ _ _ [] _
        = ([], Result Set.empty [])

lambdasLetRec c a bxsAcc ((b, x) : bxsMore) xBody
 = let  (x',   r1) = enterLetLRec  c a bxsAcc b x bxsMore xBody lambdasX

   in   case contextCtx c of
         -- If we're at top-level then drop lifted bindings here.
         CtxTop 
          -> let  (bxs', r2) = lambdasLetRec c a ((b, x') : bxsAcc) bxsMore xBody
                  Result _ bxsLifted = r1
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
        = ([], Result Set.empty [])

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
        CastWeakenEffect eff    
         -> let kenv    = contextKindEnv c
                us      = Set.map (\u -> (True, u)) $ freeVarsT kenv eff
                (x', r) = enterCastBody c a cc x lambdasX
            in  ( XCast a cc x'
                , mappend (Result us []) r)

        -- TODO: The closure typing system is a mess, and doing this
        --       properly would be be hard work, so we just don't bother.
        --       Lambda lifting won't work with the Eval or Lite fragments.
        CastWeakenClosure{}
         ->    error "ddc-core-simpl.lambdas: closures not handled."

        CastPurify w
         -> let -- Get the free variables in the witness.
                kenv    = contextKindEnv c
                tenv    = contextTypeEnv c
                supp    = support kenv tenv w
                us      = Set.unions
                          [ Set.map (\u -> (True,  u)) $ supportSpVar supp
                          , Set.map (\u -> (False, u)) $ supportWiVar supp
                          , Set.map (\u -> (False, u)) $ supportDaVar supp ]

                -- Enter into the body.
                (x', r) = enterCastBody c a cc x lambdasX
                
            in  ( XCast a cc x'
                , mappend (Result us []) r)

        CastForget w
         -> let -- Get the free variables in the witness.
                kenv    = contextKindEnv c
                tenv    = contextTypeEnv c
                supp    = support kenv tenv w
                us      = Set.unions
                        [ Set.map (\u -> (True,  u)) $ supportSpVar supp
                        , Set.map (\u -> (False, u)) $ supportWiVar supp
                        , Set.map (\u -> (False, u)) $ supportDaVar supp ]
                
                -- Enter into the body.
                (x', r) = enterCastBody  c a cc x lambdasX

            in  ( XCast a cc x'
                , mappend (Result us []) r)

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
        CtxTop                          -> False
        CtxLetLLet CtxTop _ _ _         -> False
        CtxLetLRec CtxTop _ _ _ _ _     -> False

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


