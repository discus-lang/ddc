
module DDC.Core.Transform.Lambdas
        ( lambdasModule
        , encodeBind
        , topNameOfCtx)
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
import DDC.Type.Env             (KindEnv, TypeEnv)
import Data.Set                 (Set)
import qualified DDC.Type.Env   as Env
import qualified Data.Set       as Set
import Debug.Trace
import Data.Monoid

---------------------------------------------------------------------------------------------------
-- TODO
--   data Context n
--      contextCtx
--      contextKEnv     
--      contextTEnv      
--

-- | Check if this is a context that we should lift lambda abstractions out of.
isLiftyContext :: Ctx a n -> Bool
isLiftyContext ctx
 = case ctx of
        -- Can't lift out of the top-level context.
        CtxTop          -> False
        
        -- Don't lift if we're inside more lambdas.
        --  We want to lift the whole binding group together.
        CtxLAM{}        -> False
        CtxLam{}        -> False

        -- Don't lift out of the top-level context.
        -- There's nowhere else to lift to.
        -- TODO: handle chain of let bindings from top-level
        CtxLetLLet CtxTop _ _ _         -> False
        CtxLetLRec CtxTop _ _ _ _ _     -> False

        -- Abstraction was let-bound, but not at top-level.
        CtxLetLLet{}    -> True
        CtxLetLRec{}    -> True
        
        -- We can't do code generation for abstractions in these contexts.
        CtxAppLeft{}    -> True
        CtxAppRight{}   -> True
        CtxLetBody{}    -> True
        CtxCaseScrut{}  -> True
        CtxCaseAlt{}    -> True
        CtxCast{}       -> True
        

-- | Convert a context to a name extension that we can use for an abstraction 
--   lifted out of there. These names are long and ugly, but we'll fix them
--   up once we find all the things that need lifting. 
--
--   We use these contextual names instead of simply generating a globally
--   fresh integer so that they don't change when other bindings are added
--   to the top-level environment,
encodeCtx :: Ctx a n -> String
encodeCtx _ = "do_encode_context"


-- | Produce a unique string from a Bind.
encodeBind :: Pretty n => Bind n -> String
encodeBind b
 = case b of
        BNone _          -> "z"
        BName n _        -> "a" ++ (renderPlain $ ppr n)
        BAnon _          -> "n"


topNameOfCtx :: Ctx a n -> Maybe n
topNameOfCtx ctx0
 = eat ctx0
 where  eat ctx
         = case ctx of
                CtxTop
                 -> Nothing
                
                CtxLetLLet CtxTop _ (BName n _) _
                 -> Just n

                CtxLetLRec CtxTop _ _ (BName n _) _ _
                 -> Just n

                _ -> case takeEnclosingCtx ctx of
                        Nothing   -> Nothing
                        Just ctx' -> eat ctx'


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
                 [Lets a n]             -- Lifted bindings

instance Ord n => Monoid (Result a n) where
 mempty
  = Result Set.empty []
 
 mappend (Result s1 lts1) (Result s2 lts2)
  = Result (Set.union s1 s2) (lts1 ++ lts2)


-- Exp --------------------------------------------------------------------------------------------
-- When leaving a lambda abs, when leaving inner most one then chop it.
--
--  TODO: pass up free type variables in type sigs,
--        eg on the types of lambda and let binders.
--        might have \(x : Thing a). ()
--
-- TODO: factor out entering context and mantaining environment into separate code
--       can we make some higher order functions to enter each level.
--  
--       enterLAM 
--              :: Context a n  
--              -> a -> Bind n  -> Exp a n
--              -> (Context a n -> Exp a n -> b) -> b
--
--      enterLAM ctx a0 b0 x0
--       $ \ctx x
--
--      (xR, mR)  = enterAppLeft  context a0 xL xR lambdas
--      (xL, mL)  = enterAppRight context a0 xL xR lambdas
--      ...

lambdasX :: (Show n, Show a, Pretty n, CompoundName n, Ord n)
         => Context a n
         -> Exp a n 
         -> ( Exp a n               -- Replacement expression
            , Result a n)           -- Lifter result passed up
         
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
                xx'     = XLam a b x'
                liftMe  = isLiftyContext (contextCtx c)

            in trace (unlines ["* LAM LEAVE " ++ show liftMe])
                $ if liftMe
                  then let ctx     = contextCtx c
                           Just n  = topNameOfCtx ctx
                           u       = UName (extendName n (encodeCtx ctx))
                       in  (XVar a u, r)
                  else (xx', r)

        XLam a b x0
         -> enterLam c a b x0 $ \c' x
         -> let (x', r) = lambdasX c' x
                xx'     = XLam a b x'
                liftMe  = isLiftyContext (contextCtx c)

            in trace (unlines ["* Lam LEAVE " ++ show liftMe])
                $ if liftMe
                  then let ctx     = contextCtx c
                           Just n  = topNameOfCtx ctx
                           u       = UName (extendName n (encodeCtx ctx))
                       in  (XVar a u, r)
                  else (xx', r)

        XApp a x1 x2
         -> let (x1', r1)       = enterAppLeft  c a x1 x2 lambdasX
                (x2', r2)       = enterAppRight c a x1 x2 lambdasX
            in  ( XApp a x1' x2'
                , mappend r1 r2)
                
        XLet a lts x
         -> let kenv            = contextKindEnv c
                tenv            = contextTypeEnv c
                ctx             = contextCtx     c
                (lts', r1)      = lambdasLets   kenv tenv ctx a x lts
                
                (x',   r2)      = enterLetBody  c a lts x lambdasX
            in  ( XLet a lts' x'
                , mappend r1 r2)
                
        XCase a x alts
         -> let (x',    r1)     = enterCaseScrut c a x alts lambdasX

                kenv            = contextKindEnv c
                tenv            = contextTypeEnv c
                ctx             = contextCtx c
                (alts', r2)     = lambdasAlts kenv tenv ctx a x [] alts
            in  ( XCase a x' alts'
                , mappend r1 r2)

        XCast a cc x
         -> let 
                kenv            = contextKindEnv c
                tenv            = contextTypeEnv c
                ctx             = contextCtx c
            in  lambdasCast kenv tenv ctx a cc x
                
        XType _ t
         -> let 
                kenv    = contextKindEnv c

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
lambdasLets
        :: (Show a, Show n, Ord n, Pretty n, CompoundName n)
        => KindEnv n -> TypeEnv n -> Ctx a n   
        -> a -> Exp a n
        -> Lets a n
        -> (Lets a n, Result a n)
           
lambdasLets kenv tenv ctx a xBody lts
 = case lts of
        LLet b x
         -> let tenv'     = Env.extend b tenv
                c'        = Context kenv tenv' (CtxLetLLet ctx a b xBody)
                (x', r)   = lambdasX c' x
            in  (LLet b x', r)

        LRec bxs
         -> let tenv'     = Env.extends (map fst bxs) tenv
                (bxs', r) = lambdasLetRec kenv tenv' ctx a xBody [] bxs
            in  (LRec bxs', r)
                
        LPrivate bsR mParent bsW
         -> let 
                kenv'   = Env.extends bsR kenv

                -- Free type variables in the witness bindings.
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


lambdasLetRec _ _ _ _ _ _ []
        = ([], Result Set.empty [])

lambdasLetRec kenv tenv ctx a xBody 
        bxsAcc ((b, x) : bxsMore)
 = let 
        c'       = Context kenv tenv (CtxLetLRec ctx a bxsAcc b bxsMore xBody)
        (x', r1) = lambdasX c' x
        
        (bxs', r2)
         = lambdasLetRec kenv tenv ctx a xBody 
                ((b, x') : bxsAcc) bxsMore

   in   ( (b, x') : bxs'
        , mappend r1 r2)


-- Alts -------------------------------------------------------------------------------------------
lambdasAlts 
        :: (Show a, Show n, Ord n, Pretty n, CompoundName n)
        => KindEnv n -> TypeEnv n -> Ctx a n   
        -> a -> Exp a n
        -> [Alt a n] -> [Alt a n]
        -> ([Alt a n], Result a n)
           
lambdasAlts _ _ _ _ _ _ []
        = ([], Result Set.empty [])

lambdasAlts kenv tenv ctx a xScrut
        altsAcc (AAlt w x : altsMore)
 = let
        c'       = Context kenv tenv (CtxCaseAlt ctx a xScrut altsAcc w altsMore)
        (x', r1) = lambdasX c' x

        (alts', r2)
         = lambdasAlts kenv tenv ctx a xScrut
                (AAlt w x' : altsAcc) altsMore

   in   ( AAlt w x' : alts'
        , mappend r1 r2)


-- Cast -------------------------------------------------------------------------------------------
lambdasCast kenv tenv ctx a cc x
  = case cc of
        CastWeakenEffect eff    
         -> let us      = Set.map (\u -> (True, u)) $ freeVarsT kenv eff
                
                c'      = Context kenv tenv (CtxCast ctx a cc)
                (x', r2) = lambdasX c' x
            in  ( XCast a cc x'
                , mappend (Result us []) r2)

        CastWeakenClosure{}
         ->    error "ddc-core-simpl.lambdas: closures not handled."

        CastPurify w
         -> let -- Get the free variables in this witness.
                supp    = support kenv tenv w

                us      = Set.unions
                          [ Set.map (\u -> (True,  u)) $ supportSpVar supp
                          , Set.map (\u -> (False, u)) $ supportWiVar supp
                          , Set.map (\u -> (False, u)) $ supportDaVar supp ]

                c'       = Context kenv tenv (CtxCast ctx a cc)
                (x', r2) = lambdasX c' x

            in  ( XCast a cc x'
                , mappend (Result us []) r2)

        CastForget w
         -> let -- Get the free variables in this witness.
                supp    = support kenv tenv w

                us      = Set.unions
                        [ Set.map (\u -> (True,  u)) $ supportSpVar supp
                        , Set.map (\u -> (False, u)) $ supportWiVar supp
                        , Set.map (\u -> (False, u)) $ supportDaVar supp ]

                c'       = Context kenv tenv (CtxCast ctx a cc)
                (x', r2) = lambdasX c' x

            in  ( XCast a cc x'
                , mappend (Result us []) r2)

        CastBox 
         -> let c'      = Context kenv tenv (CtxCast ctx a cc)
                (x', r) = lambdasX c' x
            in  (XCast a cc x', r)
        

        CastRun 
         -> let c'      = Context kenv tenv (CtxCast ctx a cc)
                (x', r) = lambdasX c' x
            in  (XCast a cc x', r)

