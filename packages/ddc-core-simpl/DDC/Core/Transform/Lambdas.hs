
module DDC.Core.Transform.Lambdas
        ( lambdasModule
        , lambdas 
        , encodeBind
        , topNameOfCtx)
where
import DDC.Core.Collect.Support
import DDC.Core.Exp.AnnotCtx
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

        (x', _us, _lts) = lambdas kenv tenv CtxTop
                        $ moduleBody mm

   in   mm { moduleBody = x' }


---------------------------------------------------------------------------------------------------
class Lambdas (c :: * -> * -> *) where
 
 -- | Perform lambda lifting in a thing.
 lambdas :: (Show n, Show a, Pretty n, CompoundName n, Ord n)
         => KindEnv n               -- ^ Kind environment.
         -> TypeEnv n               -- ^ Type environment.
         -> Ctx a n                 -- ^ Current context.
         -> c a n 
         -> ( c a n                 -- ^ Replacement expression.
            , Set (Bool, Bound n)   -- ^ Free variables.
            , [Lets a n])           -- ^ Lifted bindings


-- Exp --------------------------------------------------------------------------------------------
-- When leaving a lambda abs, when leaving inner most one then chop it.
--
--  TODO: pass up free type variables in type sigs,
--        eg on the types of lambda and let binders.
--        might have \(x : Thing a). ()
--
-- TODO: factor out usFree, lts into a monoid, use <> to combine them 
--       factor out entering context and mantaining environment into separate code
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


instance Lambdas Exp where
 lambdas kenv tenv ctx xx
  = case xx of
        XVar _ u
         -> let usFree  
                 | Env.member u tenv    = Set.singleton (False, u)
                 | otherwise            = Set.empty

            in  (xx, usFree, [])

        XCon{}          
         ->     (xx, Set.empty, [])

        XLAM a b x      
         -> let kenv'           = Env.extend b kenv
                (x', us, lts)   = lambdas kenv' tenv (CtxLAM ctx a b) x
                xx'             = XLAM a b x'

                liftMe          = isLiftyContext ctx

            in trace (unlines
                       $ [ "* LAM LEAVE " ++ show liftMe ])
                $ if liftMe
                  then let Just n  = topNameOfCtx ctx
                           u       = UName (extendName n (encodeCtx ctx))
                       in  (XVar a u, us, lts)
                  else (xx', us, lts)

        XLam a b x
         -> let tenv'           = Env.extend b tenv

                (x', us, lts)   = lambdas kenv tenv' (CtxLam ctx a b) x

                xx'             = XLam a b x'
                liftMe          = isLiftyContext ctx

            in  trace (unlines
                        $  [ "* Lam LEAVE " ++ show (isLiftyContext ctx)])
                        
                $ if liftMe
                  then let Just n  = topNameOfCtx ctx
                           u       = UName (extendName n (encodeCtx ctx))
                       in  (XVar a u, us, lts)
                  else (xx', us, lts)

        XApp a x1 x2
         -> let (x1', us1, lts1)   = lambdas kenv tenv   (CtxAppLeft  ctx a x2) x1
                (x2', us2, lts2)   = lambdas kenv tenv   (CtxAppRight ctx a x1) x2
            in  ( XApp a x1' x2'
                , Set.union us1 us2
                , lts1 ++ lts2)

        XLet a lts x
         -> let (lts', usA, ltsA)  = lambdasLets kenv tenv ctx a x lts
                (bs1, bs0)         = bindsOfLets lts
                kenv'              = Env.extends bs1 kenv
                tenv'              = Env.extends bs0 tenv
                (x',   usB, ltsB)  = lambdas kenv' tenv' (CtxLetBody ctx a lts) x
            in  ( XLet a lts' x'
                , Set.union usA usB
                , ltsA ++ ltsB)

        XCase a x alts
         -> let (x',    usA, ltsA) = lambdas     kenv tenv (CtxCaseScrut ctx a alts) x
                (alts', usB, ltsB) = lambdasAlts kenv tenv ctx a x [] alts
            in  ( XCase a x' alts'
                , Set.union usA usB
                , ltsA ++ ltsB)

        XCast a c x
         -> lambdasCast kenv tenv ctx a c x
                
        XType _ t
         -> let -- Get the free variables in this type.
                us      = Set.map (\u -> (True, u))  $ freeVarsT kenv t

            in  (xx, us, [])

        XWitness _ w
         -> let -- Get the free variables in this witness.
                supp    = support kenv tenv w

                us      = Set.unions
                        [ Set.map (\u -> (True,  u)) $ supportSpVar supp
                        , Set.map (\u -> (False, u)) $ supportWiVar supp
                        , Set.map (\u -> (False, u)) $ supportDaVar supp ]

            in  (xx, us, [])


-- Lets -------------------------------------------------------------------------------------------
lambdasLets
        :: (Show a, Show n, Ord n, Pretty n, CompoundName n)
        => KindEnv n -> TypeEnv n -> Ctx a n   
        -> a -> Exp a n
        -> Lets a n
        -> ( Lets a n
           , Set (Bool, Bound n) 
           , [Lets a n])         

lambdasLets kenv tenv ctx a xBody lts
 = case lts of
        LLet b x
         -> let tenv'             = Env.extend b tenv
                (x', us', lts')   = lambdas kenv tenv' (CtxLetLLet ctx a b xBody) x
            in  (LLet b x', us', lts')

        LRec bxs
         -> let tenv'             = Env.extends (map fst bxs) tenv
                (bxs', us', lts') = lambdasLetRec kenv tenv' ctx a xBody [] bxs
            in  (LRec bxs', us', lts')
                
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

            in (lts, us, [])

        LWithRegion _
         ->     (lts, Set.empty, [])


lambdasLetRec _ _ _ _ _ _ []
        = ([], Set.empty, [])

lambdasLetRec kenv tenv ctx a xBody 
        bxsAcc ((b, x) : bxsMore)
 = let 
        (x',   usA, ltsA)   
         = lambdas kenv tenv 
                (CtxLetLRec ctx a bxsAcc b bxsMore xBody) 
                x
        
        (bxs', usB, ltsB)
         = lambdasLetRec kenv tenv ctx a xBody 
                ((b, x') : bxsAcc) bxsMore

   in   ( (b, x') : bxs'
        , Set.union usA usB
        , ltsA ++ ltsB)


-- Alts -------------------------------------------------------------------------------------------
lambdasAlts 
        :: (Show a, Show n, Ord n, Pretty n, CompoundName n)
        => KindEnv n -> TypeEnv n -> Ctx a n   
        -> a -> Exp a n
        -> [Alt a n] -> [Alt a n]
        -> ( [Alt a n]   
           , Set (Bool, Bound n) 
           , [Lets a n])         

lambdasAlts _ _ _ _ _ _ []
        = ([], Set.empty, [])

lambdasAlts kenv tenv ctx a xScrut
        altsAcc (AAlt w x : altsMore)
 = let
        (x',    usA, ltsA)
         = lambdas kenv tenv 
                (CtxCaseAlt ctx a xScrut altsAcc w altsMore)
                x

        (alts', usB, ltsB)
         = lambdasAlts kenv tenv ctx a xScrut
                (AAlt w x' : altsAcc) altsMore

   in   ( AAlt w x' : alts'
        , Set.union usA usB
        , ltsA ++ ltsB)



-- Cast -------------------------------------------------------------------------------------------

lambdasCast kenv tenv ctx a cc x
  = case cc of
        CastWeakenEffect eff    
         -> let usA             = Set.map (\u -> (True, u)) $ freeVarsT kenv eff
                (x', usB, ltsB) = lambdas kenv tenv (CtxCast ctx a cc) x
            in  (XCast a cc x', Set.union usA usB, ltsB)

        CastWeakenClosure{}
         ->    error "ddc-core-simpl.lambdas: closures not handled."

        CastPurify w
         -> let -- Get the free variables in this witness.
                supp    = support kenv tenv w

                usA     = Set.unions
                        [ Set.map (\u -> (True,  u)) $ supportSpVar supp
                        , Set.map (\u -> (False, u)) $ supportWiVar supp
                        , Set.map (\u -> (False, u)) $ supportDaVar supp ]

                (x', usB, ltsB) = lambdas kenv tenv (CtxCast ctx a cc) x

            in  (XCast a cc x', Set.union usA usB, ltsB)

        CastForget w
         -> let -- Get the free variables in this witness.
                supp    = support kenv tenv w

                usA     = Set.unions
                        [ Set.map (\u -> (True,  u)) $ supportSpVar supp
                        , Set.map (\u -> (False, u)) $ supportWiVar supp
                        , Set.map (\u -> (False, u)) $ supportDaVar supp ]

                (x', usB, ltsB) = lambdas kenv tenv (CtxCast ctx a cc) x

            in  (XCast a cc x', Set.union usA usB, ltsB)

        CastBox 
         -> let (x', us, lts)   = lambdas kenv tenv (CtxCast ctx a cc) x
            in  (XCast a cc x', us, lts)
        

        CastRun 
         -> let (x', us, lts)   = lambdas kenv tenv (CtxCast ctx a cc) x
            in  (XCast a cc x', us, lts)


