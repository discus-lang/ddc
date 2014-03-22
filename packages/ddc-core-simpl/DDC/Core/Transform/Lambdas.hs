
module DDC.Core.Transform.Lambdas
        ( lambdasModule
        , lambdas )
where
import DDC.Core.Collect.Support
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
import Data.List



---------------------------------------------------------------------------------------------------
-- | TODO: define a full zipper based on this idea.
--   This would be a generally useful thing to have.
--   Rebake this into a snoc list.
--
data Context n
        = ContextLAM      (Bind n)
        | ContextLam      (Bind n)
        | ContextLetBind  (Bind n)
        | ContextLetBody
        | ContextAppLeft
        | ContextAppRight
        | ContextCaseScrut
        | ContextCaseAlt
        | ContextCast
        deriving Show


-- | Check if this is a context that we should lift lambda abstractions out of.
isLiftyContexts :: [Context n] -> Bool
isLiftyContexts ctx
 = case ctx of
        -- Don't lift if we're inside more lambdas.
        --  We want to lift the whole binding group together.
        ContextLAM{}     : _  -> False
        ContextLam{}     : _  -> False

        -- Don't lift out of the top-level context.
        -- There's nowhere else to lift to.
        ContextLetBind _ : [] -> False

        -- Abstraction was let-bound, but not at top-level.
        ContextLetBind _ : _  -> True
        
        -- Always lift out of these. 
        -- We can't do code generation for abstractions in these contexts.
        ContextLetBody   : _  -> True
        ContextAppLeft   : _  -> True
        ContextAppRight  : _  -> True
        ContextCaseScrut : _  -> True
        ContextCaseAlt   : _  -> True
        ContextCast      : _  -> True

        -- Lambdas should not appear at top-level.
        []                    -> False


-- | Convert a context to a name extension that we can use for an abstraction 
--   lifted out of there. These names are long and ugly, but we'll fix them
--   up once we find all the things that need lifting. 
--
--   We use these contextual names instead of simply generating a globally
--   fresh integer so that they don't change when other bindings are added
--   to the top-level environment,
encodeContexts :: Pretty n => [Context n] -> String
encodeContexts ctxs
 = concat $ intersperse "$$" $ map encodeContext ctxs
   

-- | Produce a unique string from a single context specifier.
encodeContext :: Pretty n => Context n -> String
encodeContext ctx
 = case ctx of
        ContextLAM b     -> "L" ++ encodeBind b
        ContextLam b     -> "l" ++ encodeBind b
        ContextLetBind b -> "b" ++ encodeBind b
        ContextLetBody   -> "o"
        ContextAppLeft   -> "e"
        ContextAppRight  -> "r"
        ContextCaseScrut -> "s" 
        ContextCaseAlt   -> "a"                         -- TODO: disambiguate via case tag
        ContextCast      -> "c"


-- | Produce a unique string from a Bind.
encodeBind :: Pretty n => Bind n -> String
encodeBind b
 = case b of
        BNone _          -> "z"
        BName n _        -> "a" ++ (renderPlain $ ppr n)
        BAnon _          -> "n"


-- | Get the name of the top-level let binding in a context, 
--   if there is one.
topNameOfContexts :: [Context n] -> Maybe n
topNameOfContexts ctxs
 = eat ctxs
 where  eat ctx
         = case ctx of
                []                                -> Nothing
                ContextLetBind (BName n _) : []   -> Just n
                _                          : ctx' -> eat ctx'


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

        (x', _us, _lts) = lambdas kenv tenv [] 
                        $ moduleBody mm

   in   mm { moduleBody = x' }


---------------------------------------------------------------------------------------------------
class Lambdas (c :: * -> * -> *) where
 
 -- | Perform lambda lifting in a thing.
 lambdas :: (Show n, Show a, Pretty n, CompoundName n, Ord n)
         => KindEnv n               -- ^ Kind environment.
         -> TypeEnv n               -- ^ Type environment.
         -> [Context n]             -- ^ Current context.
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
                (x', us, lts)   = lambdas kenv' tenv (ContextLAM b : ctx) x
                xx'             = XLAM a b x'

                liftMe          = isLiftyContexts ctx

            in trace (unlines
                       $ [ "* LAM LEAVE " ++ show liftMe ]
                        ++ map show ctx)
                $ if liftMe
                  then let Just n  = topNameOfContexts ctx
                           u       = UName (extendName n (encodeContexts ctx))
                       in  (XVar a u, us, lts)
                  else (xx', us, lts)

        XLam a b x
         -> let tenv'           = Env.extend b tenv

                (x', us, lts)   = lambdas kenv tenv' (ContextLam b : ctx) x

                xx'             = XLam a b x'
                liftMe          = isLiftyContexts ctx

            in  trace (unlines
                        $  [ "* Lam LEAVE " ++ show (isLiftyContexts ctx) ]
                        ++ map show ctx)
                $ if liftMe
                  then let Just n  = topNameOfContexts ctx
                           u       = UName (extendName n (encodeContexts ctx))
                       in  (XVar a u, us, lts)
                  else (xx', us, lts)

        XApp a x1 x2
         -> let (x1', us1, lts1)   = lambdas kenv tenv   (ContextAppLeft  : ctx) x1
                (x2', us2, lts2)   = lambdas kenv tenv   (ContextAppRight : ctx) x2
            in  ( XApp a x1' x2'
                , Set.union us1 us2
                , lts1 ++ lts2)

        XLet a lts x
         -> let (lts', usA, ltsA)  = lambdas kenv tenv    ctx lts
                (bs1, bs0)         = bindsOfLets lts
                kenv'              = Env.extends bs1 kenv
                tenv'              = Env.extends bs0 tenv
                (x',   usB, ltsB)  = lambdas kenv' tenv'  (ContextLetBody : ctx) x
            in  ( XLet a lts' x'
                , Set.union usA usB
                , ltsA ++ ltsB)

        XCase a x alts
         -> let (x',    usA, ltsA) = lambdas     kenv tenv (ContextCaseScrut : ctx) x
                (alts', usB, ltsB) = lambdasAlts kenv tenv ctx alts
            in  ( XCase a x' alts'
                , Set.union usA usB
                , ltsA ++ ltsB)

        XCast a c x
         -> let (c',    usA, ltsA) = lambdas kenv tenv (ContextCast : ctx) c
                (x',    usB, ltsB) = lambdas kenv tenv (ContextCast : ctx) x
            in  ( XCast a c' x'
                , Set.union usA usB
                , ltsA ++ ltsB)

        XType _ t
         -> let -- Get the free variables in this type.
                us      = Set.map (\u -> (True, u)) $ freeVarsT kenv t

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
instance Lambdas Lets where
 lambdas kenv tenv ctx lts
  = case lts of
        LLet b x
         -> let tenv'           = Env.extend b tenv
                (x', us, lts')  = lambdas kenv tenv' (ContextLetBind b : ctx) x
            in  (LLet b x', us, lts')

        LRec bxs
         -> let tenv'            = Env.extends (map fst bxs) tenv
            
                (xs', uss, ltss) 
                  = unzip3 
                  $ map (\(b, x) -> lambdas kenv tenv' (ContextLetBind b : ctx) x) 
                  $ bxs
            
            in  ( LRec (zip (map fst bxs) xs')
                , Set.unions uss
                , concat ltss)

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


-- Alts -------------------------------------------------------------------------------------------
lambdasAlts 
        :: (Show a, Show n, Ord n, Pretty n, CompoundName n)
        => KindEnv n -> TypeEnv n 
        -> [Context n]
        -> [Alt a n]
        -> ( [Alt a n]   
           , Set (Bool, Bound n) 
           , [Lets a n])         

lambdasAlts kenv tenv ctx aa
 = case aa of
    []
     -> ([], Set.empty, [])
 
    alt : alts
     -> let  (alt',  usA, ltsA)  = lambdas     kenv tenv ctx alt
             (alts', usB, ltsB)  = lambdasAlts kenv tenv ctx alts
        in   ( alt' : alts'
             , Set.union usA usB
             , ltsA ++ ltsB)


-- Alt --------------------------------------------------------------------------------------------
instance Lambdas Alt where
 lambdas kenv tenv ctx alt
  = case alt of
        AAlt p x
         -> let bs              = bindsOfPat p
                tenv'           = Env.extends bs tenv
                (x', us, lts)   = lambdas kenv tenv' (ContextCaseAlt : ctx) x
            in  (AAlt p x', us, lts)


-- Cast -------------------------------------------------------------------------------------------
instance Lambdas Cast where
 lambdas kenv tenv _ cc
  = case cc of
        CastWeakenEffect eff    
         -> let us      = Set.map (\u -> (True, u)) $ freeVarsT kenv eff
            in (cc, us, [])

        CastWeakenClosure{}
         ->    error "ddc-core-simpl.lambdas: closures not handled."

        CastPurify w
         -> let -- Get the free variables in this witness.
                supp    = support kenv tenv w

                us      = Set.unions
                        [ Set.map (\u -> (True,  u)) $ supportSpVar supp
                        , Set.map (\u -> (False, u)) $ supportWiVar supp
                        , Set.map (\u -> (False, u)) $ supportDaVar supp ]

            in  (cc, us, [])

        CastForget w
         -> let -- Get the free variables in this witness.
                supp    = support kenv tenv w

                us      = Set.unions
                        [ Set.map (\u -> (True,  u)) $ supportSpVar supp
                        , Set.map (\u -> (False, u)) $ supportWiVar supp
                        , Set.map (\u -> (False, u)) $ supportDaVar supp ]

            in  (cc, us, [])

        CastBox -> (cc, Set.empty, [])
        CastRun -> (cc, Set.empty, [])

