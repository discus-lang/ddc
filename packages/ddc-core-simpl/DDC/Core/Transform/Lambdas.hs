
module DDC.Core.Transform.Lambdas
        ( lambdasModule
        , lambdas )
where
import DDC.Core.Collect.Support
import DDC.Core.Module
import DDC.Core.Compounds
import DDC.Core.Exp
import DDC.Type.Collect
import DDC.Type.Env             (KindEnv, TypeEnv)
import Data.Set                 (Set)
import qualified DDC.Type.Env   as Env
import qualified Data.Set       as Set


-- | Perform lambda lifting in a module.
lambdasModule 
        :: (Show a, Show n, Ord n)
        => Module a n -> Module a n

lambdasModule mm
 = let  
        -- TODO: build initial env
        kenv    = Env.empty
        tenv    = Env.empty

        (x', _us, _lts) = lambdas kenv tenv [] 
                        $ moduleBody mm

   in   mm { moduleBody = x' }


class Lambdas (c :: * -> * -> *) where
 -- | Perform lambda lifting in a thing.
 lambdas :: (Show n, Show a, Ord n)
         => KindEnv n               -- ^ Kind environment.
         -> TypeEnv n               -- ^ Type environment.
         -> [(Bool, Bind n)]        -- ^ Lambda abstractions that we're immediately inside of.
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
 lambdas kenv tenv bsOuter xx
  = case xx of
        XVar _ u
         -> let usFree  
                 | Env.member u tenv    = Set.singleton (False, u)
                 | otherwise            = Set.empty

            in  (xx, usFree, [])

        XCon{}          
         ->     (xx, Set.empty, [])

        XLAM a b x      
         -> let kenv'   = Env.extend b kenv

                (x', us, lts)
                        = lambdas kenv' tenv ((True, b) : bsOuter) x

            in  (XLAM a b x', us, lts)


        XLam a b x
         -> let tenv'   = Env.extend b tenv

                (x', us, lts)
                        = lambdas kenv tenv' ((False, b) : bsOuter) x

            in  (XLam a b x', us, lts)

        XApp a x1 x2
         -> let (x1', us1, lts1)  = lambdas kenv tenv [] x1
                (x2', us2, lts2)  = lambdas kenv tenv [] x2
            in  ( XApp a x1' x2'
                , Set.union us1 us2
                , lts1 ++ lts2)

        XLet a lts x
         -> let (lts', usA, ltsA)  = lambdas kenv  tenv  [] lts
                (bs1, bs0)         = bindsOfLets lts
                kenv'              = Env.extends bs1 kenv
                tenv'              = Env.extends bs0 tenv
                (x',   usB, ltsB)  = lambdas kenv' tenv' [] x
            in  ( XLet a lts' x'
                , Set.union usA usB
                , ltsA ++ ltsB)

        XCase a x alts
         -> let (x',    usA, ltsA) = lambdas     kenv tenv [] x
                (alts', usB, ltsB) = lambdasAlts kenv tenv [] alts
            in  ( XCase a x' alts'
                , Set.union usA usB
                , ltsA ++ ltsB)

        XCast a c x
         -> let (c',    usA, ltsA) = lambdas kenv tenv [] c
                (x',    usB, ltsB) = lambdas kenv tenv [] x
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
 lambdas kenv tenv bsOuter lts
  = case lts of
        LLet b x
         -> let tenv'           = Env.extend b tenv
                (x', us, lts')  = lambdas kenv tenv' bsOuter x
            in  (LLet b x', us, lts')

        LRec bxs
         -> let (bs, xs)         = unzip bxs
                tenv'            = Env.extends bs tenv
                (xs', uss, ltss) = unzip3 $ map (lambdas kenv tenv' bsOuter) xs
            in  ( LRec (zip bs xs')
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
        :: (Show a, Show n, Ord n)
        => KindEnv n -> TypeEnv n 
        -> [(Bool, Bind n)]
        -> [Alt a n]
        -> ( [Alt a n]   
           , Set (Bool, Bound n) 
           , [Lets a n])         

lambdasAlts kenv tenv bsOuter aa
 = case aa of
    []
     -> ([], Set.empty, [])
 
    alt : alts
     -> let  (alt',  usA, ltsA)  = lambdas     kenv tenv bsOuter alt
             (alts', usB, ltsB)  = lambdasAlts kenv tenv bsOuter alts
        in   ( alt' : alts'
             , Set.union usA usB
             , ltsA ++ ltsB)


-- Alt --------------------------------------------------------------------------------------------
instance Lambdas Alt where
 lambdas kenv tenv bsOuter alt
  = case alt of
        AAlt p x
         -> let bs              = bindsOfPat p
                tenv'           = Env.extends bs tenv
                (x', us, lts)   = lambdas kenv tenv' bsOuter x
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

