
module DDC.Core.Transform.Lambdas.Lift
        (liftLambda)
where
import DDC.Core.Transform.Lambdas.Base
import DDC.Core.Fragment
import DDC.Core.Exp.Annot.Context
import DDC.Core.Exp.Annot.Ctx
import DDC.Core.Exp.Annot
import DDC.Core.Collect
import DDC.Base.Pretty
import DDC.Base.Name
import Data.Function
import Data.List
import Data.Set                                 (Set)
import qualified DDC.Core.Check                 as Check
import qualified DDC.Type.Env                   as Env
import qualified DDC.Core.Env.EnvX              as EnvX
import qualified Data.Set                       as Set
import qualified Data.Map                       as Map


---------------------------------------------------------------------------------------------------
-- | Construct the call site, and new lifted binding for a lambda lifted
--   abstraction.
liftLambda 
        :: (Show a, Show n, Pretty n, Ord n, CompoundName n, Pretty a)
        => Profile n            -- ^ Language profile.
        -> Context a n          -- ^ Context of the original abstraction.
        -> Set (Bool, Bound n)  -- ^ Free variables in the body of the abstraction.
        -> a
        -> [(Bool, Bind n)]     -- ^ Parameters of the current lambda abstraction.
        -> Exp a n              -- ^ Body of the current lambda abstraction.
        -> S ( Exp a n
             , Bind n, Exp a n)

liftLambda p c fusFree a bfsParam xBody
 = do   
        -- Name of the enclosing top-level binding.
        let Just nTop       = takeTopNameOfCtx        (contextCtx c)

        -- Names of other supers bound at top-level.
        let nsSuper         = takeTopLetEnvNamesOfCtx (contextCtx c)

        -- The complete abstraction that we're lifting out.
        let xLambda         = makeXLamFlags a bfsParam xBody

        -- Get the list of new parameters we need to add to our super.
        -- These bind all the variables that were free in the abstraction
        -- group that we're lifting out.
        let fusFree_filtered
                = filter (needParamForFreeBound nsSuper bfsParam)
                $ Set.toList fusFree


        -- Lookup the types of those variables from the context,
        -- so we can add the correct types for the parameters of the new super.
        let joinType (f@True, u)
             | Just t <- EnvX.lookupT u (contextEnv c)
             = ((f, u), t)

            joinType (f@False, u)
             | Just t <- EnvX.lookupX u (contextEnv c)
             = ((f, u), t)

            joinType (f, u)
             = error $ unlines
                     [ "ddc-core-simpl.liftLambda: cannot find type of free variable."
                     , show (f, u)
                     , show (Map.keys $ EnvX.envxMap (contextEnv c)) ]

        let futsFree_types
                = map joinType fusFree_filtered


        -- Add in type variables that are free in the types of free
        -- value variables. We need to bind these as well in the new super.
        let expandFree ((f@True,  u), _)
                = [(f, u)]

            expandFree ((f@False, u), t)
                =  [(f, u)]
                ++ [(True, ut) | ut  <- Set.toList
                                     $  freeVarsT Env.empty t]

        let fusFree_body    
                =  [(True, ut) | ut  <- Set.toList 
                                     $  freeVarsT Env.empty $ typeOfExp p c xLambda]

        let futsFree_expandFree
                =  map joinType
                $  Set.toList $ Set.fromList
                $  (concatMap expandFree $ futsFree_types)
                ++ fusFree_body


        -- Sort free vars so the type variables come out the front.
        let futsFree
                = sortBy (compare `on` (not . fst . fst))
                $ futsFree_expandFree


        -- Make the new super.
        --   For the lifted abstraction, wrap it in new lambdas to bind all of
        --   its free variables. 
        --   ISSUE #330: Lambda lifter doesn't work with anonymous binders.
        let makeBind ((True,  (UName n)), t) = (True,  BName n t)
            makeBind ((False, (UName n)), t) = (False, BName n t)
            makeBind fut
                    = error $ "ddc-core-simpl.liftLamba: unhandled binder " ++ show fut
        
        let bsParam = map makeBind futsFree
        let xLifted = makeXLamFlags a bsParam xLambda

        -- Make a binder for the new super.
        (bLifted, uLifted)  
                  <- newVarExtend nTop "L" 
                  $  typeOfExp p c xLifted
        
        -- At the point where the original abstraction group was, 
        -- call our new lifted super instead.
        let makeArg  (True,  u) = XType a (TVar u)
            makeArg  (False, u) = XVar a u

        let xCall = xApps a (XVar a uLifted)
                  $ map makeArg $ map fst futsFree
        
        -- All done.
        return  ( xCall
                , bLifted, xLifted)


---------------------------------------------------------------------------------------------------
-- | Decide whether we need to bind the given variable as a new parameter
--   on the lifted super.
needParamForFreeBound 
        :: (Ord n)
        => Set n                -- ^ Names of supers at top level.
        -> [(Bool, Bind n)]     -- ^ Parameters of the current lambda abstraction.
        -> (Bool,  Bound n)     -- ^ Bound variable, and whether it is a type (True) or not.
        -> Bool

needParamForFreeBound nsSuper bfsParam (bType, u)
        -- We don't need new parameters for supers that are already
        -- at top level, as we can reference those directly.
        | not bType
        , UName n  <- u
        = not $ Set.member n nsSuper

        -- We don't need new parameters for primitives, 
        -- as we can reference those directly.
        | UPrim{}  <- u
        = False
 
        -- We don't need new paramters for variables that
        -- are already bound by the abstractions that we're lifting.
        | any (boundMatchesBind u . snd) bfsParam
        = False
 
        -- We need a new parameter for this free variable.
        | otherwise
        = True


-- Function to get the type of an expression in the given context.
typeOfExp 
        :: (Pretty a, Show a, Show n, Pretty n, Eq n, Ord n)
        => Profile n    -- ^ Language profile.
        -> Context a n  -- ^ Current context.
        -> Exp a n      -- ^ Expression we want the type for.
        -> Type n

typeOfExp p c x
 = case Check.typeOfExp
                (Check.configOfProfile p)
                (contextEnv c) x
    of  Left err
         -> error $ renderIndent $ vcat
            [ text "ddc-core-simpl.liftLambda: type error in lifted expression"
            , ppr err]

        Right t 
         -> t

