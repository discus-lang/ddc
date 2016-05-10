{-# LANGUAGE TypeFamilies, UndecidableInstances #-}

-- | Look at type signatures and add quantifiers to bind any free type
--   variables. Also add holes for missing type arguments.
--   
--   Given
--
-- @
--    mapS (f : a -> S e b) (xx : List a) : S e (List b)
--     = box case xx of
--        Nil        -> Nil
--        Cons x xs  -> Cons (run f x) (run mapS f xs)
-- @
--
--  We get:
--
-- @
--    mapS [a e b : ?] (f : a -> S e b) (xx : List a) : S e (List b)
--     = /\(a e b : ?). box case xx of
--        Nil        -> Nil
--        Cons x xs  -> Cons (run f x) (run mapS [?] [?] [?] f xs)
-- @
--
module DDC.Source.Tetra.Transform.Expand
        ( Config        (..)
        , configDefault
        , Expand        (..))
where
import DDC.Source.Tetra.Compounds
import DDC.Source.Tetra.Predicates
import DDC.Source.Tetra.DataDef
import DDC.Source.Tetra.Module
import DDC.Source.Tetra.Prim
import DDC.Source.Tetra.Exp
import DDC.Type.Collect
import Data.Maybe
import DDC.Type.Env                     (KindEnv, TypeEnv)
import qualified DDC.Type.Env           as Env
import qualified Data.Set               as Set


---------------------------------------------------------------------------------------------------
-- | Expander configuration.
data Config l
        = Config
        { -- | Make a type hole of the given kind.
          configMakeTypeHole    :: Kind (GName l) -> Type (GName l) }


-- | Default expander configuration.
configDefault :: GName l ~ Name => Config l
configDefault 
        = Config
        { configMakeTypeHole    = \k -> TVar (UPrim NameHole k)}


---------------------------------------------------------------------------------------------------
type ExpandLanguage l
        = ( Ord (GName l)
          , GBind  l ~ Bind  (GName l)
          , GBound l ~ Bound (GName l))

class ExpandLanguage l => Expand (c :: * -> *) l where
 -- | Add quantifiers to the types of binders. Also add holes for missing
 --   type arguments.
 expand
        :: Ord (GName l) 
        => Config l
        -> GAnnot l
        -> KindEnv (GName l) -> TypeEnv (GName l)
        -> c l -> c l


---------------------------------------------------------------------------------------------------
instance ExpandLanguage l => Expand Module l where
 expand = expandM

expandM config a kenv tenv mm
  = let 
        -- Add quantifiers to the types of bindings, and also slurp
        -- out the contribution to the top-level environment from each binding.
        --   We need to do this in an initial binding because each top-level
        --   thing is in-scope of all the others.
        preTop p
         = case p of
                TopClause a' (SLet _ b [] [GExp x])
                 -> let (b', x') = expandQuant a' config kenv (b, x)
                    in  ( TopClause a' (SLet a' b' [] [GExp x'])
                        , Env.singleton b')

                TopData _ def
                 -> (p, typeEnvOfDataDef def)

                -- Clauses should have already desugared.
                _ -> error "source-tetra.expand: can't expand sugared TopClause."

        (tops_quant, tenvs)
                = unzip $ map preTop $ moduleTops mm

        -- Build the compound top-level environment.
        tenv'           = Env.unions $ tenv : tenvs

        -- Expand all the top-level definitions.
        tops'           = map (expand config a kenv tenv')
                        $ tops_quant

    in  mm { moduleTops = tops' }


---------------------------------------------------------------------------------------------------
instance ExpandLanguage l => Expand Top l where
 expand = expandT

expandT config _a kenv tenv top
  = case top of
        TopClause a1 (SLet a2 b [] [GExp x])
         -> let tenv'   = Env.extend b tenv
                x'      = expand config a2 kenv tenv' x
            in  TopClause a1 (SLet a2 b [] [GExp x'])

        TopData{} -> top

        -- Clauses should have already been desugared.
        _   -> error "source-tetra.expand: can't expand sugared TopClause."


---------------------------------------------------------------------------------------------------
instance ExpandLanguage l => Expand GExp l where
 expand = downX

downX config a kenv tenv xx
  = let fm = fromMaybe
    in case xx of
        XAnnot a' x
         -> downX config a' kenv tenv x

        -- Invoke the expander --------
        XVar{}
         ->     expandApp config a kenv tenv xx []

        XCon{}
         ->     expandApp config a kenv tenv xx []

        XPrim{}
         ->     expandApp config a kenv tenv xx []

        XApp{}
         | (x1, xas)     <- takeXAppsWithAnnots xx
         -> if isXVar x1 || isXCon x1
             -- If the function is a variable or constructor then try to expand
             -- extra arguments in the application.
             then let   xas'     = [ (expand config (fm a a') kenv tenv x, a') 
                                   | (x, a') <- xas ]
                  in    expandApp config a kenv tenv x1 xas'

             -- Otherwise just apply the original arguments.
             else let   x1'      = expand config a kenv tenv x1
                        xas'     = [ (expand config (fm a a') kenv tenv x, a') 
                                   | (x, a') <- xas ]
                  in    makeXAppsWithAnnots x1' xas'

        XLet (LLet b x1) x2
         -> let 
                -- Add missing quantifiers to the types of let-bindings.
                (b_quant, x1_quant)
                        = expandQuant a config kenv (b, x1)

                tenv'   = Env.extend b_quant tenv
                x1'     = expand config a kenv tenv' x1_quant
                x2'     = expand config a kenv tenv' x2
            in  XLet (LLet b x1') x2'

        XLet (LRec bxs) x2
         -> let 
                (bs_quant, xs_quant)
                        = unzip
                        $ [expandQuant a config kenv (b, x) | (b, x) <- bxs]

                tenv'   = Env.extends bs_quant tenv
                xs'     = map (expand config a kenv tenv') xs_quant
                x2'     = expand config a kenv tenv' x2
            in  XLet (LRec (zip bs_quant xs')) x2'

        -- LGroups need to be desugared first because any quantifiers
        -- we add to the front of a function binding need to scope over
        -- all the clauses related to that binding.
        XLet (LGroup [SLet _ b [] [GExp x1]]) x2
         -> expand config a kenv tenv (XLet (LLet b x1) x2)

        -- This should have already been desugared.
        XLet (LGroup{}) _
         -> error $ "ddc-source-tetra.expand: can't expand sugared LGroup."


        -- Boilerplate ----------------
        XLAM b x
         -> let kenv'   = Env.extend b kenv
                x'      = expand config a kenv' tenv x
            in  XLAM b x'

        XLam b x
         -> let tenv'   = Env.extend b tenv
                x'      = expand config a kenv tenv' x
            in  XLam b x'

        XLet (LPrivate bts mR bxs) x2
         -> let tenv'   = Env.extends bts kenv
                kenv'   = Env.extends bxs tenv
                x2'     = expand config a kenv' tenv' x2
            in  XLet (LPrivate bts mR bxs) x2'

        XCase  x alts   -> XCase  (downX config a kenv tenv x)   
                                  (map (downA config a kenv tenv) alts)
        XCast  c x      -> XCast  c (downX config a kenv tenv x)
        XType{}         -> xx
        XWitness{}      -> xx
        XDefix a' xs    -> XDefix a' (map (downX config a' kenv tenv) xs)
        XInfixOp{}      -> xx
        XInfixVar{}     -> xx


---------------------------------------------------------------------------------------------------
instance ExpandLanguage l => Expand GAlt l where
 expand = downA

downA config a kenv tenv alt
  = case alt of
        AAlt p gsx
         -> let tenv'   = extendPat p tenv
                gsx'    = map (expand config a kenv tenv') gsx
            in  AAlt p gsx'


---------------------------------------------------------------------------------------------------
instance ExpandLanguage l => Expand GGuardedExp l where
 expand = downGX

downGX config a kenv tenv (GGuard g x)
  = let g'      = expand  config a kenv tenv g
        tenv'   = extendGuard g' tenv
    in  GGuard g' (expand config a kenv tenv' x)

downGX config a kenv tenv (GExp x)
  = let x'      = expand config a kenv tenv x
    in  GExp x'


---------------------------------------------------------------------------------------------------
instance ExpandLanguage l => Expand GGuard l where
 expand = downG

downG config a kenv tenv gg
  = case gg of
        GPat p x
         -> let tenv'   = extendPat p tenv
                x'      = expand config a kenv tenv' x
            in  GPat  p x'

        GPred x
         -> let x'      = expand config a kenv tenv x
            in  GPred x'

        GDefault
         -> GDefault 


---------------------------------------------------------------------------------------------------
-- | Extend a type environment with the variables bound by the given pattern.
extendPat 
        :: ExpandLanguage l
        => GPat l -> TypeEnv (GName l) -> TypeEnv (GName l)
extendPat ww tenv
 = case ww of
        PDefault        -> tenv
        PData _  bs     -> Env.extends bs tenv


-- | Extend a type environment with the variables bound by the given guard.
extendGuard
        :: ExpandLanguage l
        => GGuard l -> TypeEnv (GName l) -> TypeEnv (GName l)
extendGuard gg tenv
 = case gg of
        GPat w _        -> extendPat w tenv
        _               -> tenv


---------------------------------------------------------------------------------------------------
-- | Expand missing quantifiers in types of bindings.
--  
--   If a binding mentions type variables that are not in scope then add new
--   quantifiers to its type, as well as matching type lambdas.
--
expandQuant 
        :: ExpandLanguage l
        => GAnnot l             -- ^ Annotation to use on new type lambdas.
        -> Config l             -- ^ Expander configuration.
        -> KindEnv  (GName l)   -- ^ Current kind environment.
        -> (GBind l, GExp l)    -- ^ Binder and expression of binding.
        -> (GBind l, GExp l)

expandQuant _a config kenv (b, x)
 | fvs  <- freeVarsT kenv (typeOfBind b)
 , not $ Set.null fvs
 = let  
        -- Make binders for each of the free type variables.
        --   We set these to holes so the Core type inferencer will determine
        --   their kinds for us.
        kHole   = configMakeTypeHole config sComp
        makeBind u
         = case u of 
                UName n         -> Just $ BName n kHole
                UIx{}           -> Just $ BAnon kHole
                _               -> Nothing

        Just bsNew = sequence $ map makeBind $ Set.toList fvs

        -- Attach quantifiers to the front of the old type.
        t'      = foldr TForall  (typeOfBind b) bsNew
        b'      = replaceTypeOfBind t' b

        -- Attach type lambdas to the front of the expression.
        x'      = foldr XLAM x bsNew

   in   (b', x')

 | otherwise
 = (b, x)


---------------------------------------------------------------------------------------------------
-- | Expand missing type arguments in applications.
--   
--   The thing being applied needs to be a variable or data constructor
--   so we can look up its type in the environment. Given the type, look
--   at the quantifiers out the front and insert new type applications if
--   the expression is missing them.
--
expandApp 
        :: ExpandLanguage l
        => Config l                     -- ^ Expander configuration.
        -> GAnnot l                     -- ^ Default annotation
        -> KindEnv (GName l)            -- ^ Current kind environment.
        -> TypeEnv (GName l)            -- ^ Current type environment.
        -> GExp l                       -- ^ Functional expression being applied.
        -> [(GExp l, Maybe (GAnnot l))] -- ^ Function arguments.
        -> GExp l

expandApp config a0 _kenv tenv x0 xas0
 | Just (ma, u) <- slurpVarConBound a0 x0
 , Just tt      <- Env.lookup u tenv 
 , not $ isBot tt
 = let
        go t xas
         = case (t, xas) of
                (TForall _b t2, (x1@(XType _t1'), a1) : xas')
                 ->     (x1, a1) : go t2 xas'

                (TForall b t2, xas')
                 -> let k       = typeOfBind b
                        xh      = XType (configMakeTypeHole config k)
                    in  (xh, ma) : go t2 xas'

                _ -> xas

        xas_expanded
                = go tt xas0

   in   makeXAppsWithAnnots x0 xas_expanded

 | otherwise
 = makeXAppsWithAnnots x0 xas0


-- | Slurp a `Bound` from and `XVar` or `XCon`. 
--   Named data constructors are converted to `UName`s.
slurpVarConBound 
        :: GBound l ~ Bound (GName l)
        => GAnnot l
        -> GExp   l 
        -> Maybe (Maybe (GAnnot l), GBound l)

slurpVarConBound a xx
 = case xx of
        XAnnot a' x
         -> slurpVarConBound a' x

        XVar u 
         -> Just (Just a, u)

        XCon dc 
         | DaConBound n   <- dc -> Just (Just a, UName n)
         | DaConPrim  n t <- dc -> Just (Just a, UPrim n t)

        _       -> Nothing

