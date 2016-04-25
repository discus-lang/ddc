{-# LANGUAGE TypeFamilies, UndecidableInstances #-}

-- | Look at type signatures and add quantifiers to bind any free type
--   variables. Also add holes for missing type arguments.
--   
--   Given
-- @
--    mapS (f : a -> S e b) (xx : List a) : S e (List b)
--     = box case xx of
--        Nil             -> Nil
--        Cons x xs       -> Cons (run f x) (run mapS f xs)
-- @
--
--  We get:
-- @
--    mapS [a e b : ?] (f : a -> S e b) (xx : List a) : S e (List b)
--     = /\(a e b : ?). box case xx of
--        Nil             -> Nil
--        Cons x xs       -> Cons (run f x) (run mapS [?] [?] [?] f xs)
-- @


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
        -> KindEnv (GName l) -> TypeEnv (GName l)
        -> c l -> c l


---------------------------------------------------------------------------------------------------
instance ExpandLanguage l => Expand Module l where
 expand = expandM

expandM config kenv tenv mm
  = let 
        -- Add quantifiers to the types of bindings, and also slurp
        -- out the contribution to the top-level environment from each binding.
        --   We need to do this in an initial binding because each top-level
        --   thing is in-scope of all the others.
        preTop p
         = case p of
                TopClause a (SLet _ b [] [GExp x])
                 -> let (b', x') = expandQuant a config kenv (b, x)
                    in  ( TopClause a (SLet a b' [] [GExp x'])
                        , Env.singleton b')

                TopData _ def
                 -> (p, typeEnvOfDataDef def)

                _ -> error "source-tetra.expand: found TopClause"

        (tops_quant, tenvs)
                = unzip $ map preTop $ moduleTops mm

        -- Build the compound top-level environment.
        tenv'           = Env.unions $ tenv : tenvs

        -- Expand all the top-level definitions.
        tops'           = map (expand config kenv tenv')
                        $ tops_quant

    in  mm { moduleTops = tops' }


---------------------------------------------------------------------------------------------------
instance ExpandLanguage l => Expand Top l where
 expand = expandT

expandT config kenv tenv top
  = case top of
        TopClause a1 (SLet a2 b [] [GExp x])
         -> let tenv'   = Env.extend b tenv
                x'      = expand config kenv tenv' x
            in  TopClause a1 (SLet a2 b [] [GExp x'])

        TopData{} -> top
        _         -> error "source-tetra.expand: found TopClause"


---------------------------------------------------------------------------------------------------
instance ExpandLanguage l => Expand GExp l where
 expand = downX

downX config kenv tenv xx
  = case xx of

        -- Invoke the expander --------
        XVar{}
         ->     expandApp config kenv tenv xx []

        XCon{}
         ->     expandApp config kenv tenv xx []

        XPrim{}
         ->     expandApp config kenv tenv xx []

        XApp{}
         | (x1, xas)     <- takeXAppsWithAnnots xx
         -> if isXVar x1 || isXCon x1
             -- If the function is a variable or constructor then try to expand
             -- extra arguments in the application.
             then let   xas'    = [ (expand config kenv tenv x, a) | (x, a) <- xas ]
                  in    expandApp config kenv tenv x1 xas'

             -- Otherwise just apply the original arguments.
             else let   x1'     = expand config kenv tenv x1
                        xas'    = [ (expand config kenv tenv x, a) | (x, a) <- xas ]
                  in    makeXAppsWithAnnots x1' xas'

        XLet a (LLet b x1) x2
         -> let 
                -- Add missing quantifiers to the types of let-bindings.
                (b_quant, x1_quant)
                        = expandQuant a config kenv (b, x1)

                tenv'   = Env.extend b_quant tenv
                x1'     = expand config kenv tenv' x1_quant
                x2'     = expand config kenv tenv' x2
            in  XLet a (LLet b x1') x2'

        XLet a (LRec bxs) x2
         -> let 
                (bs_quant, xs_quant)
                        = unzip
                        $ [expandQuant a config kenv (b, x) | (b, x) <- bxs]

                tenv'   = Env.extends bs_quant tenv
                xs'     = map (expand config kenv tenv') xs_quant
                x2'     = expand config kenv tenv' x2
            in  XLet a (LRec (zip bs_quant xs')) x2'

        -- LGroups need to be desugared first because any quantifiers
        -- we add to the front of a function binding need to scope over
        -- all the clauses related to that binding.
        XLet a (LGroup [SLet _ b [] [GExp x1]]) x2
         -> expand config kenv tenv (XLet a (LLet b x1) x2)

        XLet _ (LGroup{}) _
         -> error $ "ddc-source-tetra.expand: can't expand sugared LGroup."


        -- Boilerplate ----------------
        XLAM a b x
         -> let kenv'   = Env.extend b kenv
                x'      = expand config kenv' tenv x
            in  XLAM a b x'

        XLam a b x
         -> let tenv'   = Env.extend b tenv
                x'      = expand config kenv tenv' x
            in  XLam a b x'

        XLet a (LPrivate bts mR bxs) x2
         -> let tenv'   = Env.extends bts kenv
                kenv'   = Env.extends bxs tenv
                x2'     = expand config kenv' tenv' x2
            in  XLet a (LPrivate bts mR bxs) x2'

        XCase a x alts  -> XCase a   (downX config kenv tenv x)   
                                     (map (downA config kenv tenv) alts)
        XCast a c x     -> XCast a c (downX config kenv tenv x)
        XType{}         -> xx
        XWitness{}      -> xx
        XDefix a xs     -> XDefix a  (map (downX config kenv tenv) xs)
        XInfixOp{}      -> xx
        XInfixVar{}     -> xx


---------------------------------------------------------------------------------------------------
instance ExpandLanguage l => Expand GAlt l where
 expand = downA

downA config kenv tenv alt
  = case alt of
        AAlt p gsx
         -> let tenv'   = extendPat p tenv
                gsx'    = map (expand config kenv tenv') gsx
            in  AAlt p gsx'


---------------------------------------------------------------------------------------------------
instance ExpandLanguage l => Expand GGuardedExp l where
 expand = downGX

downGX config kenv tenv (GGuard g x)
  = let g'      = expand config kenv tenv g
        tenv'   = extendGuard g' tenv
    in  GGuard g' (expand config kenv tenv' x)

downGX config kenv tenv (GExp x)
  = let x'      = expand config kenv tenv x
    in  GExp x'


---------------------------------------------------------------------------------------------------
instance ExpandLanguage l => Expand GGuard l where
 expand = downG

downG config kenv tenv gg
  = case gg of
        GPat p x
         -> let tenv'   = extendPat p tenv
                x'      = expand config kenv tenv' x
            in  GPat  p x'

        GPred x
         -> let x'      = expand config kenv tenv x
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

expandQuant a config kenv (b, x)
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
        x'      = foldr (XLAM a) x bsNew

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
        => Config l             -- ^ Expander configuration.
        -> KindEnv (GName l)    -- ^ Current kind environment.
        -> TypeEnv (GName l)    -- ^ Current type environment.
        -> GExp l               -- ^ Functional expression being applied.
        -> [(GExp l, GAnnot l)] -- ^ Function arguments.
        -> GExp l

expandApp config _kenv tenv x0 xas0
 | Just (a, u)  <- slurpVarConBound x0
 , Just tt      <- Env.lookup u tenv 
 , not $ isBot tt
 = let
        go t xas
         = case (t, xas) of
                (TForall _b t2, (x1@(XType _ _t1'), a1) : xas')
                 ->     (x1, a1) : go t2 xas'

                (TForall b t2, xas')
                 -> let k       = typeOfBind b
                        Just a0 = takeAnnotOfExp x0
                        xh      = XType a0 (configMakeTypeHole config k)
                    in  (xh, a) : go t2 xas'

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
        => GExp l 
        -> Maybe (GAnnot l, GBound l)
slurpVarConBound xx
 = case xx of
        XVar a u -> Just (a, u)
        XCon a dc 
         | DaConBound n   <- dc -> Just (a, UName n)
         | DaConPrim  n t <- dc -> Just (a, UPrim n t)
        _       -> Nothing

