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
        ( expandModule
        , Expand        (..))
where
import DDC.Source.Tetra.Collect.FreeVars
import DDC.Source.Tetra.Exp
import DDC.Source.Tetra.DataDef
import DDC.Source.Tetra.Module
import DDC.Data.SourcePos
import Data.Function
import qualified DDC.Source.Tetra.Env   as Env
import DDC.Source.Tetra.Env             (Env)
import Data.Maybe
import qualified Data.Set               as Set


-- | Run the expander on the given module.
expandModule :: SourcePos -> Module Source -> Module Source
expandModule sp mm
 = expand sp Env.empty mm


---------------------------------------------------------------------------------------------------
class Expand c where
 -- | Add quantifiers to the types of binders. Also add holes for missing
 --   type arguments.
 expand :: SourcePos -> Env -> c -> c 


---------------------------------------------------------------------------------------------------
instance Expand (Module Source) where
 expand = expandM

expandM a env mm
  = let 
        -- Add quantifiers to the types of bindings, and also slurp
        -- out the contribution to the top-level environment from each binding.
        --   We need to do this in an initial binding because each top-level
        --   thing is in-scope of all the others.
        preTop p
         = case p of
                TopClause aT (SLet aL  bm [] [GExp x])
                 -> let (bm', x') = expandQuant env   bm x
                    in  ( TopClause aT (SLet aL bm' [] [GExp x'])
                        , Env.extendDaVarMT bm' Env.empty)

                -- Clauses should have already desugared.
                TopClause{}
                 -> error $ "source-tetra.expand: can't expand sugared TopClause."
                          ++ show p

                TopData _ def   -> (p, envOfDataDef def)
                TopType{}       -> (p, Env.empty)

        (tops_quant, envs)
                = unzip $ map preTop $ moduleTops mm

        -- Build the compound top-level environment.
        env'    = Env.unions $ env : envs

        -- Expand all the top-level definitions.
        tops'   = map (expand a env')
                $ tops_quant

    in  mm { moduleTops = tops' }


---------------------------------------------------------------------------------------------------
instance Expand (Top Source) where
 expand = expandT

expandT _a env top
 = case top of
        TopClause a1 (SLet a2 bm [] [GExp x])
         -> let env'    = Env.extendDaVarMT bm env
                x'      = expand a2 env' x
            in  TopClause a1 (SLet a2 bm [] [GExp x'])

        TopClause{}
         -> error "source-tetra.expand: can't expand sugared TopClause."

        TopData{} -> top
        TopType{} -> top

---------------------------------------------------------------------------------------------------
instance Expand Exp where
 expand = downX

downX a env xx
  = case xx of
        XAnnot a' x
         -> downX a' env x

        -- Invoke the expander --------
        XVar{}  -> expandApp a env xx []
        XCon{}  -> expandApp a env xx []
        XPrim{} -> expandApp a env xx []

        XApp{}
         | (x1, xas)     <- takeXAppsWithAnnots xx
         -> if isXVar x1 || isXCon x1
             -- If the function is a variable or constructor then try to expand
             -- extra arguments in the application.
             then let   xas'     = [ (expand (fromMaybe a a') env x, a') 
                                   | (x, a') <- xas ]
                  in    expandApp a env x1 xas'

             -- Otherwise just apply the original arguments.
             else let   x1'      = expand a env x1
                        xas'     = [ (expand (fromMaybe a a') env x, a') 
                                   | (x, a') <- xas ]
                  in    makeXAppsWithAnnots x1' xas'

        XLet (LLet b x1) x2
         -> let 
                -- Add missing quantifiers to the types of let-bindings.
                (b_quant, x1_quant)
                        = expandQuant env b x1

                env'    = Env.extendDaVarMT b_quant env
                x1'     = expand a env' x1_quant
                x2'     = expand a env' x2
            in  XLet (LLet b x1') x2'

        XLet (LRec bxs) x2
         -> let 
                (bs_quant, xs_quant)
                        = unzip
                        $ [expandQuant env b x | (b, x) <- bxs]

                env'    = Env.extendsDaVarMT  bs_quant env
                xs'     = map (expand a env') xs_quant
                x2'     = expand a env' x2
            in  XLet (LRec (zip bs_quant xs')) x2'

        -- LGroups need to be desugared first because any quantifiers
        -- we add to the front of a function binding need to scope over
        -- all the clauses related to that binding.
        XLet (LGroup [SLet _ b [] [GExp x1]]) x2
         -> expand a env (XLet (LLet b x1) x2)

        -- This should have already been desugared.
        XLet (LGroup{}) _
         -> error $ "ddc-source-tetra.expand: can't expand sugared LGroup."


        -- Boilerplate ----------------
        XLAM bm@(XBindVarMT b _) x
         -> let env'    = env   & Env.extendDaVar' b
                x'      = expand a env' x
            in  XLAM bm x'

        XLam bm@(XBindVarMT b _) x
         -> let env'    = env   & Env.extendDaVar' b 
                x'      = expand a env' x
            in  XLam bm x'

        XLet (LPrivate bts mR bxs) x2
         -> let env'    = env   & Env.extendsTyVar' bts
                                & Env.extendsDaVar  bxs
                x2'     = expand a env' x2
            in  XLet (LPrivate bts mR bxs) x2'

        XCase  x alts   -> XCase  (downX a env x)   
                                  (map (downA a env) alts)

        XCast  c x      -> XCast  c (downX a env x)
        XType{}         -> xx
        XWitness{}      -> xx
        XDefix a' xs    -> XDefix a' (map (downX a' env) xs)
        XInfixOp{}      -> xx
        XInfixVar{}     -> xx


---------------------------------------------------------------------------------------------------
instance Expand Alt where
 expand = downA

downA a env alt
  = case alt of
        AAlt p gsx
         -> let env'    = extendPat p env
                gsx'    = map (expand a env') gsx
            in  AAlt p gsx'


---------------------------------------------------------------------------------------------------
instance Expand GuardedExp where
 expand = downGX

downGX a env (GGuard g x)
  = let g'      = expand  a env g
        env'    = extendGuard g' env
    in  GGuard g' (expand a env' x)

downGX a env (GExp x)
  = let x'      = expand  a env x
    in  GExp x'


---------------------------------------------------------------------------------------------------
instance Expand Guard where
 expand = downG

downG a env gg
  = case gg of
        GPat p x
         -> let env'    = extendPat p env
                x'      = expand    a env' x
            in  GPat  p x'

        GPred x
         -> let x'      = expand a env x
            in  GPred x'

        GDefault
         -> GDefault 


---------------------------------------------------------------------------------------------------
-- | Extend a type environment with the variables bound by the given pattern.
extendPat :: Pat -> Env -> Env
extendPat ww env
 = case ww of
        PDefault        -> env
        PData _ bs      -> Env.unions (env : map Env.singletonDaVar' bs)


-- | Extend a type environment with the variables bound by the given guard.
extendGuard :: Guard -> Env -> Env
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
        :: Env                  -- ^ Current environment.
        -> BindVarMT            -- ^ Type of binding.
        -> Exp                  -- ^ Body of binding.
        -> (BindVarMT, Exp)     -- ^ Expanded type and body of binding.

expandQuant env bmBind xBind
 | XBindVarMT bBind (Just tBind) <- bmBind
 , fvs                           <- freeVarsT  env tBind
 , not $ Set.null fvs
 = let  
        -- Make new binders for each of the free type variables.
        --  TODO: handle free debruijn indices. Find the maximum index and
        --  add enough anonymous binders to cover it.
        makeBind u
         = case u of 
                UName n -> Just $ BName n
                UHole   -> error "ddc-source-tetra.expandQuant: holes should not be free"
                UIx{}   -> error "ddc-source-tetra.expandQuant: not expanding deBruijn tyvar"

        Just bsNew = sequence $ map makeBind $ Set.toList fvs

        -- Attach quantifiers to the front of the old type,
        --   using a hole bound to indicate we want the type inferencer,
        --   to infer a kind or this.
        k       = TVar UHole
        tBind'  = foldr (\b t -> TApp (TCon (TyConForall k)) (TAbs b k t)) tBind bsNew

        -- Attach type lambdas to the front of the term
        --   using a matching hole bound on the type abstraction.
        --   We could instead just not include a kind, but use
        --   a hole so the form of the term matches the form 
        --   of its type.
        xBind'     = foldr (\b1 x1 -> XLAM (XBindVarMT b1 (Just (TVar UHole))) x1)
                           xBind bsNew

   in   (XBindVarMT bBind (Just tBind'), xBind')

 | otherwise
 = (bmBind, xBind)


---------------------------------------------------------------------------------------------------
-- | Expand missing type arguments in applications.
--   
--   The thing being applied needs to be a variable or data constructor
--   so we can look up its type in the environment. Given the type, look
--   at the quantifiers out the front and insert new type applications if
--   the expression is missing them.
--
expandApp 
        :: SourcePos                    -- ^ Default annotation
        -> Env                          -- ^ Environment.
        -> Exp                          -- ^ Functional expression being applied.
        -> [(Exp, Maybe SourcePos)]     -- ^ Function arguments.
        -> Exp

expandApp a0 env x0 xas0
 | Just (ma, _x, Just tt) <- slurpVarConBound a0 env x0
 = let
        go t xas
         | Just (_kParam, _bParam, tBody) <- takeTForall t
         = case xas of
                (x1@(XType _t1'), a1) : xas'
                 -> (x1, a1) : go tBody xas'

                xas'
                 -> (XType (TVar UHole), ma) : go tBody xas'

         | otherwise
         = xas

   in   makeXAppsWithAnnots x0 (go tt xas0)

 | otherwise
 = makeXAppsWithAnnots x0 xas0


-- | Slurp a `Bound` from and `XVar` or `XCon`. 
--   Named data constructors are converted to `UName`s.
slurpVarConBound 
        :: SourcePos
        -> Env -> Exp
        -> Maybe (Maybe SourcePos, Exp, Maybe Type)

slurpVarConBound a env xx
 = case xx of
        XAnnot a' x
         -> slurpVarConBound a' env x

        XVar u 
         -> let mt = Env.takePresent (Env.lookupDaVar env u)
            in  Just (Just a, xx, mt)

        XCon dc 
         -> case dc of
                DaConUnit
                 -> Nothing

                DaConBound n
                 -> let mt = Env.lookupDaCon n env
                    in  Just (Just a, xx, mt) 

                DaConPrim _n t
                 -> Just (Just a, xx, Just t)

        _ -> Nothing

