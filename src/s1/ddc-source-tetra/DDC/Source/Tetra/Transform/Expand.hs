{-# LANGUAGE TypeFamilies, UndecidableInstances #-}

-- | Look at type signatures and add quantifiers to bind any free type
--   variables.
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
--        Cons x xs  -> Cons (run f x) (run mapS f xs)
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
import DDC.Source.Tetra.Env             (Env)
import Data.Maybe
import qualified DDC.Source.Tetra.Env   as Env
import qualified Data.Set               as Set
import qualified Data.List              as List


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
                TopClause aT (SLet aL bm ps gxs)
                 -> let (bm', ps') = expandQuantParams env bm ps
                    in  ( TopClause aT (SLet aL bm' ps' gxs)
                        , Env.extendDaVarMT bm' Env.empty)

                -- Clauses should have already desugared.
                TopClause _ SSig{} -> (p, Env.empty)
                TopData   _ def    -> (p, envOfDataDef def)
                TopType{}          -> (p, Env.empty)

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
        TopClause a1 (SLet a2 bm ps gxs)
         -> let env'    = Env.extendDaVarMT bm env
                env''   = List.foldl' (flip extendParam) env' ps
                gxs'    = map (expand a2 env'') gxs
            in  TopClause a1 (SLet a2 bm ps gxs')

        TopClause _ (SSig{})    -> top

        TopData{}               -> top
        TopType{}               -> top


---------------------------------------------------------------------------------------------------
instance Expand Exp where
 expand = downX

downX a env xx
  = case xx of
        XAnnot a' x
         -> downX a' env x

        -- Invoke the expander --------
        XPrim{}         -> xx
        XFrag{}         -> xx
        XVar{}          -> xx
        XCon{}          -> xx

        XApp{}
         | (x1, xas)     <- takeXAppsWithAnnots xx
         -> let x1'      = expand a env x1
                xas'     = [ (expand (fromMaybe a a') env x, a')
                                   | (x, a') <- xas ]
            in  makeXAppsWithAnnots x1' xas'

        XLet (LLet b x1) x2
         -> let x1'     = expand a env x1

                env'    = Env.extendDaVarMT b env
                x2'     = expand a env' x2
            in  XLet (LLet b x1') x2'

        XLet (LRec bxs) x2
         -> let (bs, xs) = unzip bxs
                env'    = Env.extendsDaVarMT bs env

                xs'     = map (expand a env') xs
                bxs'    = zip bs xs'

                x2'     = expand a env' x2
            in  XLet (LRec bxs') x2'

        -- ISSUE #429: In Expand transform, env management
        -- is wrong for non-recursive clauses.
        XLet (LGroup bRec cs) x2
         -> let cs'     = map (downCX a env) cs
                bs      = [b | SLet _ b _ _ <- cs']
                env'    = Env.extendsDaVarMT bs env
                x2'     = downX a env' x2
            in  XLet (LGroup bRec cs') x2'


        -- Boilerplate ----------------
        XAbs bm@(MType b _) x
         -> let env'    = env   & Env.extendTyVar' b
                x'      = expand a env' x
            in  XAbs bm x'

        XAbs bm@(MTerm p _) x
         -> let env'    = env   & extendPat p
                x'      = expand a env' x
            in  XAbs bm x'

        XAbs bm@(MImplicit p _) x
         -> let env'    = env   & extendPat p
                x'      = expand a env' x
            in  XAbs bm x'

        XLet (LPrivate bts mR bxs) x2
         -> let env'    = env   & Env.extendsTyVar' bts
                                & Env.extendsDaVar  bxs
                x2'     = expand a env' x2
            in  XLet (LPrivate bts mR bxs) x2'

        XCase  x alts   -> XCase  (downX a env x)
                                  (map (downA a env) alts)

        XCast  c x      -> XCast  c (downX a env x)

        XDefix a' xs    -> XDefix a' (map (downArg a' env) xs)
        XInfixOp{}      -> xx
        XInfixVar{}     -> xx

        XMatch a' as x   -> XMatch  a' (map (downMA a' env) as) (downX a' env x)
        XWhere a' x cls  -> XWhere  a' (downX a' env x) (map (downCX a env) cls)

        XAbsPat a' ps p mt x
         -> let env'     = extendPat p env
            in  XAbsPat a' ps p mt (downX a' env' x)

        XLamCase a' alts -> XLamCase a (map (downA a' env) alts)


---------------------------------------------------------------------------------------------------
instance Expand Arg where
 expand = downArg

downArg a env arg
 = case arg of
        RType{}         -> arg
        RWitness{}      -> arg
        RTerm x         -> RTerm     (downX   a env x)
        RImplicit arg'  -> RImplicit (downArg a env arg')


---------------------------------------------------------------------------------------------------
instance Expand Clause where
 expand a env cl
  = downCX a env cl

downCX _a env cl
 = case expandQuantClause env cl of
        (_, SSig{})
         -> cl

        (env', SLet a mt ps gxs)
         -> let gxs'   = map (downGX a env') gxs
            in  SLet a mt ps gxs'


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
instance Expand AltCase where
 expand = downA

downA a env alt
  = case alt of
        AAltCase p gsx
         -> let env'    = extendPat p env
                gsx'    = map (expand a env') gsx
            in  AAltCase p gsx'


---------------------------------------------------------------------------------------------------
instance Expand AltMatch where
 expand = downMA

downMA a env alt
  = case alt of
        AAltMatch gx    -> AAltMatch (downGX a env gx)


---------------------------------------------------------------------------------------------------
-- | Extend a type environment with the variables bound by the given pattern.
extendPat :: Pat -> Env -> Env
extendPat ww env
 = case ww of
        PDefault        -> env
        PAt   b p       -> extendPat p $ Env.union env (Env.singletonDaVar' b)
        PVar  b         -> Env.union env (Env.singletonDaVar' b)
        PData{}         -> env


extendParam :: Param -> Env -> Env
extendParam pp env
 = case pp of
        MType      b _  -> Env.union env (Env.singletonTyVar' b)
        MTerm      p _  -> extendPat p env
        MImplicit  p _  -> extendPat p env


-- | Extend a type environment with the variables bound by the given guard.
extendGuard :: Guard -> Env -> Env
extendGuard gg tenv
 = case gg of
        GPat w _        -> extendPat w tenv
        _               -> tenv


---------------------------------------------------------------------------------------------------
expandQuantClause :: Env -> Clause -> (Env, Clause)
expandQuantClause env cc
 = case cc of
        SSig{}
         -> (env, cc)

        SLet a mt ps gxs
         -> let (mt', ps')      = expandQuantParams env mt ps
            in  (env, SLet a mt' ps' gxs)


-- | Expand missing quantifiers in types of bindings.
--
--   If a binding mentions type variables that are not in scope then add new
--   quantifiers to its type, as well as matching type lambdas.
--
expandQuantParams
        :: Env                  -- ^ Current environment.
        -> BindVarMT            -- ^ Type of binding.
        -> [Param]              -- ^ Parameters of binding.
        -> (BindVarMT, [Param]) -- ^ Expanded type and body of binding.

expandQuantParams env bmBind ps
 | XBindVarMT bBind (Just tBind) <- bmBind
 , fvs                           <- freeVarsT  env tBind
 , not $ Set.null fvs
 = let
        -- Make new binders for each of the free type variables.
        --   We shouldn't have any holes or indices in the incoming type,
        --   but don't have a way to specify this in the type of the AST.
        makeBind u
         = case u of
                UName n -> Just $ BName n
                UHole   -> error "ddc-source-tetra.expandQuant: not expanding hole in type"
                UIx{}   -> error "ddc-source-tetra.expandQuant: not expanding deBruijn type var"

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
        ps'     = [MType b Nothing | b <- bsNew] ++ ps

   in   (XBindVarMT bBind (Just tBind'), ps')

 | otherwise
 = (bmBind, ps)

