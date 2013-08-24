
module DDC.Source.Tetra.Infer.Expand
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
import DDC.Type.Env                     (KindEnv, TypeEnv)
import qualified DDC.Type.Env           as Env


-------------------------------------------------------------------------------
-- | Expander configuration.
data Config a n
        = Config
        { -- | Make a type hole of the given kind.
          configMakeTypeHole    :: Kind n -> Type n }


-- | Default expander configuration.
configDefault :: Config a Name
configDefault 
        = Config
        { configMakeTypeHole    = \k -> TVar (UPrim NameHole k)}


-------------------------------------------------------------------------------
class Expand (c :: * -> * -> *) where
 expand
        :: Ord n 
        => Config a n
        -> KindEnv n -> TypeEnv n
        -> c a n     -> c a n


instance Expand Module where
 expand config kenv tenv mm
  = let 
        -- Build an environment with types for all the data constructors.
        tenv_dataCtors  = Env.unions 
                        $ [ typeEnvOfDataDef def | TopData _ def <- moduleTops mm]

        -- Add types of top level bindings.
        tenv_tops       = Env.fromList
                        $ [ b                    | TopBind _ b _ <- moduleTops mm]

        -- Build the compound top-level environment.
        tenv'           = Env.unions
                        $ [tenv, tenv_dataCtors, tenv_tops]

        -- Expand all the top-level definitions.
        tops'           = map (expand config kenv tenv')
                        $ moduleTops mm

    in  mm { moduleTops = tops' }


instance Expand Top where
 expand config kenv tenv top
  = case top of
        TopBind a b x   
         -> let tenv'   = Env.extend b tenv
                x'      = expand config kenv tenv' x
            in  TopBind a b x'

        TopData{}
         -> top


instance Expand Exp where
 expand config kenv tenv xx
  = let down = expand config kenv tenv
    in case xx of

        -- Invoke the expander --------
        XVar{}
         ->     expandApp config kenv tenv xx []

        XCon{}
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


        -- Boilerplate ----------------
        XLAM a b x
         -> let kenv'   = Env.extend b kenv
                x'      = expand config kenv' tenv x
            in  XLAM a b x'

        XLam a b x
         -> let tenv'   = Env.extend b tenv
                x'      = expand config kenv tenv' x
            in  XLam a b x'

        XLet a (LLet b x1) x2
         -> let tenv'   = Env.extend b tenv
                x1'     = expand config kenv tenv' x1
                x2'     = expand config kenv tenv' x2
            in  XLet a (LLet b x1') x2'

        XLet a (LLetRegions bts bxs) x2
         -> let tenv'   = Env.extends bts kenv
                kenv'   = Env.extends bxs tenv
                x2'     = expand config kenv' tenv' x2
            in  XLet a (LLetRegions bts bxs) x2'

        XCase a x alts  -> XCase a   (down x)   (map down alts)
        XCast a c x     -> XCast a c (down x)
        XType{}         -> xx
        XWitness{}      -> xx
        XDefix a xs     -> XDefix a  (map down xs)
        XInfixOp{}      -> xx
        XInfixVar{}     -> xx


instance Expand Alt where
 expand config kenv tenv alt
  = case alt of
        AAlt PDefault x2
         -> let x2'     = expand config kenv tenv x2
            in  AAlt PDefault x2'

        AAlt (PData dc bs) x2
         -> let tenv'   = Env.extends bs tenv
                x2'     = expand config kenv tenv' x2
            in  AAlt (PData dc bs) x2'


-------------------------------------------------------------------------------
-- | Expand missing type arguments in applications.
--   
--   The thing being applied needs to be a variable or data constructor
--   so we can look up its type in the environment. Given the type, look
--   at the quantifiers out the front and insert new type applications if
--   the expression is missing them.
--
expandApp 
        :: Ord n
        => Config a n           -- ^ Expander configuration.
        -> KindEnv n            -- ^ Current kind environment.
        -> TypeEnv n            -- ^ Current type environment.
        -> Exp a n              -- ^ Functional expression being applied.
        -> [(Exp a n, a)]       -- ^ Function arguments.
        -> Exp a n

expandApp config _kenv tenv x0 xas0
 | Just (a, u)  <- slurpVarConBound x0
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
                    in  (xh, a) : go t2 xas'

                _ -> xas

        xas_expanded
                = go tt xas0

   in   makeXAppsWithAnnots x0 xas_expanded

 | otherwise
 = makeXAppsWithAnnots x0 xas0


-- | Slurp a `Bound` from and `XVar` or `XCon`. 
--   Named data constructors are converted to `UName`s.
slurpVarConBound :: Exp a n -> Maybe (a, Bound n)
slurpVarConBound xx
 = case xx of
        XVar a u -> Just (a, u)
        XCon a dc 
         | DaConNamed n <- daConName dc
         -> Just (a, UName n)
        _       -> Nothing

