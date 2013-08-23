
module DDC.Source.Tetra.Infer.Expand
        ( Config        (..)
        , configDefault
        , Expand        (..))
where
import DDC.Source.Tetra.Compounds
import DDC.Source.Tetra.Module
import DDC.Source.Tetra.Prim
import DDC.Source.Tetra.Exp
import DDC.Type.Env                     (KindEnv, TypeEnv)
import qualified DDC.Type.Env           as Env


-------------------------------------------------------------------------------
data Config a n
        = Config
        { configMakeTypeHole    :: Kind n -> Type n }


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
  = mm { moduleTops     = map (expand config kenv tenv)
                        $ moduleTops mm }


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

        XApp{}
         | (x1, xas)     <- takeXAppsWithAnnots xx
         -> case x1 of
             _ -> let   x1'     = expand config kenv tenv x1
                        xas'    = [ (expand config kenv tenv x, a) | (x, a) <- xas ]
                  in    makeXAppsWithAnnots x1' xas'

        XVar{}          -> xx
        XCon{}          -> xx

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

