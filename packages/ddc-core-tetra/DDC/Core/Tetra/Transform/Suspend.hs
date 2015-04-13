
module DDC.Core.Tetra.Transform.Suspend
        (suspendModule)
where
import DDC.Core.Tetra
import DDC.Core.Module
import DDC.Core.Annot.AnTEC
import DDC.Core.Exp
import qualified DDC.Base.Env   as Env

-- TODO: this currently does nothing, changed the plan.

-- | Wrap suspended expressions in lambdas.
-- 
--   Given:  let s = box (f ...)
--           in  ... g (run s) ... h (run s) ...
--
--   The semantics of the 'box' construct are that evaluation of the body
--   should be suspended. The uses of 'run' later in the program then cause
--   the expression to be evaluated.
--
--   This transform eta-expands suspended expressions like so:
--
--           let s = \(_ : Unit). box (f ...)
--           in  ... g (run (s ())) ... h (run (s ())) ...
--
--   After performing this transform the body of the 'box' is suspended
--   by virtue of the outer lambda. We can then convert the program
--   directly to Core-Salt without needing to produce any extra
--   code for run/box.
--
suspendModule 
        :: Module (AnTEC a Name) Name 
        -> Module (AnTEC a Name) Name

suspendModule mm
 = mm { moduleBody = suspend Env.empty (moduleBody mm)}


-- | Records bindings that have been eta-expanded.
type Env        = Env.Env Name () 


-------------------------------------------------------------------------------
-- | Eta-expend suspended bindings expressions in a thing.
class Suspend (c :: * -> * -> *) where
 suspend :: Env -> c a n -> c a n


instance Suspend Exp where
 suspend env xx
  = let down = suspend env
    in case xx of

        -- Boilerplate.
        XVar{}          -> xx
        XCon{}          -> xx
        XLAM a b x      -> XLAM  a b (down x)
        XLam a b x      -> XLam  a b (down x)
        XLet a lts x    -> XLet  a   (down lts) (down x)
        XApp a x1 x2    -> XApp  a   (down x1)  (down x2)
        XCase a x alts  -> XCase a   (down x)   (map down alts)
        XCast a c x     -> XCast a c (down x)
        XType{}         -> xx
        XWitness{}      -> xx


instance Suspend Lets where
 suspend env lts 
  = case lts of
        LLet b x        -> LLet b $ suspend env x
        LRec bxs        -> LRec [(b, suspend env x) | (b, x) <- bxs]
        LPrivate{}      -> lts
        LWithRegion{}   -> lts


instance Suspend Alt where
 suspend env aa
  = case aa of
        AAlt w x        -> AAlt w $ suspend env x

