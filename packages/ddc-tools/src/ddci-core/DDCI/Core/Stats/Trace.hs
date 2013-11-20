
module DDCI.Core.Stats.Trace
        (traceStore)
where
import DDC.Core.Eval.Store
import DDC.Core.Eval.Name
import DDC.Type.Compounds
import DDC.Core.Compounds
import DDC.Core.Exp
import qualified Data.Set       as Set
import Data.Set                 (Set)


-- | Replace non-recursive store locations in an expression by their values.
--  
--   * If the value is recursive then we just leave the original store location.
--   * Constructors in the result just have *0 for their type annotation.
--
traceStore :: Store -> Exp () Name -> Exp () Name
traceStore store xx
        = traceStoreX store Set.empty xx


-- | Trace an expression.
traceStoreX :: Store -> Set Name -> Exp () Name -> Exp () Name
traceStoreX store entered xx
 = let down = traceStoreX store entered
   in  case xx of
        XVar _ (UPrim n@(NameLoc l) _)
         | not $ Set.member n entered
         , Just sbind           <- lookupBind l store
         -> traceStoreX store (Set.insert n entered) (expOfSBind sbind)

        XVar{}          -> xx
        XCon{}          -> xx
        XApp  a x1 x2   -> XApp  a (down x1) (down x2)
        XLAM  a b x     -> XLAM  a b (down x)
        XLam  a b  x    -> XLam  a b (down x)
        XLet  a ls x    -> XLet  a (traceStoreLs store entered ls) (down x)
        XCase a x alts  -> XCase a (down x) (map (traceStoreA store entered) alts)
        XCast a c x     -> XCast a c (down x)
        XType{}         -> xx
        XWitness{}      -> xx


-- | Trace lets.
traceStoreLs :: Store -> Set Name -> Lets () Name -> Lets () Name
traceStoreLs store entered ls
 = let down = traceStoreX store entered 
   in case ls of
        LLet b x        -> LLet b (down x)
        LRec bxs        -> LRec [(b, down x) | (b, x) <- bxs]
        LPrivate{}      -> ls
        LWithRegion{}   -> ls


-- | Trace case alts.
traceStoreA  :: Store -> Set Name -> Alt () Name  -> Alt () Name
traceStoreA store entered (AAlt p x)
        = AAlt p (traceStoreX store entered x)


-- | Convert a store binding to an expression.
expOfSBind :: SBind -> Exp () Name
expOfSBind sbind
 = case sbind of
        SObj dcTag lsArgs
         -> xApps () (XCon () dcTag) (map expOfLoc lsArgs)

        SLams fbs x
         -> makeXLamFlags () fbs x

        SThunk x
         -> x


-- | Convert a store location to a constructor expression.
expOfLoc :: Loc -> Exp () Name
expOfLoc l = XVar () (UPrim (NameLoc l) (tBot kData))

