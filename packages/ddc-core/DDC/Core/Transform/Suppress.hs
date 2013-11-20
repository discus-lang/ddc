
module DDC.Core.Transform.Suppress
        ( suppress
        , Config (..)
        , configZero)
where
import DDC.Core.Compounds
import DDC.Core.Module
import DDC.Core.Exp


-- | Suppression config.
data Config
        = Config
        { -- | Suppress type annotations on let-bindings.
          --   Core program will still type check after this.
          configLetTypes        :: Bool }        


-- | Suppression config with all flags set to `False`.
configZero :: Config
configZero
        = Config
        { configLetTypes        = False }


-- | Suppress various aspects of a core thing.
class Suppress (c :: * -> * -> *) where
 suppress :: Config -> c a n -> c a n


instance Suppress Module where
 suppress config mm
  = mm  { moduleBody = suppress config (moduleBody mm) }


instance Suppress Exp where
 suppress config xx
  = let down = suppress config
    in case xx of
        XVar{}          -> xx
        XCon{}          -> xx
        XLAM  a b x     -> XLAM  a b (down x)
        XLam  a b x     -> XLam  a b (down x)
        XApp  a x1 x2   -> XApp  a   (down x1)  (down x2)
        XLet  a lts x   -> XLet  a   (down lts) (down x)
        XCase a x alts  -> XCase a   (down x)   (map down alts)
        XCast a c x     -> XCast a   c          (down x)
        XType{}         -> xx
        XWitness{}      -> xx


instance Suppress Lets where
 suppress config lts
  = let down = suppress config
    in case lts of
        LLet b x
         | configLetTypes config
         -> let b'      = replaceTypeOfBind (tBot kData) b
            in  LLet b' (down x)

         | otherwise
         -> LLet b (down x)

        LRec bxs        -> LRec [(b, down x) | (b, x) <- bxs]
        LPrivate{}      -> lts
        LWithRegion{}   -> lts


instance Suppress Alt where
 suppress config aa
  = let down = suppress config
    in case aa of
        AAlt p x        -> AAlt p (down x)

