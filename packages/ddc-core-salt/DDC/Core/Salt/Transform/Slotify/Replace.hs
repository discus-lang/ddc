
module DDC.Core.Salt.Transform.Slotify.Replace
        (replaceX)
where
import DDC.Core.Exp.Annot
import Data.Map                         (Map)
import qualified DDC.Core.Salt          as A
import qualified Data.Map               as Map


-- | Replace variables with expressions in the given expression.
--   This does capturing substitution.
replaceX :: Map A.Name (Exp a A.Name)
         -> Exp a A.Name
         -> Exp a A.Name

replaceX sub xx
 = let down     = replaceX sub
   in  case xx of
        XVar _a (UName n)
         -> case Map.lookup n sub of
                Just x' -> x'
                Nothing -> xx

        XVar{}          -> xx
        XPrim{}         -> xx
        XCon{}          -> xx
        XAbs  a b x     -> XAbs  a b (down x)
        XApp  a x1 x2   -> XApp  a   (down x1) (replaceArg sub x2)
        XLet  a lts x2  -> XLet  a   (replaceL sub lts) (down x2)
        XCase a x alts  -> XCase a   (down x) (map (replaceA sub) alts)
        XCast a c x     -> XCast a c (down x)


-- | Replace variables with expressions in the given argument.
replaceArg :: Map A.Name (Exp a A.Name)
           -> Arg a A.Name -> Arg a A.Name

replaceArg sub aa
 = case aa of
        RType{}         -> aa
        RWitness{}      -> aa
        RTerm x         -> RTerm     $ replaceX sub x
        RImplicit x     -> RImplicit $ replaceX sub x


-- | Replace variables with expressions in the given let bindings.
replaceL :: Map A.Name (Exp a A.Name)
         -> Lets a A.Name
         -> Lets a A.Name

replaceL sub lts
 = case lts of
        LLet b x        -> LLet b (replaceX sub x)
        LRec bxs        -> LRec [(b, replaceX sub x) | (b, x) <- bxs]
        LPrivate{}      -> lts


-- | Replace variables with expressions in the given alternative.
replaceA :: Map A.Name (Exp a A.Name)
         -> Alt a A.Name
         -> Alt a A.Name

replaceA sub (AAlt w x)
 = AAlt w (replaceX sub x)

