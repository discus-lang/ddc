
-- | Add controll transfer primops to function bodies.
--   For example:
--
--   @
--      fun [x : Int32#] : Int32#
--         = case ... of
--                 ...      -> add# [Int32] ...
--                 ...      -> fun x
--
--   => fun [x : Int32#] : Int32#
--         = case ... of
--                 ...      -> return# [Int32#] (add# [Int32] ...)
--                 ...      -> tailcall1# [Int32#] [Int32#] fun x
--  @
--
--  The return# and tailcall1# primops tell us how to transfer control
--  after we've finished with the current function.
--
module DDC.Core.Salt.Convert.Transfer
        (transferModule)
where
import DDC.Core.Module
import DDC.Core.Exp
import DDC.Core.Salt.Name
import Data.Set                 (Set)
import qualified Data.Set       as Set


transferModule :: Module a Name -> Module a Name
transferModule mm
        = mm { moduleBody       = transX Set.empty $ moduleBody mm }


transX :: Set Name -> Exp a Name -> Exp a Name 
transX tails xx
 = let down     = transX tails
   in case xx of
        XVar{}          -> xx
        XCon{}          -> xx
        XLAM  a b x     -> XLAM  a b $ down x
        XLam  a b x     -> XLam  a b $ down x
        XApp  a x1 x2   -> XApp  a (down x1) (down x2)
        XLet  a lts x   -> XLet  a (transL tails lts) (down x)
        XCase a x alts  -> XCase a (down x) (map (transA tails) alts)
        XCast a c x     -> XCast a c (down x)
        XType{}         -> xx
        XWitness{}      -> xx


transL :: Set Name -> Lets a Name -> Lets a Name
transL tails lts
 = let down     = transX tails
   in case lts of
        LLet mode b x   -> LLet mode b (down x)
        LRec bxs        -> LRec [(b, down x) | (b, x) <- bxs]
        LLetRegion{}    -> lts
        LWithRegion{}   -> lts


transA :: Set Name -> Alt a Name -> Alt a Name
transA tails aa
 = case aa of
        AAlt p x        -> AAlt p (transX tails x)

