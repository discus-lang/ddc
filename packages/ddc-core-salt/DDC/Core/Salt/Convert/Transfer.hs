
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
import DDC.Core.Check           (AnTEC(..))
import DDC.Core.Salt.Runtime
import DDC.Core.Salt.Name
import DDC.Core.Salt.Error
import Data.Set                 (Set)
import qualified Data.Set       as Set


transferModule 
        :: Module (AnTEC a Name) Name 
        -> Either (Error  (AnTEC a Name))
                  (Module (AnTEC a Name) Name)

transferModule mm@ModuleCore{}
        | XLet a (LRec bxs) x1  <- moduleBody mm
        = let bxs'        = [ (b, transSuper Set.empty x)
                                        | (b, x) <- bxs ]

          in Right $ mm { moduleBody = XLet a (LRec bxs') x1 }

        | otherwise
        = Left (ErrorNoTopLevelLetrec mm)


-- Super ----------------------------------------------------------------------
transSuper 
        :: Set Name 
        -> Exp (AnTEC a Name) Name 
        -> Exp (AnTEC a Name) Name

transSuper tails xx
 = let down = transSuper tails
   in  case xx of
        XVar a _        -> xReturn a (annotType a) xx
        XCon a _        -> xReturn a (annotType a) xx

        XLAM  a b x     -> XLAM  a b $ down x
        XLam  a b x     -> XLam  a b $ down x

        XApp  a x1 x2   
         -> xReturn a (annotType a) 
          $ XApp  a (transX tails x1) (transX tails x2)

        XLet  a lts x   -> XLet  a (transL tails lts) (down x)
        XCase a x alts  -> XCase a (transX tails x) (map (transA tails) alts)
        XCast a c x     -> XCast a c (transX tails x)
        XType{}         -> xx
        XWitness{}      -> xx


-- Let ------------------------------------------------------------------------
transL  :: Set Name 
        -> Lets (AnTEC a Name) Name
        -> Lets (AnTEC a Name) Name

transL tails lts
 = case lts of
        LLet mode b x   -> LLet mode b (transX tails x)
        LRec bxs        -> LRec [(b, transX tails x) | (b, x) <- bxs]
        LLetRegion{}    -> lts
        LWithRegion{}   -> lts


-- Alt ------------------------------------------------------------------------
transA  :: Set Name
        -> Alt (AnTEC a Name) Name
        -> Alt (AnTEC a Name) Name

transA tails aa
 = case aa of
        AAlt p x        -> AAlt p (transSuper tails x)


-- Exp ------------------------------------------------------------------------
transX  :: Set Name 
        -> Exp (AnTEC a Name) Name 
        -> Exp (AnTEC a Name) Name 

transX tails xx
 = let down     = transX tails
   in case xx of
        XVar{}          -> xx
        XCon{}          -> xx
        XLAM{}          -> xx
        XLam{}          -> xx 
        XApp  a x1 x2   -> XApp  a (down x1) (down x2)
        XLet{}          -> xx 
        XCase{}         -> xx
        XCast{}         -> xx
        XType{}         -> xx
        XWitness{}      -> xx

