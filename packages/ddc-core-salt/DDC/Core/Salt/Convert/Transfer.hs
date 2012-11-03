
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
import DDC.Core.Salt.Convert.Base
import DDC.Core.Salt.Runtime
import DDC.Core.Salt.Name
import DDC.Core.Salt.Env
import DDC.Core.Module
import DDC.Core.Exp
import DDC.Core.Predicates
import DDC.Core.Compounds
import DDC.Type.Compounds
import DDC.Core.Check           (AnTEC(..))
import Data.Map                 (Map)
import qualified Data.Map       as Map


transferModule 
        :: Module (AnTEC a Name) Name 
        -> Either (Error  (AnTEC a Name))
                  (Module (AnTEC a Name) Name)

transferModule mm@ModuleCore{}
        | XLet a (LRec bxs) x1  <- moduleBody mm
        = let bxs'        = map transLet bxs
          in  Right $ mm { moduleBody = XLet a (LRec bxs') x1 }

        | otherwise
        = Left (ErrorNoTopLevelLetrec mm)


-- Let ------------------------------------------------------------------------
transLet :: (Bind Name, Exp (AnTEC a Name) Name)
         -> (Bind Name, Exp (AnTEC a Name) Name)

transLet (BName n t, x)
 = let  tails   = Map.singleton n t
        x'      = transSuper tails x
   in   (BName n t, x')

transLet tup
        = tup


-- Super ----------------------------------------------------------------------
transSuper 
        :: Map Name (Type Name)         -- ^ Tail-callable supers, with types.
        -> Exp (AnTEC a Name) Name 
        -> Exp (AnTEC a Name) Name

transSuper tails xx
 = let down = transSuper tails
   in  case xx of
        -- Return the value bound to a var.
        XVar a _        -> xReturn a (annotType a) xx

        -- Return a constructor value.
        XCon a _        -> xReturn a (annotType a) xx

        XLAM  a b x     -> XLAM  a b $ down x
        XLam  a b x     -> XLam  a b $ down x

        -- Tail-call a supercombinator.
        --   The super must have its arguments in standard order,
        --   being type arguments, then witness arguments, then value arguments.
        --   We need this so we can split off just the value arguments and 
        --   pass them to the appropriate tailcallN# primitive.
        XApp{}
         | Just (xv@(XVar a (UName n)), args) <- takeXApps xx
         , Just tF                  <- Map.lookup n tails

         -- Split off args and check they are in standard order.
         , (xsArgsType, xsArgsMore) <- span isXType    args
         , (xsArgsWit,  xsArgsVal)  <- span isXWitness xsArgsMore
         , not $ any isXType    xsArgsVal
         , not $ any isXWitness xsArgsVal

         -- Get the types of the value parameters.
         , (_, tsValArgs, tResult)  <- takeTFunWitArgResult $ eraseTForalls tF

         -> let arity   = length xsArgsVal
                p       = PrimCallTail arity
                u       = UPrim (NamePrim (PrimCall p)) (typeOfPrimCall p)
            in  makeXApps a (XVar a u) 
                        $  (map XType (tsValArgs ++ [tResult])) 
                        ++ [makeXApps a xv (xsArgsType ++ xsArgsWit)] 
                        ++ xsArgsVal

        -- Return the result of this application.
        XApp  a x1 x2   
         -> let x1'     = transX tails x1
                x2'     = transX tails x2
            in  addReturnX a (annotType a) (XApp a x1' x2')

        XLet  a lts x   -> XLet  a (transL tails lts) (down x)
        XCase a x alts  -> XCase a (transX tails x) (map (transA tails) alts)
        XCast a c x     -> XCast a c (transSuper tails x)
        XType{}         -> xx
        XWitness{}      -> xx


-- | Add a statment to return this value, 
--   but don't wrap existing control transfer operations.
addReturnX :: a          -> Type Name
           -> Exp a Name -> Exp a Name
addReturnX a t xx

        -- If there is already a control transfer primitive here then
        -- don't add another one.
        | Just (NamePrim p, _)  <- takeXPrimApps xx
        , PrimControl{}         <- p
        = xx

        -- Wrap the final expression in a return primitive.
        | otherwise
        = xReturn a t xx


-- Let ------------------------------------------------------------------------
transL  :: Map Name (Type Name)         -- ^ Tail-callable supers, with types.
        -> Lets (AnTEC a Name) Name
        -> Lets (AnTEC a Name) Name

transL tails lts
 = case lts of
        LLet mode b x   -> LLet mode b (transX tails x)
        LRec bxs        -> LRec [(b, transX tails x) | (b, x) <- bxs]
        LLetRegions{}   -> lts
        LWithRegion{}   -> lts


-- Alt ------------------------------------------------------------------------
transA  :: Map Name (Type Name)         -- ^ Tail-callable supers, with types.
        -> Alt (AnTEC a Name) Name
        -> Alt (AnTEC a Name) Name

transA tails aa
 = case aa of
        AAlt p x        -> AAlt p (transSuper tails x)


-- Exp ------------------------------------------------------------------------
transX  :: Map Name (Type Name)         -- ^ Tail-callable supers, with types.
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

