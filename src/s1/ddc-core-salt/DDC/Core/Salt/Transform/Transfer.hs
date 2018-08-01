
module DDC.Core.Salt.Transform.Transfer
        (transferModule)
where
import DDC.Core.Salt.Compounds
import DDC.Core.Salt.Name
import DDC.Core.Module
import DDC.Core.Exp.Annot
import DDC.Core.Check           (AnTEC(..))
import Data.Map                 (Map)
import qualified Data.Map       as Map


-- | Add control transfer primops to function bodies.
--   For example:
--
--   @
--      fun \[x : Int32\#\] : Int32\#
--         = case ... of
--                 ...      -> add\# \[Int32\] ...
--                 ...      -> fun x
--
--   => fun \[x : Int32\#\] : Int32\#
--         = case ... of
--                 ...      -> return\# \[Int32\#] (add\# \[Int32\] ...)
--                 ...      -> tailcall1\# \[Int32\#] \[Int32\#\] fun x
--  @
--
--  The control primops return# and tailcall1# tell us how to transfer control
--  after we've finished with the current function.
--
transferModule
        :: Module (AnTEC a Name) Name
        -> Either String
                  (Module (AnTEC a Name) Name)

transferModule mm@ModuleCore{}
        | XLet a (LRec bxs) x1  <- moduleBody mm
        = let bxs'        = map transLet bxs
          in  Right $ mm { moduleBody = XLet a (LRec bxs') x1 }

        | otherwise
        = Left "no top level letrec"


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

        -- Return a primitive value.
        XPrim a _       -> xReturn a (annotType a) xx

        XAbs  a b x     -> XAbs  a b $ down x

        -- Tail-call a supercombinator.
        --   The super must have its arguments in standard order,
        --   being type arguments, then witness arguments, then value arguments.
        --   We need this so we can split off just the value arguments and
        --   pass them to the appropriate tailcallN# primitive.
        XApp _ x1 x2
         | (xv@(XVar a (UName n)), args) <- takeXApps1 x1 x2
         , Just tF                  <- Map.lookup n tails

         -- Split off args and check they are in standard order.
         , (asArgsType, asArgsMore) <- span isRType    args
         , (asArgsWit,  asArgsVal)  <- span isRWitness asArgsMore
         , not $ any isRType    asArgsVal
         , not $ any isRWitness asArgsVal

         -- Get the types of the value parameters.
         , (_, tsValArgs, tResult)  <- takeTFunWitArgResult $ eraseTForalls tF

         -> let arity   = length asArgsVal
                p       = PrimControlTailCall arity
                u       = UName (NamePrimOp (PrimControl p))
            in  xApps a (XVar a u)
                        $  (map RType  (tsValArgs  ++ [tResult]))
                        ++ [RTerm $ xApps a xv (asArgsType ++ asArgsWit)]
                        ++ asArgsVal

        -- Return the result of this application.
        XApp  a x1 x2
         -> let x1'     = transX   tails x1
                a2'     = transArg tails x2
            in  addReturnX a (annotType a) (XApp a x1' a2')

        XLet  a lts x   -> XLet  a (transL tails lts) (down x)
        XCase a x alts  -> XCase a (transX tails x) (map (transA tails) alts)
        XCast a c x     -> XCast a c (transSuper tails x)


-- | Add a statment to return this value,
--   but don't wrap existing control transfer operations.
addReturnX :: a          -> Type Name
           -> Exp a Name -> Exp a Name
addReturnX a t xx

        -- If there is already a control transfer primitive here then
        -- don't add another one.
        | Just (NamePrimOp p, _)  <- takeXNameApps xx
        , case p of
                PrimControl (PrimControlFail)       -> True
                PrimControl (PrimControlReturn)     -> True
                PrimControl (PrimControlCall{})     -> False
                PrimControl (PrimControlTailCall{}) -> True
                _ -> False
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
        LLet b x        -> LLet b (transX tails x)
        LRec bxs        -> LRec [(b, transX tails x) | (b, x) <- bxs]
        LPrivate{}      -> lts


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
        XPrim{}         -> xx
        XCon{}          -> xx
        XAbs{}          -> xx
        XApp  a x1 x2   -> XApp  a (down x1) (transArg tails x2)
        XLet{}          -> xx
        XCase{}         -> xx
        XCast{}         -> xx


-- Arg ------------------------------------------------------------------------
transArg :: Map Name (Type Name)
         -> Arg (AnTEC a Name) Name
         -> Arg (AnTEC a Name) Name

transArg tails aa
 = case aa of
        RType{}         -> aa
        RWitness{}      -> aa
        RTerm x         -> RTerm     $ transX   tails x
        RImplicit x     -> RImplicit $ transArg tails x

