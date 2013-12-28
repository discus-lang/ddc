-- | Manage boxing in a module.
--
--   Given a module that uses primitive types like Nat#, rewrite it to use the
--   explicitly boxed and unboxed versions, like (B# Nat#) and (U# Nat#)
--   respectively. This transform does just enough to make the code well-formed
--   with respect to boxing. Demand-driven optimisations like local unboxing
--   should be done somewhere else.
--
--   Wrap numeric literals:
--       23# 
--     => convert# [B# Nat#] [Nat#] 23#
--
--   Use unboxed versions of primitive operators:
--        add# [Nat#] x y 
--     => convert# [B# Nat#] [U# Nat#] 
--                 (add# [U# Nat#] (convert# [U# Nat#] [B# Nat#] x)
--                                 (convert# [U# Nat#] [B# Nat#] y))
--
--   All other occurrences of primitive types such as Nat# are converted to their
--   boxed versions (like (B# Nat)).
--
-- [Note: Boxed vs Unboxed vs Unrepresented]
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
-- A boxed (B# Nat#) is represented as a heap allocated object, which can be
-- referred to generically by a pointer. An unboxed (U# Nat#) is a primitive 
-- value that is passed around directly (like in a register), but does not 
-- appear naked in the heap. An unrepresented Nat# is a pure value that can 
-- be manipulated in the core program but does not exist directly at runtime.
-- When we wrap literals like (convert# [B# Nat#] [Nat#] 23#) we choose a 
-- representation for the pure literal value.
--
-- [Note: Boxing and Partial Application]
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
-- We do not allow unboxed types in the source program because we don't want to
-- deal with partial applications of functions to unboxed values. With our
-- current setup we always have a version of each function that accepts boxed
-- values, so we never need to do generic application involving unboxed values.
-- Fast-path function specialisations that take unboxed parameters should be
-- created separately, and not replace the existing slow-path, fully boxed version.
--
module DDC.Core.Transform.Boxing
        ( Rep           (..)
        , Config        (..)
        , Boxing        (..))
where
import DDC.Core.Compounds
import DDC.Core.Module
import DDC.Core.Exp
import DDC.Type.Transform.Instantiate
import Data.Maybe


-- | Representation of the values of some type.
data Rep
        -- | Values of this type cannot be directly represented in the target
        --   language. We need to use a boxed or unboxed representation instead.
        = RepNone

        -- | Type is represented in boxed form,
        --   and thus can instantiate polymorphic types.
        | RepBoxed     

        -- | Type is represented in unboxed form,
        --   and thus cannot instantiate polymorphic types.
        | RepUnboxed
        deriving (Eq, Ord, Show)


data Config a n
        = Config
        { -- | Get the representation of some type.
          --   This only needs to work for types of kind Data.
          configRepOfType        :: Type n -> Rep

          -- | Get the boxed version of some data type, if any.
        , configTakeTypeBoxed    :: Type n -> Maybe (Type n) 

          -- | Get the unboxed version of some data type, if any.
        , configTakeTypeUnboxed  :: Type n -> Maybe (Type n) 

          -- | Check if the constructor with this name is a literal.
        , configNameIsLiteral    :: n -> Bool 

          -- | Check if the primop with this name works on unboxed values
          --   directly. Operators where this function returns False are assumed
          --   to take boxed values for every argument.
        , configNameIsUnboxedOp  :: n -> Bool 

          -- | Take the type of a primitive operator, if it is one.
          --   The primops can be polytypic, but must have prenex rank-1 types.
        , configTypeOfPrimOpName :: n -> Maybe (Type n) 

          -- | Box a literal expression.
        , configBoxLiteral       :: a -> n -> Maybe (Exp a n) 

          -- | Box an expression of the given type.
        , configBoxExp           :: a -> Exp a n -> Type n -> Maybe (Exp a n)

          -- | Unbox an expression of the given type.
        , configUnboxExp         :: a -> Exp a n -> Type n -> Maybe (Exp a n) }


class Boxing (c :: * -> * -> *) where
 -- | Rewrite a module to use explitit boxed and unboxed types.
 boxing :: Ord n
        => Config a n
        -> c a n
        -> c a n


instance Boxing Module where
 boxing config mm
  = mm  { moduleBody    = boxing config (moduleBody mm) }


instance Boxing Exp where
 boxing config xx
  = let down = boxing config
    in case xx of

        -- Convert literals to their boxed representations.
        XCon a dc
         |  Just dcn    <- takeNameOfDaCon dc
         ,  configNameIsLiteral config dcn
         ,  Just xx'    <- configBoxLiteral config a dcn
         -> xx'

        -- Wrap primops in conversions to unbox the arguments and box
        -- up the result.
        XApp a _ _
         -- Split the application of a primop into its name and arguments.
         -- The arguments here include type arguments as well.
         | Just (n, xsArgsAll)  <- takeXPrimApps xx
         , Just (xFn, _)        <- takeXApps     xx
         , configNameIsUnboxedOp config n

           -- Instantiate the type of the primop to work out which arguments
           -- need to be unboxed, and which we can leave as-is.
         , Just tPrim           <- configTypeOfPrimOpName config n
         , tsArgs               <- [t | XType _ t <- xsArgsAll]
         , Just tsArgsUnboxed   <- sequence $ map (configTakeTypeUnboxed config) tsArgs
         , xsArgs               <- drop (length tsArgs) xsArgsAll
         , Just tPrimInst       <- instantiateTs tPrim tsArgs
         , (tsArgsInst, tResultInst)
                                <- takeTFunArgResult tPrimInst

           -- We must end up with a type of each argument.
         , length xsArgs == length tsArgsInst
         -> let 
                -- Unbox arguments as nessesary.
                xsArgs' 
                 = [ case configRepOfType config tArg of
                        RepNone    -> xArg
                        RepBoxed   -> xArg
                        RepUnboxed -> fromMaybe xArg 
                                       (configUnboxExp config a xArg tArg)
                        | xArg <- xsArgs
                        | tArg <- tsArgsInst ]

                -- Rebox the result as nessesary.
                fResult x
                 = case configRepOfType config tResultInst of
                        RepNone    -> x
                        RepBoxed   -> x
                        RepUnboxed -> fromMaybe x
                                        (configBoxExp  config a x tResultInst)
            in  fResult 
                 $ xApps a xFn  (  [XType a t | t <- tsArgsUnboxed] 
                                ++ map down xsArgs')

        -- Boilerplate
        XVar{}          -> xx
        XCon{}          -> xx
        XLAM a b x      -> XLAM  a b (down x)
        XLam a b x      -> XLam  a (boxingB config b) (down x)
        XApp a x1 x2    -> XApp  a (down x1)  (down x2)
        XLet a lts x    -> XLet  a (down lts) (down x)
        XCase a x alts  -> XCase a (down x)   (map down alts)
        XCast a c x     -> XCast a c (down x)
        XType{}         -> xx
        XWitness{}      -> xx


instance Boxing Lets where
 boxing config lts
  = let down    = boxing config
    in case lts of
        LLet b x
         -> let b'      = boxingB config b
                x'      = down x
            in  LLet b' x'

        LRec bxs
         -> let bxs'    = [(boxingB config b, down x) 
                                | (b, x) <- bxs]
            in  LRec bxs'

        LPrivate{}      -> lts
        LWithRegion{}   -> lts


instance Boxing Alt where
 boxing config alt
  = case alt of
        AAlt PDefault x 
         -> AAlt PDefault (boxing config x)

        AAlt (PData dc bs) x
         -> AAlt (PData dc (map (boxingB config) bs)) (boxing config x)


-- | Manage boxing in a Bind.
boxingB :: Config a n -> Bind n -> Bind n
boxingB config bb
 = case bb of
        BAnon t         -> BAnon (boxingT config t)
        BName n t       -> BName n (boxingT config t)
        BNone t         -> BNone (boxingT config t)


-- | Manage boxing in a Type.
boxingT :: Config a n -> Type n -> Type n
boxingT config tt
  | RepUnboxed          <- configRepOfType     config tt
  , Just tResult        <- configTakeTypeBoxed config tt
  = tResult

  | otherwise
  = let down = boxingT config
    in case tt of
        TVar{}          -> tt
        TCon{}          -> tt
        TForall b t     -> TForall b (down t)
        TApp t1 t2      -> TApp (down t1) (down t2)
        TSum{}          -> tt

