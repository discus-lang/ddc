-- | Manage representation of numeric values in a module.
--
--   We use three seprate versions of each numeric type.
--      Nat#            Numeric value type.
--      B# Nat#         Boxed   representation type.
--      U# Nat#         Unboxed representation type.
--
--   A numeric value type is type of pure values like 23#, where "pure value"
--   means the mathematical value, free from any considerations about how that 
--   might be represented at runtime in the physical machine.
--
--   The Boxed and Unboxed representation types commit to a specific runtime
--   representation, and have implications for runtime performance and space 
--   usage of the compiled program.
--
--   The boxing transform takes an input program using just pure values and
--   numeric index types, and refines it to a program that commits to particular
--   representations of those values. 
--
--   This Boxing transform should do  just enough to make the code well-formed
--   with respect to runtime representation. Demand-driven optimisations like
--   local unboxing should be done in follow-on transformations.
--
--   We make the following representation commitments, so that the default
--   representation is boxed.
--
--  [Note: Boxing and Partial Application]
--  ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
--  Unlike in Haskell, we do not allow explictly unboxed types in the source
--  program because we don't want to deal with partial applications of
--  functions to unboxed values. With our current setup we always have a version
--  of each function that accepts boxed values, so we never need to do generic
--  application involving unboxed values. Fast-path function specialisations
--  that take unboxed parameters should be created separately, and not replace
--  the existing slow-path, fully boxed version. Taking this approach is possible
--  in a strict language because the boxed and unboxed values have the same 
--  semantic meaning. Boxing of values does not imply "lifting" of the associated
--  semantic domain.
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


---------------------------------------------------------------------------------------------------
-- | Representation of the values of some type.
data Rep
        -- | These types don't contain any values.
        = RepNone

        -- | Values of this type are uncomitted to a particular representation,
        --   they just describe a set of logical values.
        | RepValue

        -- | Values of this type are represented in boxed form.
        | RepBoxed     

        -- | Values of this type are represented in unboxed form.
        | RepUnboxed
        deriving (Eq, Ord, Show)


data Config a n
        = Config
        { -- | Get the representation of this type.
          configRepOfType               :: Type n -> Maybe Rep

          -- | Get the type for a different representation of the given one.
        , configConvertRepType          :: Rep -> Type n -> Maybe (Type n)

          -- | Take the type of a literal name, if there is one.
        , configValueTypeOfLitName      :: n -> Maybe (Type n)

          -- | Take the type of a primitive operator name, if it is one.
          --   The primops can be polytypic, but must have prenex rank-1 types.
        , configValueTypeOfPrimOpName   :: n -> Maybe (Type n) 

          -- | Take the type of a foreign function name, if it is one.
          --   The function can be polymorphic, but must have a prenex rank-1 type.
        , configValueTypeOfForeignName  :: n -> Maybe (Type n)

          -- | Check if the primop with this name works on unboxed values
          --   directly. Operators where this function returns False are assumed
          --   to take boxed values for every argument.
        , configNameIsUnboxedOp         :: n -> Bool 

          -- | Wrap a value of the given index type.
          --   This will only be passed types where typeNeedsBoxing returns True.
        , configBoxedOfValue    :: a -> Exp a n -> Type n -> Maybe (Exp a n) 

          -- | Unwrap a boxed value of the given index type.
          --   This will only be passed types where typeNeedsBoxing returns True.
        , configValueOfBoxed    :: a -> Exp a n -> Type n -> Maybe (Exp a n)

          -- | Box an unboxed value of the given index type.
          --   This will only be passed types where typeNeedsBoxing returns True.
        , configBoxedOfUnboxed  :: a -> Exp a n -> Type n -> Maybe (Exp a n)

          -- | Unbox a boxed value of the given index type.
          --   This will only be passed types where typeNeedsBoxing returns True.
        , configUnboxedOfBoxed  :: a -> Exp a n -> Type n -> Maybe (Exp a n) }


---------------------------------------------------------------------------------------------------
class Boxing (c :: * -> * -> *) where
 -- | Rewrite a module to use explitit boxed and unboxed types.
 boxing :: (Show n, Show a, Ord n)
        => Config a n
        -> c a n
        -> c a n


-- Module -----------------------------------------------------------------------------------------
instance Boxing Module where
 boxing config mm
  = mm  { moduleBody            = boxing config (moduleBody mm) }  


-- Exp --------------------------------------------------------------------------------------------
instance Boxing Exp where
 boxing config xx
  = let down = boxing config
    in case xx of

        -- When applying a primop that works on unboxed values, 
        -- unbox its arguments and then rebox the result.
        XApp a x1 x2
         -- Split the application of a primop into its name and arguments.
         -- The arguments here include type arguments as well.
         | Just (xFn, tPrim, xsArgsAll) 
                <- splitUnboxedOpApp config xx
         -> let 
                -- Split off the type arguments.
                (asArgs, tsArgs) = unzip $ [(a', t) | XType a' t <- xsArgsAll]
                
                -- For each type argument, if we know how to create the unboxed version
                -- then do so. If this is wrong then the type checker will catch it later.
                getTypeUnboxed t
                 | Just t'      <- configConvertRepType config RepUnboxed t  
                                = t'                
                 | otherwise    = t 

                tsArgsUnboxed   = map getTypeUnboxed tsArgs
                
                -- Instantiate the type to work out which arguments need to be unboxed,
                -- and which we can leave as-is. 
                Just tPrimInstUnboxed   = instantiateTs tPrim tsArgsUnboxed
                (tsArgsInstUnboxed, tResultInstUnboxed)
                                        = takeTFunArgResult tPrimInstUnboxed

                -- Unboxing arguments to the function.
                xsArgs  = drop (length tsArgs) xsArgsAll

            in if -- We must end up with a type of each argument.
                  -- If not then the primop is partially applied or something else is wrong.
                  -- The Tetra to Salt conversion will give a proper error message
                  -- if the primop is indeed partially applied.
                  not (length xsArgs == length tsArgsInstUnboxed)
                   then XApp a (down x1) (down x2)

                   -- We got a type for each argument, so the primop is fully applied
                   -- and we can do the boxing/unboxing transform.
                   else let xsArgs' 
                             = [ unboxExp config a tArgInst (down xArg)
                                  | xArg      <- xsArgs
                                  | tArgInst  <- tsArgsInstUnboxed ]
                        in  boxExp config a tResultInstUnboxed
                                $ xApps a xFn  (  [XType a' t  | t  <- tsArgsUnboxed
                                                        | a' <- asArgs]
                                                ++ xsArgs')

        -- Boilerplate
        XVar{}          -> xx
        XCon{}          -> xx
        XLAM a b x      -> XLAM  a b (down x)
        XLam a b x      -> XLam  a b (down x)
        XApp a x1 x2    -> XApp  a   (down x1)  (down x2)
        XLet a lts x    -> XLet  a   (down lts) (down x)
        XCase a x alts  -> XCase a   (down x)   (map down alts)
        XCast a c x     -> XCast a c (down x)
        XType a t       -> XType a t 
        XWitness{}      -> xx


-- | Box an expression that produces a value.
boxExp :: Config a n -> a -> Type n -> Exp a n -> Exp a n
boxExp config a t xx
        | Just RepValue <- configRepOfType    config t
        , Just x'       <- configValueOfBoxed config a xx t
        = x'

        | Just RepUnboxed <- configRepOfType config t
        , Just tIdx     <- configConvertRepType config RepValue t
        , Just x'       <- configBoxedOfUnboxed config a xx tIdx
        = x'

        | otherwise
        = xx


-- | Unbox an expression that produces a boxed value.
unboxExp :: Config a n -> a -> Type n -> Exp a n -> Exp a n
unboxExp config a t xx
        | Just RepValue <- configRepOfType config t
        , Just x'       <- configUnboxedOfBoxed     config a xx t
        = x'

        | Just RepBoxed <- configRepOfType      config t
        , Just tIdx     <- configConvertRepType config RepValue t
        , Just x'       <- configUnboxedOfBoxed config a xx tIdx
        = x'

        | otherwise
        = xx


-- | If this is an application of some primitive operator or foreign function that 
--   works on unboxed values then split it into the function and arguments.
--
--   The arguments returned include type arguments as well.
splitUnboxedOpApp
        :: Config a n
        -> Exp a n 
        -> Maybe (Exp a n, Type n, [Exp a n])

splitUnboxedOpApp config xx
 = case xx of
        XApp{}
         | Just (n, xsArgsAll)  <- takeXPrimApps xx
         , Just (xFn, _)        <- takeXApps     xx
         , configNameIsUnboxedOp config n
         , Just tPrim           <- configValueTypeOfPrimOpName config n
         -> Just (xFn, tPrim, xsArgsAll)

        XApp{}
         | Just (xFn@(XVar _ (UName n)), xsArgsAll)
                                <- takeXApps xx
         , Just tForeign        <- configValueTypeOfForeignName config n
         -> Just (xFn, tForeign, xsArgsAll)

        _ -> Nothing


-- Lets -------------------------------------------------------------------------------------------
instance Boxing Lets where
 boxing config lts
  = let down    = boxing config
    in case lts of
        LLet b x        -> LLet b (down x)
        LRec bxs        -> LRec [(b, down x) | (b, x) <- bxs]
        LPrivate{}      -> lts
        LWithRegion{}   -> lts


-- Alt --------------------------------------------------------------------------------------------
instance Boxing Alt where
 boxing config alt
  = case alt of
        AAlt PDefault x 
         -> AAlt PDefault (boxing config x)

        AAlt (PData dc bs) x
         -> AAlt (PData dc bs) (boxing config x)

