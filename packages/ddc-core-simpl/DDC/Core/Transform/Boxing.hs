-- | Manage representation of numeric values in a module.
--
--   We use three seprate versions of each numeric type.
--      Nat#            Numeric index type.
--      B# Nat#         Boxed representation type.
--      U# Nat#         Unboxed representation type.
--
--   A numeric index type is the type of pure values like 23#, where "pure value"
--   means the mathematical value, free from any considerations about how that 
--   might be represented at runtime in the physical machine.
--
--   The Boxed and Unboxed representation types commit to a specific runtime
--   representation, and have implications for runtime performance and space 
--   usage of the compiled program.
--
--   The boxing transform takes an input program using just pure values and
--   numeric index types, and refines it to a program that commits to particular
--   representations of those values. In particular, we commit to a particular
--   representation for function arguments and results, which makes the program
--   adhere to a function calling convention that follow-on transformations
--   to lower level languages (like Core Salt) can deal with.
--
--   This Boxing transform should do  just enough to make the code well-formed
--   with respect to runtime representation. Demand-driven optimisations like
--   local unboxing should be done in follow-on transformations.
--
--   We make the following representation commitments, so that the default
--   representation is boxed.
--
--   Literal values are wrapped into their boxed representation:
--        23# 
--     => convert# [B# Nat#] [Nat#] 23#
--
--   Use unboxed versions of primitive operators:
--        add# [Nat#] x y 
--     => convert# [B# Nat#] [U# Nat#] 
--                 (add# [U# Nat#] (convert# [U# Nat#] [B# Nat#] x)
--                                 (convert# [U# Nat#] [B# Nat#] y))
--
--   Case scrutinees are unwrapped when matching against literal patterns:
--        case x of { 0# -> ... }
--     => case convert [B# Nat#] [Nat#] x of { 0# -> ... }
--
--   After performing this transformation the program is said
--   to "use representational types", or be in "representational form".
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
import DDC.Type.DataDef
import Data.Maybe
import Control.Monad


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
        { -- | Values of this type needs boxing to make the program
          --   representational. This will only be passed types of kind Data.
          configTypeNeedsBoxing :: Type n -> Bool

          -- | Get the boxed version of some data type, if any.
          --   This will only be passed types where typeNeedsBoxing returns True.
        , configTakeTypeBoxed   :: Type n -> Maybe (Type n) 

          -- | Get the unboxed version of some data type, if any.
          --   This will only be passed types where typeNeedsBoxing returns True.
        , configTakeTypeUnboxed :: Type n -> Maybe (Type n) 

          -- | Check if the primop with this name works on unboxed values
          --   directly. Operators where this function returns False are assumed
          --   to take boxed values for every argument.
        , configNameIsUnboxedOp :: n -> Bool 

          -- | Take the type of a literal name, if there is one.
        , configTypeOfLitName   :: n -> Maybe (Type n)

          -- | Take the type of a primitive operator name, if it is one.
          --   The primops can be polytypic, but must have prenex rank-1 types.
        , configTypeOfPrimOpName :: n -> Maybe (Type n) 

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


class Boxing (c :: * -> * -> *) where
 -- | Rewrite a module to use explitit boxed and unboxed types.
 boxing :: Ord n
        => Config a n
        -> c a n
        -> c a n


-- TODO: also apply to imported and exported type sigs.
instance Boxing Module where
 boxing config mm
  = mm  { moduleBody            = boxing config (moduleBody mm) 
        , moduleDataDefsLocal   = map (boxingDataDef config) (moduleDataDefsLocal mm) }


instance Boxing Exp where
 boxing config xx
  = let down = boxing config
    in case xx of

        -- Convert literals to their boxed representations.
        XCon a dc
         |  Just dcn    <- takeNameOfDaCon dc
         ,  Just tLit   <- configTypeOfLitName config dcn
         ,  configTypeNeedsBoxing config tLit
         ,  Just xx'    <- configBoxedOfValue  config a xx tLit
         -> xx'

        -- When applying a primop that works on unboxed values, 
        -- unbox its arguments and then rebox the result.
        XApp a _ _
         -- Split the application of a primop into its name and arguments.
         -- The arguments here include type arguments as well.
         | Just (n, xsArgsAll)  <- takeXPrimApps xx
         , Just (xFn, _)        <- takeXApps     xx
         , configNameIsUnboxedOp config n

           -- Instantiate the type of the primop to work out which arguments
           -- need to be unboxed, and which we can leave as-is.
         , Just tPrim           <- configTypeOfPrimOpName config n
         , (asArgs, tsArgs)     <- unzip $ [(a', t) | XType a' t <- xsArgsAll]
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
                 = [ if configTypeNeedsBoxing config tArg 
                        then fromMaybe (down xArg)
                                       (configUnboxedOfBoxed config a (down xArg) tArg)
                        else xArg
                        | xArg <- xsArgs
                        | tArg <- tsArgsInst ]

                -- Rebox the result as nessesary.
                fResult x
                 = if configTypeNeedsBoxing config tResultInst
                        then fromMaybe x
                                       (configBoxedOfUnboxed  config a x tResultInst)
                        else x
            in  fResult 
                 $ xApps a xFn  (  [XType a' t  | t  <- tsArgsUnboxed
                                                | a' <- asArgs]
                                ++ xsArgs')

        -- Unrap scrutinees when matching against literal patterns.
        XCase a xScrut alts
         | p : _        <- [ p  | AAlt (PData p@DaConPrim{} []) _ <- alts]
         , Just tLit    <- configTypeOfLitName config (daConName p)
         , configTypeNeedsBoxing config tLit
         , Just xScrut' <- configValueOfBoxed  config a (down xScrut) tLit
         -> XCase a xScrut' (map down alts)

        -- Boilerplate
        XVar{}          -> xx
        XCon{}          -> xx
        XLAM a b x      -> XLAM  a b (down x)
        XLam a b x      -> XLam  a (boxingB config b) (down x)
        XApp a x1 x2    -> XApp  a (down x1)  (down x2)
        XLet a lts x    -> XLet  a (down lts) (down x)
        XCase a x alts  -> XCase a (down x)   (map down alts)
        XCast a c x     -> XCast a c (down x)
        XType a t       -> XType a (boxingT config t)
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
  | configTypeNeedsBoxing config tt
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


-- | Manage boxing in a data type definition.
boxingDataDef :: Config a n -> DataDef n -> DataDef n
boxingDataDef config def@DataDef{}
 = def { dataDefCtors = liftM (map (boxingDataCtor config)) (dataDefCtors def) }


-- | Manage boxing in a data constructor definition.
boxingDataCtor :: Config a n -> DataCtor n -> DataCtor n    
boxingDataCtor config ctor@DataCtor{}
 = ctor 
 { dataCtorFieldTypes   = map (boxingT config) (dataCtorFieldTypes ctor)
 , dataCtorResultType   = boxingT config (dataCtorResultType ctor) }
