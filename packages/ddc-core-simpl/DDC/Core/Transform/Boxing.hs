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
import DDC.Type.DataDef
import Control.Monad


---------------------------------------------------------------------------------------------------
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
        { -- | Values of this type need boxing to make the program
          --   representational. This will only be passed types of kind Data.
          configIsValueType             :: Type n -> Bool

          -- | Check if this is a  boxed representation type.
        , configIsBoxedType             :: Type n -> Bool

          -- | Check if this is an unboxed representation type.
        , configIsUnboxedType           :: Type n -> Bool

          -- | Get the boxed version of some data type, if any.
          --   This will only be passed types where typeNeedsBoxing returns True.
        , configBoxedOfValueType        :: Type n -> Maybe (Type n) 

          -- | Get the unboxed version of some data type, if any.
          --   This will only be passed types where typeNeedsBoxing returns True.
        , configUnboxedOfValueType      :: Type n -> Maybe (Type n) 

          -- | Take the index type from a boxed type, if it is one.
        , configValueTypeOfBoxed        :: Type n -> Maybe (Type n)

          -- | Take the index type from an unboxed type, if it is one.
        , configValueTypeOfUnboxed      :: Type n -> Maybe (Type n)

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
  = let 
        -- Handle boxing in the types of exported values.
        exportValues'
         = map (boxingExportValue config) $ moduleExportValues mm
         
        -- Handle boxing in the types of imported values.
        importValues'
         = map (boxingImportValue config) $ moduleImportValues mm

        -- Add locally imported foreign functions to the foreign function detector.
        --  We want the original type here, before it has been passed through
        --  the boxing transform.
        typeOfForeignName n
         -- The provided config already says this is foreign.
         | Just t       <- configValueTypeOfForeignName config n  
         = Just t

         -- This is a locally imported C function.
         | Just (ImportSourceSea _ t)
                        <- lookup n (moduleImportValues mm)  
         = Just t

         | otherwise
         = Nothing

        -- Use our new foreign function detector in the config.
        config'
         = config
         { configValueTypeOfForeignName  = typeOfForeignName }

        -- Do the boxing transform.
    in  mm  { moduleBody            = boxing config' (moduleBody mm) 
            , moduleExportValues    = exportValues'
            , moduleImportValues    = importValues'
            , moduleDataDefsLocal   = map (boxingDataDef config') (moduleDataDefsLocal mm) }


-- | Manage boxing in the type of an exported value.
boxingExportValue
        :: Config a n
        -> (n, ExportSource n)
        -> (n, ExportSource n)

boxingExportValue config (n, esrc)
 = case esrc of
        ExportSourceLocal n' t
         -> (n, ExportSourceLocal n' (boxingT config t))

        ExportSourceLocalNoType{}
         -> (n, esrc)


-- | Manage boxing in the type of an imported value.
boxingImportValue 
        :: Config a n
        -> (n, ImportSource n)
        -> (n, ImportSource n)

boxingImportValue config (n, isrc)
 = case isrc of
        -- This shouldn't happen for values, but just pass it through.
        ImportSourceAbstract _
         -> (n, isrc)

        -- Function imported from a DDC compiled module.
        ImportSourceModule mn n' t
         -> (n, ImportSourceModule mn n' (boxingT config t))

        -- Value imported using the standard C calling convention.
        ImportSourceSea str t
         -> (n, ImportSourceSea str (boxingSeaT config t))


-- Exp --------------------------------------------------------------------------------------------
instance Boxing Exp where
 boxing config xx
  = let down = boxing config
    in case xx of

        -- Convert literals to their boxed representations.
        XCon a dc
         |  Just dcn    <- takeNameOfDaCon dc
         ,  Just tLit   <- configValueTypeOfLitName config dcn
         ,  configIsValueType config tLit
         ,  Just xx'    <- configBoxedOfValue  config a xx tLit
         -> xx'

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
                 | Just t'      <- configUnboxedOfValueType config t  
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

        -- Unrap scrutinees when matching against literal patterns.
        XCase a xScrut alts
         | p : _        <- [ p  | AAlt (PData p@DaConPrim{} []) _ <- alts]
         , Just tLit    <- configValueTypeOfLitName config (daConName p)
         , configIsValueType config tLit
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


-- | Box an expression that produces a value.
boxExp :: Config a n -> a -> Type n -> Exp a n -> Exp a n
boxExp config a t xx
        | configIsValueType config t
        , Just x'      <- configBoxedOfUnboxed config a xx t
        = x'

        | configIsUnboxedType config t
        , Just tIdx    <- configValueTypeOfUnboxed config t
        , Just x'      <- configBoxedOfUnboxed config a xx tIdx
        = x'

        | otherwise
        = xx


-- | Unbox an expression that produces a boxed value.
unboxExp :: Config a n -> a -> Type n -> Exp a n -> Exp a n
unboxExp config a t xx
        | configIsValueType config t
        , Just x'      <- configUnboxedOfBoxed     config a xx t
        = x'

        | configIsUnboxedType config t
        , Just tIdx    <- configValueTypeOfUnboxed config t
        , Just x'      <- configUnboxedOfBoxed     config a xx tIdx
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


-- Alt --------------------------------------------------------------------------------------------
instance Boxing Alt where
 boxing config alt
  = case alt of
        AAlt PDefault x 
         -> AAlt PDefault (boxing config x)

        AAlt (PData dc bs) x
         -> AAlt (PData dc (map (boxingB config) bs)) (boxing config x)


---------------------------------------------------------------------------------------------------
-- | Manage boxing in a Bind.
boxingB :: Config a n -> Bind n -> Bind n
boxingB config bb
 = case bb of
        BAnon t         -> BAnon   (boxingT config t)
        BName n t       -> BName n (boxingT config t)
        BNone t         -> BNone   (boxingT config t)


-- | Manage boxing in a Type.
boxingT :: Config a n -> Type n -> Type n
boxingT config tt
  | configIsValueType config tt
  , Just tResult        <- configBoxedOfValueType config tt
  = tResult

  | otherwise
  = let down = boxingT config
    in case tt of
        TVar{}          -> tt
        TCon{}          -> tt
        TForall b t     -> TForall b (down t)
        TApp t1 t2      -> TApp (down t1) (down t2)
        TSum{}          -> tt


-- | Manage boxing in the type of a C value.
boxingSeaT :: Config a n -> Type n -> Type n
boxingSeaT config tt
  | configIsValueType config tt
  , Just tResult        <- configUnboxedOfValueType config tt
  = tResult

  | otherwise
  = let down = boxingSeaT config
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
