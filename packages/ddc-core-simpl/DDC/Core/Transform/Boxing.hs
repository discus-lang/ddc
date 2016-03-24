-- | Manage representation of numeric values in a module.
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
        , boxingModule)
where
import DDC.Core.Compounds
import DDC.Core.Module
import DDC.Core.Exp
import DDC.Core.Pretty
import DDC.Type.Transform.Instantiate
import Data.Maybe


---------------------------------------------------------------------------------------------------
-- | Representation of the values of some type.
data Rep
        -- | These types don't contain any values.
        = RepNone

        -- | Values of this type are uncomitted to a particular representation,
        --   they just describe a set of logical values.
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

          -- | Convert a value between representations.
        , configConvertRepExp           :: Rep -> a -> Type n -> Exp a n -> Maybe (Exp a n) 

          -- | Take the type of a literal name, if there is one.
        , configValueTypeOfLitName      :: n -> Maybe (Type n)

          -- | Take the type of a primitive operator name, if it is one.
          --   The primops can be polytypic, but must have prenex rank-1 types.
        , configValueTypeOfPrimOpName   :: n -> Maybe (Type n) 

          -- | Take the type of a foreign function name, if it is one.
          --   The function can be polymorphic, but must have a prenex rank-1 type.
        , configValueTypeOfForeignName  :: n -> Maybe (Type n)

          -- | Convert a literal name to its unboxed version.
        , configUnboxLitName            :: n -> Maybe n

          -- | Covnert a primop name to its unboxed version.
        , configUnboxPrimOpName         :: n -> Maybe n
        }


-- Module -----------------------------------------------------------------------------------------
-- TODO: throw real errors instead of just returning original, 
--       want this when handling foreign functions anyway.
-- TODO: only do boxing for foreign functions, not foreign values.
boxingModule 
        :: (Show a, Show n, Pretty n, Ord n) 
        => Config a n -> Module a n -> Module a n

boxingModule config mm
 = let  
        -- Use explicitly unboxed types when importing foreign sea functions.
        boxingImport imp
         = case imp of
                ImportValueSea v t
                  -> ImportValueSea v  $ boxingForeignSeaType config t
                _ -> imp

        -- Use explicitly unboxed types when exporting foreign sea functions.
        nsImportSea   = [ n | (n, ImportValueSea _ _) <- moduleImportValues mm]
        boxingExport expt
         = case expt of
                ExportSourceLocal n t
                  |  elem n nsImportSea
                  -> ExportSourceLocal n $ boxingForeignSeaType config t
                _ -> expt

   in   mm { moduleBody         
              = boxingX config (moduleBody mm)

           , moduleExportValues 
              = [(n, boxingExport expt) | (n, expt) <- moduleExportValues mm ]

           , moduleImportValues 
              = [(n, boxingImport impt) | (n, impt) <- moduleImportValues mm ] }


boxingX config xx
 = case xx of

        -- Convert literals to their unboxed form, followed by a boxing conversion.
        XCon a (DaConPrim n tLit)
         | Just RepBoxed        <- configRepOfType config tLit
         -> let Just tLitU      = configConvertRepType config RepUnboxed tLit
                Just nU         = configUnboxLitName   config n

                Just xLit       = configConvertRepExp  config RepBoxed a tLitU 
                                $ XCon a (DaConPrim nU tLitU)
           in   xLit

        -- Use unboxed versions of primops by unboxing their arguments then 
        -- reboxing their results.
        XCast _ CastRun xx'@(XApp a _ _)
         |  Just (n, xsArgsAll) <- takeXPrimApps xx'
         ,  Just n'             <- configUnboxPrimOpName config n
         -> let Just tPrimBoxed    = configValueTypeOfPrimOpName config n
                Just tPrimUnboxed  = configValueTypeOfPrimOpName config n'
                xsArgsAll'         = map (boxingX config) xsArgsAll
            in  boxingPrimitive config a True xx' (XVar a (UPrim n' tPrimUnboxed)) 
                        tPrimBoxed tPrimUnboxed
                        xsArgsAll'

        XApp a _ _
         |  Just (n, xsArgsAll) <- takeXPrimApps xx
         ,  Just n'             <- configUnboxPrimOpName config n
         -> let Just tPrimBoxed    = configValueTypeOfPrimOpName config n
                Just tPrimUnboxed  = configValueTypeOfPrimOpName config n'
                xsArgsAll'         = map (boxingX config) xsArgsAll
            in  boxingPrimitive config a False xx (XVar a (UPrim n' tPrimUnboxed))
                        tPrimBoxed tPrimUnboxed
                        xsArgsAll'

        -- Foreign calls
        XApp a _ _
         | Just (xFn@(XVar _ (UName n)), xsArgsAll)
                                <- takeXApps xx
         , Just tForeign        <- configValueTypeOfForeignName config n
         -> let xsArgsAll'      = map (boxingX config) xsArgsAll
            in  boxingForeignSea config a xx xFn tForeign xsArgsAll'


        XCase a xScrut alts
         | p : _         <- [ p  | AAlt (PData p@DaConPrim{} []) _ <- alts]
         , Just tLit1    <- configValueTypeOfLitName config (daConName p)
         , Just RepBoxed <- configRepOfType config tLit1
         -> let alts'    = map (boxingAlt config) alts
            in  boxingCase config a tLit1 xScrut alts'

        -- Boilerplate.
        XVar{}          -> xx
        XCon{}          -> xx
        XLAM a b x      -> XLAM a b  (boxingX   config x)
        XLam a b x      -> XLam a b  (boxingX   config x)
        XApp a x1 x2    -> XApp a    (boxingX   config x1)  (boxingX config x2)
        XLet a lts x    -> XLet a    (boxingLts config lts) (boxingX config x)
        XCase a x alts  -> XCase a   (boxingX   config x)   (map (boxingAlt config) alts)
        XCast a c x     -> XCast a c (boxingX   config x)
        XType{}         -> xx
        XWitness{}      -> xx

boxingLts config lts
 = case lts of
        LLet b x        -> LLet b (boxingX config x)
        LRec bxs        -> LRec [(b, boxingX config x) | (b, x) <- bxs]
        LPrivate{}      -> lts

boxingAlt config alt
 = case alt of
        AAlt p x        -> AAlt p (boxingX config x)


---------------------------------------------------------------------------------------------------
-- | Marshall arguments and return values of primitive operations.
--   If something goes wrong then just return the original expression and leave it to
--   follow on transforms to report the error. The code generator won't be able to
--   convert the original expression.
--
--   TODO: Assumes that the type of the primitive is prenex.

boxingPrimitive
        :: (Ord n, Pretty n, Show a, Show n)
        => Config a n -> a
        -> Bool         -- ^ Primitive is being run at the call site.
        -> Exp a n      -- ^ Whole primitive application, for debugging.
        -> Exp a n      -- ^ Functional expression.
        -> Type n       -- ^ Type of the boxed version of the primitive.
        -> Type n       -- ^ Type of the unboxed version of the primitive.
        -> [Exp a n]    -- ^ Arguments to the primitive.
        -> Exp a n

boxingPrimitive config a bRun xx xFn tPrimBoxed tPrimUnboxed xsArgsAll
 = fromMaybe xx go
 where
  go = do  
        -- Split off the type args.
        let (asArgs, tsArgs) = unzip [(a', t) | XType a' t <- xsArgsAll]
        let xsArgs      = drop (length tsArgs) xsArgsAll

        -- Get the boxed version of the types of parameters and return value.
        tPrimBoxedInst   <- instantiateTs tPrimBoxed tsArgs
        let (tsParamBoxed, _tResultBoxed) 
                        = takeTFunArgResult tPrimBoxedInst

        -- Get the unboxed version of the types of parameters and return value.
        tPrimUnboxedInst <- instantiateTs tPrimUnboxed tsArgs
        let (tsParamUnboxed, tResultUnboxed)
                        = takeTFunArgResult tPrimUnboxedInst

        -- If the primitive is being run at the call site then we need to 
        -- re-box the result AFTER it has been run, not before.
        let tResultUnboxed'
                | not bRun      = tResultUnboxed
                | otherwise     = case takeTSusp tResultUnboxed of
                                        Just (_, t)     -> t
                                        Nothing         -> tResultUnboxed

        -- We must end up with a type of each argument.
        -- If not then the primop is partially applied or something else is wrong.
        -- The Tetra to Salt conversion will give a proper error message
        -- if the primop is indeed partially applied.
        (if not (  length xsArgs == length tsParamBoxed
                && length xsArgs == length tsParamUnboxed)
           then Nothing
           else Just ())

        -- We got a type for each argument, so the primop is fully applied
        -- and we can do the boxing/unboxing transform.
        let xsArgs' = [ (let t = fromMaybe xArg
                               $ configConvertRepExp config RepUnboxed a tArgInst xArg 
                                 in t)
                      | xArg      <- xsArgs
                      | tArgInst  <- tsParamBoxed ]

        -- Construct the result expression, running it if necessary.
        let xtsArgsU            = [ XType a' t | t <- tsArgs | a' <- asArgs ]
        let xResultU            = xApps a xFn (xtsArgsU ++ xsArgs')
        let xResultRunU
                | not bRun      = xResultU
                | otherwise     = XCast a CastRun xResultU

        let xResultV =  fromMaybe xResultRunU
                     $  configConvertRepExp config RepBoxed a tResultUnboxed' xResultRunU

        return xResultV


---------------------------------------------------------------------------------------------------
-- Marshall arguments and return values of foreign imported functions.
-- 
--  TODO: assumes that the type of the import is prenex.
--
boxingForeignSea
        :: (Ord n, Pretty n)
        => Config a n -> a 
        -> Exp a n      -- ^ Whole function application, for debugging.
        -> Exp a n      -- ^ Functional expression.
        -> Type n       -- ^ Type of the foreign function.
        -> [Exp a n]    -- ^ Arguments to the foreign function.
        -> Exp a n

boxingForeignSea config a xx xFn tF xsArg
 = fromMaybe xx go
 where go = do
        -- Split off the type args.
        let (_asArg, tsArgType) = unzip [(a', t) | XType a' t <- xsArg]
        let xsArgVal    = drop (length tsArgType) xsArg

        -- Get the argument and return types of the function.
        -- Unlike primitives, foreign functions are not polytypic, so we can
        -- just erase any outer foralls to reveal the types of the args.
        let (tsArgVal, tResult) 
                        = takeTFunArgResult
                        $ eraseTForalls tF

        -- We must end up with a type for each argument.
        -- TODO: throw an error if this doesn't happen, foreign function is
        --       either under or over applied. This should have been fixed by
        --       eta-expansion, in the curry transform.
        (if not (length xsArgVal == length tsArgVal)
           then Nothing
           else Just ())

        -- For each argument, if it has an unboxed representation then unbox it.
        let unboxArg xArg tArg 
             = fromMaybe xArg
             $ configConvertRepExp config RepUnboxed a tArg xArg

        let xsArgValU = zipWith unboxArg xsArgVal tsArgVal
        let xExpU     = xApps a xFn ([XType a t | t <- tsArgType] ++ xsArgValU)

        -- If the result has a boxed representation then box it.
        let boxResult tRes xRes
             = fromMaybe xRes
             $ do tResU      <- configConvertRepType config RepUnboxed tRes
                  configConvertRepExp config RepBoxed a tResU xExpU

        return $ boxResult tResult xExpU


-- | 
boxingForeignSeaType
        :: Config a n -> Type n -> Type n

boxingForeignSeaType config tForeign
 = let  
        -- Split the type into quantifiers, parameter and result types.
        (bsForall, tBody) 
                 = fromMaybe ([], tForeign)
                 $ takeTForalls tForeign

        (tsParam, tResult) 
                 = takeTFunArgResult tBody

        -- If there is an unboxed representation of each parameter and result
        -- type, then use that.
        unboxType tThing
                 = fromMaybe tThing
                 $ configConvertRepType config RepUnboxed tThing

        tsParamU    = map unboxType tsParam
        tResultU    = unboxType tResult

        -- Build the converted type back out of its parts.
        Just tBodyU = tFunOfList (tsParamU ++ [tResultU])
        tForeignU   = foldr TForall tBodyU bsForall

   in   tForeignU


---------------------------------------------------------------------------------------------------
-- For case expressions that match against literals, like
--
--   case e1 of 
--   { 5# -> e2; _ -> e3 }
--
-- Unbox the scrutinee and convert the alternatives to match against
-- unboxed literals.
-- 
--   case convert# [Nat] [Nat#] e1 of
--   { 5## -> e2; _ -> e3 }
--
boxingCase 
        :: Config a n
        -> a -> Type n
        -> Exp a n
        -> [Alt a n]
        -> Exp a n

boxingCase config a tLit1 xScrut alts
 = let
        unboxAlt (AAlt (PData (DaConPrim n tLit) []) x)
         | Just RepBoxed <- configRepOfType config tLit
         , Just nU       <- configUnboxLitName config n
         , Just tLitU    <- configConvertRepType config RepUnboxed tLit
         = Just (AAlt (PData (DaConPrim nU tLitU) []) x)

        unboxAlt alt@(AAlt PDefault _) = Just alt
        unboxAlt _                     = Nothing

        Just alts_unboxed
         = sequence $ map unboxAlt alts

        Just xScrut'    = configConvertRepExp config RepUnboxed a tLit1 xScrut
        alts_default    = ensureDefault alts_unboxed

  in    XCase a xScrut' $ alts_default


-- | Ensure that there is a default alternative in this list, 
--   if not then make the last one the default.
--   We need do this to handle the case when the unboxed type does not have
--   all its constructors listed in the data defs. If it doesn't then the 
--   case exhaustiveness checker will compilain when checking the result code.
ensureDefault :: [Alt a n] -> [Alt a n]
ensureDefault alts
        | _ : _ <- [alt | alt@(AAlt PDefault _) <- alts]
        = alts

        | AAlt (PData _ []) x : rest <- reverse alts
        = reverse rest ++ [AAlt PDefault x]

        | otherwise
        = alts
