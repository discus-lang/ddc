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
        , boxingModule)
where
import DDC.Core.Transform.TransformDownX
import DDC.Core.Compounds
import DDC.Core.Module
import DDC.Core.Exp
import DDC.Type.Transform.Instantiate
import Data.Maybe


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

          -- | Check if the primop with this name works on unboxed values
          --   directly. Operators where this function returns False are assumed
          --   to take boxed values for every argument.
        , configNameIsUnboxedOp         :: n -> Bool 

          -- | Convert a literal name to its unboxed version.
        , configUnboxLitName            :: n -> Maybe n
        }


-- Module -----------------------------------------------------------------------------------------
boxingModule :: Ord n => Config a n -> Module a n -> Module a n
boxingModule config mm
  = mm  { moduleBody = transformDownX' (boxingX config) (moduleBody mm) }  

boxingX config xx
 = let down = boxingX config
   in case xx of

        -- When applying a primop that works on unboxed values, 
        -- unbox its arguments and then rebox the result.
        XApp a _ _
         -- Split the application of a primop into its name and arguments.
         -- The arguments here include type arguments as well.
         | Just (xFn, tPrim, xsArgsAll) <- splitUnboxedOpApp config xx

         -- Split off the type args.
         , (asArgs, tsArgs) <- unzip [(a', t) | XType a' t <- xsArgsAll]
         , xsArgs           <- drop (length tsArgs) xsArgsAll

         -- Get the original types of the args and return value.
         , Just tPrimInst   <- instantiateTs tPrim tsArgs
         , (tsArgsInst, _tResultInst)   <- takeTFunArgResult tPrimInst

         -- Get the unboxed version of the args and return value
         , Just tsArgsU     <- sequence 
                            $  map (configConvertRepType config RepUnboxed) tsArgs
         , Just tPrimInstU  <- instantiateTs tPrim tsArgsU
         , (_tsArgsInstU, tResultInstU) <- takeTFunArgResult tPrimInstU

         -- We must end up with a type of each argument.
         -- If not then the primop is partially applied or something else is wrong.
         -- The Tetra to Salt conversion will give a proper error message
         -- if the primop is indeed partially applied.
         , length xsArgs == length tsArgsInst

         -- We got a type for each argument, so the primop is fully applied
         -- and we can do the boxing/unboxing transform.
         -> let xsArgs'  = [ (let Just t = configConvertRepExp config RepUnboxed
                                                       a tArgInst (down xArg) 
                              in t)
                           | xArg      <- xsArgs
                           | tArgInst  <- tsArgsInst ]

                xtsArgsU = [ XType a' t | t <- tsArgsU | a' <- asArgs ]
                xResultU = xApps a xFn (xtsArgsU ++ xsArgs')
                xResultV = fromMaybe xx
                                 (configConvertRepExp config RepValue a tResultInstU xResultU)
            in  xResultV


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
        XCase a xScrut alts
         | p : _         <- [ p  | AAlt (PData p@DaConPrim{} []) _ <- alts]
         , Just tLit1    <- configValueTypeOfLitName config (daConName p)
         , Just RepValue <- configRepOfType config tLit1

         , Just alts_unboxed
            <- sequence 
            $  map (\alt -> case alt of
                             AAlt (PData (DaConPrim n tLit) []) x
                              | Just RepValue <- configRepOfType config tLit
                              , Just nU       <- configUnboxLitName config n
                              , Just tLitU    <- configConvertRepType config RepUnboxed tLit
                              -> Just (AAlt (PData (DaConPrim nU tLitU) []) x)

                             AAlt PDefault _
                              -> Just alt

                             _ -> Nothing)
            $ alts

         , Just xScrut' <- configConvertRepExp config RepUnboxed a tLit1 xScrut
         , alts_default <- ensureDefault alts_unboxed
         -> XCase a xScrut' $ alts_default


        -- Convert literals to their unboxed form, followed by a boxing conversion.
        XCon a (DaConPrim n tLit)
         | Just RepValue <- configRepOfType config tLit
         , Just tLitU    <- configConvertRepType config RepUnboxed tLit
         , Just nU       <- configUnboxLitName   config n
         , Just xLit     <- configConvertRepExp  config RepValue a tLitU 
                         $  XVar a (UPrim nU tLitU)
         -> xLit

        _ -> xx


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
