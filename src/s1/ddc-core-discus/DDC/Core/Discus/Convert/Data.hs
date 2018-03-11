
module DDC.Core.Discus.Convert.Data
        ( constructData
        , destructData)
where
import DDC.Core.Discus.Convert.Error
import DDC.Core.Discus.Convert.Layout
import DDC.Core.Salt.Platform
import DDC.Core.Transform.BoundX
import DDC.Core.Exp
import DDC.Type.Exp.Simple
import DDC.Type.DataDef
import qualified DDC.Core.Discus.Prim            as E
import qualified DDC.Core.Salt.Runtime          as A
import qualified DDC.Core.Salt.Name             as A
import qualified DDC.Core.Salt.Compounds        as A
import Data.Maybe


-- Construct ------------------------------------------------------------------
-- | Build an expression that allocates and initialises a data object.
constructData
        :: Show a
        => Platform                     -- ^ Platform definition.
        -> a                            -- ^ Annotation to use on expressions.
        -> DataCtor E.Name              -- ^ Constructor definition of object.
        -> Type     A.Name              -- ^ Prime region variable.
        -> [Exp a   A.Name]             -- ^ Field values.
        -> [Type    A.Name]             -- ^ Field types.
        -> ConvertM a (Exp a A.Name)

constructData pp a ctorDef rPrime xsFields tsFields
 | Just HeapObjectBoxed <- heapObjectOfDataCtor pp ctorDef
 = do
        -- Allocate the object.
        let arity       = length tsFields
        let bObject     = BAnon (A.tPtr rPrime A.tObj)
        let xAlloc      = A.xAllocBoxed a rPrime (dataCtorTag ctorDef)
                        $ A.xNat a (fromIntegral arity)

        -- Statements to write each of the fields.
        let xObject'    = XVar a $ UIx 0
        let lsFields
                = [ LLet (BNone A.tVoid)
                         (A.xSetFieldOfBoxed a
                         rPrime trField xObject' ix (liftX 1 xField))
                  | ix          <- [0..]
                  | xField      <- xsFields
                  | trField     <- repeat A.rTop ]

        return  $ XLet a (LLet bObject xAlloc)
                $ foldr (XLet a) xObject' lsFields


 | Just HeapObjectSmall <- heapObjectOfDataCtor  pp ctorDef
 , Just size            <- payloadSizeOfDataCtor pp ctorDef
 = do
        -- Allocate the object.
        let bObject     = BAnon (A.tPtr rPrime A.tObj)
        let xAlloc      = A.xAllocSmall a rPrime (dataCtorTag ctorDef)
                        $ A.xNat a size

        -- Take a pointer to its payload.
        let bPayload    = BAnon (A.tPtr rPrime (A.tWord 8))
        let xPayload    = A.xPayloadOfSmall a rPrime
                        $ XVar a (UIx 0)

        -- Get the offset of each field.
        let Just offsets = fieldOffsetsOfDataCtor pp ctorDef

        -- Statements to write each of the fields.
        let xObject'    = XVar a $ UIx 1
        let xPayload'   = XVar a $ UIx 0
        let lsFields    = [ LLet (BNone A.tVoid)
                                 (A.xPoke a rPrime tField
                                        (A.xPlusPtr a A.rTop tField
                                                (A.xCastPtr a A.rTop tField (A.tWord 8) xPayload')
                                                (A.xNat a offset))
                                        (liftX 2 xField))
                                | tField        <- tsFields
                                | offset        <- offsets
                                | xField        <- xsFields]

        return  $ XLet a (LLet bObject  xAlloc)
                $ XLet a (LLet bPayload xPayload)
                $ foldr (XLet a) xObject' lsFields

 | otherwise
 = error $ unlines
        [ "constructData: don't know how to construct a "
                ++ (show $ dataCtorName ctorDef)
        , "  heapObject = " ++ (show $ heapObjectOfDataCtor  pp ctorDef)
        , "  fields     = " ++ (show $ dataCtorFieldTypes ctorDef)
        , "  size       = " ++ (show $ payloadSizeOfDataCtor pp ctorDef) ]


-- Destruct -------------------------------------------------------------------
-- | Wrap a expression in let-bindings that bind each of the fields of
--   of a data object. This is used when pattern matching in a case expression.
--
--   We take a `Bound` for the scrutinee instead of a general expression because
--   we refer to it several times, and don't want to recompute it each time.
--
destructData
        :: Platform
        -> a
        -> DataCtor E.Name      -- ^ Definition of the data constructor to unpack.
        -> Bound A.Name         -- ^ Bound of Scruitinee.
        -> Type  A.Name         -- ^ Prime region.
        -> [Bind A.Name]        -- ^ Binders for each of the fields.
        -> Exp a A.Name         -- ^ Body expression that uses the field binders.
        -> ConvertM a (Exp a A.Name)

destructData pp a ctorDef uScrut trPrime bsFields xBody
 | Just HeapObjectBoxed <- heapObjectOfDataCtor pp ctorDef
 = do
        -- Bind pattern variables to each of the fields.
        let lsFields
                = catMaybes
                $ [ if isBNone bField
                        then Nothing
                        else Just $ LLet bField
                                    (A.xGetFieldOfBoxed a trPrime rField
                                                        (XVar a uScrut) ix)
                  | bField      <- bsFields
                  | rField      <- repeat A.rTop
                  | ix          <- [0..] ]

        return  $ foldr (XLet a) xBody lsFields

 | Just HeapObjectSmall <- heapObjectOfDataCtor   pp ctorDef
 , Just offsets         <- fieldOffsetsOfDataCtor pp ctorDef
 = do
        -- Get the address of the payload.
        let bPayload    = BAnon (A.tPtr trPrime (A.tWord 8))
        let xPayload    = A.xPayloadOfSmall a trPrime (XVar a uScrut)

        -- Bind pattern variables to the fields.
        let uPayload    = UIx 0
        let lsFields
                = catMaybes
                $ [ if isBNone bField
                     then Nothing
                     else Just $ LLet bField
                               $ A.xPeek a trPrime tField
                                        (A.xPlusPtr a A.rTop tField
                                                (A.xCastPtr a A.rTop tField (A.tWord 8)
                                                        (XVar a uPayload))
                                                (A.xNat a offset))
                  | bField      <- bsFields
                  | tField      <- map typeOfBind bsFields
                  | offset      <- offsets ]

        return  $ foldr (XLet a) xBody
                $ LLet bPayload xPayload : lsFields

 | otherwise
 = error $ unlines
        [ "destructData: don't know how to destruct a "
                ++ (show $ dataCtorName ctorDef)
        , "  heapObject = " ++ (show $ heapObjectOfDataCtor  pp ctorDef)
        , "  fields     = " ++ (show $ dataCtorFieldTypes ctorDef)
        , "  size       = " ++ (show $ payloadSizeOfDataCtor pp ctorDef) ]

