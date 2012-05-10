
module DDC.Core.Salt.Lite.Convert.Data
        ( constructData
        , destructData)
where
import DDC.Core.Salt.Lite.Convert.Type
import DDC.Core.Salt.Lite.Convert.Base
import DDC.Core.Salt.Platform
import DDC.Core.Exp
import DDC.Type.Compounds
import DDC.Type.DataDef
import DDC.Type.Check.Monad                     (throw)
import qualified DDC.Core.Salt.Lite.Layout      as L
import qualified DDC.Core.Salt.Lite.Name        as L
import qualified DDC.Core.Salt.Output.Runtime   as O
import qualified DDC.Core.Salt.Output.Name      as O
import qualified DDC.Core.Salt.Output.Env       as O

-- Construct ------------------------------------------------------------------
-- | Build an expression that allocates and initialises a data constructor.
--   object.
constructData
        :: Show a
        => Platform             -- ^ Platform definition.
        -> a                    -- ^ Annotation to use on expressions.
        -> DataType L.Name      -- ^ Data Type definition of object.
        -> DataCtor L.Name      -- ^ Constructor definition of object.
        -> [Exp a O.Name]       -- ^ Field values.
        -> ConvertM a (Exp a O.Name)

constructData pp a dataDef ctorDef xsArgs 

 | Just L.HeapObjectBoxed    <- L.heapObjectOfDataCtor ctorDef
 = error $ "constructData: not finished boxed"

 | Just L.HeapObjectRawSmall    <- L.heapObjectOfDataCtor ctorDef
 , Just size                    <- L.payloadSizeOfDataCtor  pp ctorDef
 = do   
        -- Allocate the object.
        let bObject     = BAnon (O.tPtr O.tObj)
        let xAlloc      = O.xAllocRawSmall a (dataCtorTag ctorDef)
                        $ XCon a (UPrim (O.NameNat size) O.tNat)

        -- Take a pointer to its payload.
        let bPayload    = BAnon O.tAddr
        let xPayload    = O.xPayloadOfRawSmall a (XVar a (UIx 0 $ O.tPtr O.tObj))

        -- Write field values to the freshly allocated object.
        xBody           <- writeFields pp a dataDef ctorDef xsArgs 
                                (XVar a $ UIx 0 O.tAddr)
                                (XVar a $ UIx 1 $ O.tPtr O.tObj)

        return  $ XLet a (LLet LetStrict bObject  xAlloc)
                $ XLet a (LLet LetStrict bPayload xPayload)
                $ xBody

 | otherwise
 = error $ "constructData: don't know how to construct a " 
         ++ (show $ dataCtorName ctorDef)


-- | Wrap a body expression with let-bindings to write field values to 
--   a freshly allocated data object.
writeFields
        :: Platform             -- ^ Platform definition.
        -> a                    -- ^ Annotation to use on expressions.
        -> DataType L.Name      -- ^ Data Type definition of object.
        -> DataCtor L.Name      -- ^ Contructor definition of object.
        -> [Exp a O.Name]       -- ^ Field values.
        -> Exp a O.Name         -- ^ Address of the payload.
        -> Exp a O.Name         -- ^ Body of the expression.
        -> ConvertM a (Exp a O.Name)

writeFields pp a dataDef ctorDef xsArgs xPayload xBody
 = do   -- Convert the field types.
        tsFields         <- mapM convertT $ dataCtorFieldTypes ctorDef

        -- We want to write the fields into the newly allocated object.
        -- The xsArgs list also contains type arguments, so we need to
        --  drop these off first.
        let xsFields     = drop (length $ dataTypeParamKinds dataDef) xsArgs

        -- Get the offset of each field.
        let Just offsets = L.fieldOffsetsOfDataCtor pp ctorDef

        -- Statements to write each of the fields.
        let lsFields    = [ LLet LetStrict (BNone O.tVoid)
                                (O.xWrite a tField xPayload offset xField)
                                | tField        <- tsFields
                                | offset        <- offsets
                                | xField        <- xsFields]

        return $ foldr (XLet a) xBody lsFields


-- Destruct -------------------------------------------------------------------
-- | Wrap a expression in let-bindings that binds the fields of a data 
--   construct object.
--   This is used when pattern matching in a case expression.
destructData 
        :: Platform 
        -> a
        -> Bound O.Name         -- ^ Bound of Scruitinee.
        -> DataCtor L.Name      -- ^ Definition of the data constructor to unpack
        -> [Bind O.Name]        -- ^ Binders for each of the fields.
        -> Exp a O.Name         -- ^ Body expression that uses the field binders.
        -> ConvertM a (Exp a O.Name)

destructData pp a uScrut ctorDef bsFields xBody

 | Just L.HeapObjectBoxed    <- L.heapObjectOfDataCtor ctorDef
 = do   
        -- Bind pattern variables to each of the fields.
        let lsFields    = [ LLet LetStrict bField 
                                (O.xFieldOfBoxed a (XVar a uScrut) ix)
                                | bField        <- bsFields
                                | ix            <- [0..] ]

        return  $ foldr (XLet a) xBody lsFields

 | Just L.HeapObjectRawSmall <- L.heapObjectOfDataCtor ctorDef
 , Just offsets              <- L.fieldOffsetsOfDataCtor pp ctorDef
 = do   
        -- Get the address of the payload.
        let bPayload    = BAnon O.tAddr
        let xPayload    = O.xPayloadOfRawSmall a (XVar a uScrut)

        -- Bind pattern variables to the fields.
        let uPayload    = UIx 0 O.tAddr
        let lsFields    = [ LLet LetStrict bField 
                                (O.xRead a tField (XVar a uPayload) offset) 
                                | bField        <- bsFields
                                | tField        <- map typeOfBind bsFields
                                | offset        <- offsets ]

        -- TODO: lift body expression
        return  $ foldr (XLet a) xBody
                $ LLet LetStrict bPayload xPayload
                : lsFields


 | otherwise
 = throw ErrorInvalidAlt
