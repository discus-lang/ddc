
module DDC.Core.Lite.Convert.Data
        ( constructData
        , destructData)
where
import DDC.Core.Lite.Convert.Type
import DDC.Core.Lite.Convert.Base
import DDC.Core.Salt.Platform
import DDC.Core.Transform.LiftX
import DDC.Core.Exp
import DDC.Type.Compounds
import DDC.Type.Predicates
import DDC.Type.DataDef
import DDC.Type.Check.Monad             (throw)
import qualified DDC.Core.Lite.Layout   as L
import qualified DDC.Core.Lite.Name     as L
import qualified DDC.Core.Salt.Runtime  as O
import qualified DDC.Core.Salt.Name     as O
import qualified DDC.Core.Salt.Env      as O


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

 | Just L.HeapObjectBoxed       <- L.heapObjectOfDataCtor ctorDef
 , Just size                    <- L.payloadSizeOfDataCtor pp ctorDef
 , XType r@(TVar u)             <- head xsArgs
 , isRegionKind (typeOfBound u)
 = do
        -- Allocate the object.
        let bObject     = BAnon (O.tPtr r O.tObj)
        let xAlloc      = O.xAllocBoxed a (dataCtorTag ctorDef)
                        $ XCon a (UPrim (O.NameNat size) O.tNat)

        -- We want to write the fields into the newly allocated object.
        -- The xsArgs list also contains type arguments, so we need to
        --  drop these off first.
        let xsFields     = drop (length $ dataTypeParamKinds dataDef) xsArgs

        -- Statements to write each of the fields.
        let xObject'    = XVar a $ UIx 0 $ O.tPtr r O.tObj
        let lsFields    = [ LLet LetStrict (BNone O.tVoid)
                                (O.xSetFieldOfBoxed a xObject' ix (liftX 1 xField))
                                | ix            <- [0..]
                                | xField        <- xsFields]

        return  $ XLet a (LLet LetStrict bObject xAlloc)
                $ foldr (XLet a) xObject' lsFields


 | Just L.HeapObjectRawSmall    <- L.heapObjectOfDataCtor ctorDef
 , Just size                    <- L.payloadSizeOfDataCtor  pp ctorDef
 , XType r@(TVar u)             <- head xsArgs
 , isRegionKind (typeOfBound u)
 = do   
        -- Allocate the object.
        let bObject     = BAnon (O.tPtr r O.tObj)
        let xAlloc      = O.xAllocRawSmall a (dataCtorTag ctorDef)
                        $ XCon a (UPrim (O.NameNat size) O.tNat)

        -- Take a pointer to its payload.
        let bPayload    = BAnon O.tAddr
        let xPayload    = O.xPayloadOfRawSmall a (XVar a (UIx 0 $ O.tPtr r O.tObj))

        -- Convert the field types.
        tsFields         <- mapM convertT $ dataCtorFieldTypes ctorDef

        -- We want to write the fields into the newly allocated object.
        -- The xsArgs list also contains type arguments, so we need to
        --  drop these off first.
        let xsFields     = drop (length $ dataTypeParamKinds dataDef) xsArgs

        -- Get the offset of each field.
        let Just offsets = L.fieldOffsetsOfDataCtor pp ctorDef

        -- Statements to write each of the fields.
        let xObject'    = XVar a $ UIx 1 $ O.tPtr r O.tObj
        let xPayload'   = XVar a $ UIx 0 O.tAddr
        let lsFields    = [ LLet LetStrict (BNone O.tVoid)
                                (O.xWrite a tField xPayload' offset (liftX 2 xField))
                                | tField        <- tsFields
                                | offset        <- offsets
                                | xField        <- xsFields]

        return  $ XLet a (LLet LetStrict bObject  xAlloc)
                $ XLet a (LLet LetStrict bPayload xPayload)
                $ foldr (XLet a) xObject' lsFields

 | otherwise
 = error $ unlines
        [ "constructData: don't know how to construct a " 
                ++ (show $ dataCtorName ctorDef)
        , "  heapObject = " ++ (show $ L.heapObjectOfDataCtor ctorDef) 
        , "  fields     = " ++ (show $ dataCtorFieldTypes ctorDef)
        , "  size       = " ++ (show $ L.payloadSizeOfDataCtor pp ctorDef) ]


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
                                (O.xGetFieldOfBoxed a (XVar a uScrut) ix)
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

        return  $ foldr (XLet a) xBody
                $ LLet LetStrict bPayload xPayload
                : lsFields


 | otherwise
 = throw ErrorInvalidAlt

