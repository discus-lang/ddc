
module DDC.Core.Salt.Lite.Convert.Data
        ( constructData
        , destructData)
where
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


-- | Build an expression that allocates and initialises a data constructor
--   object.
constructData
        :: Show a
        => Platform
        -> a
        -> DataCtor L.Name
        -> [Exp a O.Name]
        -> ConvertM a (Exp a O.Name)

constructData pp a ctorDef _xsArgs 

 | Just L.HeapObjectBoxed    <- L.heapObjectOfDataCtor ctorDef
 = error $ "constructData: not finished boxed"

 | Just L.HeapObjectRawSmall    <- L.heapObjectOfDataCtor ctorDef
 , Just size                    <- L.payloadSizeOfDataCtor  pp ctorDef
 , Just _offsets                <- L.fieldOffsetsOfDataCtor pp ctorDef
 = do   
        -- Allocate the object
--        let bObject     = BAnon (O.tPtr O.tObj)
        let xAlloc      = O.xAllocRawSmall a (dataCtorTag ctorDef)
                        $ XCon a (UPrim (O.NameNat size) O.tNat)

        return  xAlloc                                                          -- TODO: write the fields.

 | otherwise
 = error $ "constructData: don't know how to construct a " 
         ++ (show $ dataCtorName ctorDef)


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
