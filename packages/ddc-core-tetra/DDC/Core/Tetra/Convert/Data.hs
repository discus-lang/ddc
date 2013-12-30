
module DDC.Core.Tetra.Convert.Data
        ( constructData
        , destructData)
where
import DDC.Core.Tetra.Convert.Type
import DDC.Core.Tetra.Convert.Base
import DDC.Core.Tetra.Convert.Layout
import DDC.Core.Salt.Platform
import DDC.Core.Transform.LiftX
import DDC.Core.Exp
import DDC.Type.Env
import DDC.Type.Compounds
import DDC.Type.Predicates
import DDC.Type.DataDef
import DDC.Control.Monad.Check                  (throw)
import qualified DDC.Core.Tetra.Prim            as E
import qualified DDC.Core.Salt.Runtime          as A
import qualified DDC.Core.Salt.Name             as A
import qualified DDC.Core.Salt.Compounds        as A
import Data.Maybe


-- Construct ------------------------------------------------------------------
-- | Build an expression that allocates and initialises a data object.
constructData
        :: Show a
        => Platform                     -- ^ Platform definition.
        -> KindEnv  E.Name              -- ^ Kind environment.
        -> TypeEnv  E.Name              -- ^ Type environment.
        -> a                            -- ^ Annotation to use on expressions.
        -> DataType E.Name              -- ^ Data Type definition of object.
        -> DataCtor E.Name              -- ^ Constructor definition of object.
        -> Type     A.Name              -- ^ Prime region variable.
        -> [Exp a   A.Name]             -- ^ Field values.
        -> [Type    A.Name]             -- ^ Field types.
        -> ConvertM a (Exp a A.Name)

constructData pp kenv _tenv a dataDef ctorDef rPrime xsArgs tsArgs 
 | Just HeapObjectBoxed <- heapObjectOfDataCtor pp ctorDef
 = do
        -- We want to write the fields into the newly allocated object.
        -- As xsArgs list also contains type arguments, 
        --   we need to drop these off first.
        let xsFields    = drop (length $ dataTypeParams dataDef) xsArgs

        -- Get the regions each of the objects are in.
        let tsFields    = drop (length $ dataTypeParams dataDef) tsArgs

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
                  | trField     <- tsFields ]

        return  $ XLet a (LLet bObject xAlloc)
                $ foldr (XLet a) xObject' lsFields


 | Just HeapObjectRawSmall      <- heapObjectOfDataCtor  pp ctorDef
 , Just size                    <- payloadSizeOfDataCtor pp ctorDef
 = do   
        -- Allocate the object.
        let bObject     = BAnon (A.tPtr rPrime A.tObj)
        let xAlloc      = A.xAllocRawSmall a rPrime (dataCtorTag ctorDef)
                        $ A.xNat a size

        -- Take a pointer to its payload.
        let bPayload    = BAnon (A.tPtr rPrime (A.tWord 8))
        let xPayload    = A.xPayloadOfRawSmall a rPrime
                        $ XVar a (UIx 0)

        -- Convert the field types.
        tsFields        <- mapM (convertRepableT kenv) 
                        $  dataCtorFieldTypes ctorDef

        -- We want to write the fields into the newly allocated object.
        -- The xsArgs list also contains type arguments, so we need to
        --  drop these off first.
        let xsFields    = drop (length $ dataTypeParams dataDef) xsArgs

        -- Get the offset of each field.
        let Just offsets = fieldOffsetsOfDataCtor pp ctorDef

        -- Statements to write each of the fields.
        let xObject'    = XVar a $ UIx 1
        let xPayload'   = XVar a $ UIx 0
        let lsFields    = [ LLet (BNone A.tVoid)
                                 (A.xPokeBuffer a rPrime tField xPayload'
                                                offset (liftX 2 xField))
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
 | Just HeapObjectBoxed         <- heapObjectOfDataCtor pp ctorDef
 = do   
        -- Bind pattern variables to each of the fields.
        let lsFields      
                = catMaybes
                $ [ if isBNone bField
                        then Nothing
                        else Just $ LLet bField 
                                    (A.xGetFieldOfBoxed a trPrime tField
                                                        (XVar a uScrut) ix)
                  | bField      <- bsFields
                  | tField      <- map typeOfBind bsFields
                  | ix          <- [0..] ]

        return  $ foldr (XLet a) xBody lsFields

 | Just HeapObjectRawSmall      <- heapObjectOfDataCtor   pp ctorDef
 , Just offsets                 <- fieldOffsetsOfDataCtor pp ctorDef
 = do   
        -- Get the address of the payload.
        let bPayload    = BAnon (A.tPtr trPrime (A.tWord 8))
        let xPayload    = A.xPayloadOfRawSmall a trPrime (XVar a uScrut)

        -- Bind pattern variables to the fields.
        let uPayload    = UIx 0
        let lsFields    
                = catMaybes
                $ [ if isBNone bField
                     then Nothing 
                     else Just $ LLet bField 
                                     (A.xPeekBuffer a trPrime tField 
                                              (XVar a uPayload) offset)
                  | bField      <- bsFields
                  | tField      <- map typeOfBind bsFields
                  | offset      <- offsets ]

        return  $ foldr (XLet a) xBody
                $ LLet bPayload xPayload : lsFields

 | otherwise
 = throw ErrorInvalidAlt
