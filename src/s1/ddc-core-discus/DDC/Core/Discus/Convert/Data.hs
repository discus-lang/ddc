
module DDC.Core.Discus.Convert.Data
        ( constructData
        , destructData)
where
import DDC.Core.Discus.Convert.Error
import DDC.Core.Discus.Convert.Layout
import DDC.Core.Salt.Platform
import DDC.Core.Transform.BoundX
import DDC.Core.Exp
import DDC.Core.Module.Name
import DDC.Data.Pretty
import DDC.Type.Exp.Simple
import DDC.Type.DataDef
import qualified DDC.Core.Discus.Prim           as D
import qualified DDC.Core.Salt.Runtime          as A
import qualified DDC.Core.Salt.Name             as A
import qualified DDC.Core.Salt.Compounds        as A
import qualified Data.Text                      as T
import Data.Maybe


-- Env --------------------------------------------------------------------------------------------
-- TODO: reuse this at the construction site.
nameOfInfoIndexCtorRef :: ModuleName -> Text -> Text
nameOfInfoIndexCtorRef (ModuleName parts) txCtorName
 = let  mn' = T.intercalate (T.pack ".") $ map T.pack parts
   in   "ddcInfoIndex.data." % mn' % "." % txCtorName


-- Construct --------------------------------------------------------------------------------------
-- | Build an expression that allocates and initialises a data object.
constructData
        :: Show a
        => Platform                     -- ^ Platform definition.
        -> a                            -- ^ Annotation to use on expressions.
        -> DataCtor D.Name              -- ^ Constructor definition of object.
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

        -- Expression to lookup the info table index for this data
        --   constructor, which is stored in a global variable named
        --   after the fully-qualified constructor name.
        let xInfoIndex
             = case dataCtorName ctorDef of
                D.NameCon txCtorName
                 -> let txInfoRef
                         = nameOfInfoIndexCtorRef
                                (dataCtorModuleName ctorDef) txCtorName
                    in  A.xRead a (A.tWord 32)
                                  (A.xGlobal a (A.tWord 32) txInfoRef)
                                  (A.xNat a 0)
                _ -> A.xWord a 0 32

        -- Allocate a boxed object to represent the data contructor.
        let xAlloc
             = A.xAllocBoxed a rPrime (dataCtorTag ctorDef) xInfoIndex
             $ A.xNat a (fromIntegral arity)

        -- Statements to write each of the fields.
        let xObject' = XVar a $ UIx 0
        let lsFields
             = [ LLet (BNone A.tVoid)
                        (A.xSetFieldOfBoxed a
                        rPrime trField xObject' ix (liftX 1 xField))
               | ix      <- [0..]
               | xField  <- xsFields
               | trField <- repeat A.rTop ]

        return $ XLet a (LLet bObject xAlloc)
               $ foldr (XLet a) xObject' lsFields


 | Just HeapObjectSmall <- heapObjectOfDataCtor  pp ctorDef
 , Just sizeBytes       <- payloadSizeOfDataCtor pp ctorDef
 = do
        -- Convert size of payload in bytes to size in words,
        -- as we allocate space for a small object in words.
        let nTail       = sizeBytes `rem` platformAddrBytes pp
        let nWords      = if nTail == 0
                                then  sizeBytes `div` platformAddrBytes pp
                                else (sizeBytes `div` platformAddrBytes pp) + 1

        -- Binder for the allocated object.
        let bObject     = BAnon (A.tPtr rPrime A.tObj)

        -- Expression to lookup the info table index for this value
        --   which is stored in a global variable named after the
        --   primitive value. These names need to match the ones
        --   in Info.dcs in the runtime system.
        let txTypeName
                -- For boxed primitive types we grimily reuse the data
                -- type name as the data constructor name.
                -- TODO: make proper data constructor names for these.
                | D.NamePrimTyCon tc <- dataCtorName ctorDef
                , Just tx            <- infoTableRowNameOfPrimTyCon tc
                = tx

                | otherwise
                = error $ "ddc-core.constructData: don't know what info index "
                        ++ "to use for " ++ show (dataCtorName ctorDef)

        let txInfoRef   = nameOfInfoIndexCtorRef (ModuleName ["Base"]) txTypeName

        let xInfoIndex  = A.xRead a (A.tWord 32)
                                (A.xGlobal a (A.tWord 32) txInfoRef)
                                (A.xNat a 0)

        let xAlloc      = A.xAllocSmall a rPrime xInfoIndex
                        $ A.xNat a nWords

        -- Take a pointer to its payload.
        let bPayload    = BAnon (A.tPtr rPrime (A.tWord 8))
        let xPayload    = A.xPayloadOfSmall a rPrime
                        $ XVar a (UIx 0)

        -- Get the offset of each field.
        let Just offsets = fieldOffsetsOfDataCtor pp ctorDef

        -- Statements to write each of the fields.
        let xObject'    = XVar a $ UIx 1
        let xPayload'   = XVar a $ UIx 0
        let lsFields
                = [ LLet (BNone A.tVoid)
                         (A.xPoke a rPrime tField
                                (A.xPlusPtr a A.rTop tField
                                        (A.xCastPtr a A.rTop tField (A.tWord 8) xPayload')
                                        (A.xNat a offset))
                                (liftX 2 xField))
                  | tField <- tsFields
                  | offset <- offsets
                  | xField <- xsFields]

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


-- | Names of the info table symbol and associated row to use for
--   small primitive object. These names need to match the ones
--   in Info.dcs of the runtime system.
infoTableRowNameOfPrimTyCon :: D.PrimTyCon -> Maybe Text
infoTableRowNameOfPrimTyCon ptc
 = case ptc of
        D.PrimTyConTextLit      -> Just "TextLit"
        D.PrimTyConBool         -> Just "Bool"
        D.PrimTyConNat          -> Just "Nat"
        D.PrimTyConInt          -> Just "Int"
        D.PrimTyConSize         -> Just "Size"
        D.PrimTyConWord  8      -> Just "Word8"
        D.PrimTyConWord  16     -> Just "Word16"
        D.PrimTyConWord  32     -> Just "Word32"
        D.PrimTyConWord  64     -> Just "Word64"
        D.PrimTyConFloat 32     -> Just "Float32"
        D.PrimTyConFloat 64     -> Just "Float64"
        D.PrimTyConAddr         -> Just "Addr"
        D.PrimTyConPtr          -> Just "Ptr"
        _                       -> Nothing


-- Destruct ---------------------------------------------------------------------------------------
-- | Wrap a expression in let-bindings that bind each of the fields of
--   of a data object. This is used when pattern matching in a case expression.
--
--   We take a `Bound` for the scrutinee instead of a general expression because
--   we refer to it several times, and don't want to recompute it each time.
--
destructData
        :: Platform
        -> a
        -> DataCtor D.Name      -- ^ Definition of the data constructor to unpack.
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
                  | bField <- bsFields
                  | rField <- repeat A.rTop
                  | ix     <- [0..] ]

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
                  | bField <- bsFields
                  | tField <- map typeOfBind bsFields
                  | offset <- offsets ]

        return  $ foldr (XLet a) xBody
                $ LLet bPayload xPayload : lsFields

 | otherwise
 = error $ unlines
        [ "destructData: don't know how to destruct a "
                ++ (show $ dataCtorName ctorDef)
        , "  heapObject = " ++ (show $ heapObjectOfDataCtor  pp ctorDef)
        , "  fields     = " ++ (show $ dataCtorFieldTypes ctorDef)
        , "  size       = " ++ (show $ payloadSizeOfDataCtor pp ctorDef) ]

