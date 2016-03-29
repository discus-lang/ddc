
module DDC.Core.Tetra.Convert.Exp.PrimVector
        (convertPrimVector)
where
import DDC.Core.Tetra.Convert.Exp.Base
import DDC.Core.Tetra.Convert.Boxing
import DDC.Core.Tetra.Convert.Type
import DDC.Core.Tetra.Convert.Error
import DDC.Core.Compounds
import DDC.Core.Exp
import DDC.Core.Check                           (AnTEC(..))
import qualified DDC.Core.Tetra.Prim            as E
import qualified DDC.Core.Salt.Runtime          as A
import qualified DDC.Core.Salt.Name             as A
import qualified DDC.Core.Salt.Compounds        as A


convertPrimVector
        :: Show a
        => ExpContext                   -- ^ The surrounding expression context.
        -> Context a                    -- ^ Types and values in the environment.
        -> Exp (AnTEC a E.Name) E.Name  -- ^ Expression to convert.
        -> Maybe (ConvertM a (Exp a A.Name))

convertPrimVector _ectx ctx xxRun
 = let  convertX        = contextConvertExp ctx
   in case xxRun of

        -- Vector allocate.
        -- TODO: memset payload.
        XCast _ CastRun xxApp@(XApp a _ _)
         |  Just ( E.NameOpVector E.OpVectorAlloc True
                 , [XType _ _rPrime, XType _ tElem, xLength])    
                         <- takeXPrimApps xxApp
         ,  isNumericType tElem
         -> Just $ do
                let a'   =  annotTail a

                -- The element type of the vector.
                tElem'  <- convertDataPrimitiveT tElem

                -- Length of the vector payload, in elements.
                xLengthElems'     <- convertX ExpArg ctx xLength         

                -- Length of the vector payload, in bytes.
                let xLengthBytes' = A.xShl a' A.tNat xLengthElems' 
                                        (A.xStoreSize2 a' tElem')

                return  $ XLet a' (LLet  (BAnon (A.tPtr  A.rTop A.tObj))
                                         (A.xAllocRaw a' A.rTop 0 xLengthBytes'))
                        $ XVar a' (UIx 0)

        -- Vector read.
        XCast _ CastRun xxApp@(XApp a _ _)
         | Just ( E.NameOpVector E.OpVectorRead True
                , [XType _ _rPrime, XType _ tElem, xVec, xIndex])
                        <- takeXPrimApps xxApp
         , isNumericType tElem
         -> Just $ do
                let a'  =  annotTail a

                -- The element type of the vector.
                tElem'  <- convertDataPrimitiveT tElem

                -- Pointer to the vector object.
                xVec'   <- convertX ExpArg ctx xVec

                -- Index of the element that we want.
                xIndex' <- convertX ExpArg ctx xIndex

                -- Pointer to the start of the object payload,
                -- which is the unboxed vector data.
                let xPayload'   = A.xCastPtr a' A.rTop tElem' (A.tWord 8)
                                        (A.xPayloadOfRaw a' A.rTop xVec')

                -- Offset to the starting byte of the word we want,
                -- relative to the start of the payload.
                let xStart'     = A.xShl a' A.tNat xIndex'
                                        (A.xStoreSize2 a' tElem')

                -- Length of the vector payload, in bytes.
                -- If xStart' is higher than this then we have an out-of-bounds error,
                -- which the peekBounded primop will detect.
                let xTop'       = xVectorLength a' A.rTop xVec'

                -- Read the value.
                return $ A.xPeekBounded a' A.rTop tElem' xPayload' xStart' xTop'


        -- Vector write.
        XCast _ CastRun xxApp@(XApp a _ _)
         | Just ( E.NameOpVector E.OpVectorWrite True
                , [XType _ _rPrime, XType _ tElem, xVec, xIndex, xValue])
                        <- takeXPrimApps xxApp
         , isNumericType tElem
         -> Just $ do
                let a'          = annotTail a

                -- The element type of the vector.
                tElem'          <- convertDataPrimitiveT tElem

                -- Pointer to the vector object.
                xVec'           <- convertX ExpArg ctx xVec

                -- Index of the element that we want.
                xIndex'         <- convertX ExpArg ctx xIndex

                -- The value to write.
                xValue'         <- convertX ExpArg ctx xValue

                -- Pointer to the start of the object payload,
                -- which is the unboxed vector data.
                let xPayload'   = A.xCastPtr a' A.rTop tElem' (A.tWord 8)
                                        (A.xPayloadOfRaw a' A.rTop xVec')

                -- Offset to the starting byte of the word we want,
                -- relative to the start of the payload.
                let xStart'     = A.xShl a' A.tNat xIndex'
                                        (A.xStoreSize2 a' tElem')

                -- Length of the vector payload, in bytes.
                -- If xStart' is higher than this then we have an out-of-bounds error,
                -- which the peekBounded primop will detect.
                let xTop'       = xVectorLength a' A.rTop xVec'

                -- Write the value.
                return $ A.xPokeBounded a' A.rTop tElem' xPayload' xStart' xTop' xValue'


        _ -> Nothing


-- Get the size of the vector payload, in byes.
xVectorLength   
        :: a -> Type A.Name
        -> Exp a A.Name -> Exp a A.Name

xVectorLength a rVec xVec
        = A.xSub a
                A.tNat 
                (A.xPromote a A.tNat (A.tWord 32)
                        $ A.xPeek a rVec (A.tWord 32) 
                                (A.xCastPtr a rVec (A.tWord 32) A.tObj xVec) 
                                (A.xNat a 4))
                (A.xNat a 8)


