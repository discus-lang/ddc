
module DDC.Core.Tetra.Convert.Exp.PrimVector
        (convertPrimVector)
where
import DDC.Core.Tetra.Convert.Exp.Base
import DDC.Core.Tetra.Convert.Boxing
import DDC.Core.Tetra.Convert.Type
import DDC.Core.Tetra.Convert.Error
import DDC.Core.Exp.Annot
import DDC.Core.Check                           (AnTEC(..))
import qualified DDC.Core.Tetra.Prim            as E
import qualified DDC.Core.Salt.Runtime          as A
import qualified DDC.Core.Salt.Name             as A
import qualified DDC.Core.Salt.Compounds        as A


convertPrimVector
        :: ExpContext                   -- ^ The surrounding expression context.
        -> Context a                    -- ^ Types and values in the environment.
        -> Exp (AnTEC a E.Name) E.Name  -- ^ Expression to convert.
        -> Maybe (ConvertM a (Exp a A.Name))

convertPrimVector _ectx ctx xxExp
 = let  convertX        = contextConvertExp ctx
   in case xxExp of

        -- Vector allocate.
        --   The runtime system zeroes the object when it allocates the space
        --   so we don't need to zero the elements separately.
        XCast _ CastRun xxApp@(XApp a _ _)
         | Just ( E.NameOpVector E.OpVectorAlloc True
                , [RType _rPrime, RType tElem, RTerm xLength])    
                <- takeXFragApps xxApp
         ,  isNumericType tElem
         -> Just $ do
                let a'   =  annotTail a

                -- The element type of the vector.
                tElem'          <- convertDataPrimitiveT tElem

                -- Length of the vector payload, in elements.
                xLengthElems'   <- convertX ExpArg ctx xLength         

                -- Length of the vector payload, in bytes.
                --   Note that the runtime system may allocate a larger object to
                --   preserve alignment constraints.
                let xLengthBytes' 
                        = A.xAdd a' A.tNat (A.xNat a' 4)
                        $ A.xShl a' A.tNat xLengthElems' (A.xStoreSize2 a' tElem')

                -- Pointer to the vector length field, in elements.
                let xPayloadLength' xVec
                        = A.xCastPtr a' A.rTop (A.tWord 32) (A.tWord 8)
                        $ A.xPayloadOfRaw a' A.rTop xVec

                return  $ XLet a' (LLet  (BAnon (A.tPtr  A.rTop A.tObj))
                                         (A.xAllocRaw a' A.rTop 0 xLengthBytes'))
                        $ XLet a' (LLet  (BNone A.tVoid)
                                         (A.xPoke a' A.rTop (A.tWord 32)
                                                (xPayloadLength' (XVar a' (UIx 0)))
                                                (A.xTruncate a' (A.tWord 32) A.tNat xLengthElems')))
                        $ XVar a' (UIx 0)


        -- Vector length.
        XApp a _ _
         | Just ( E.NameOpVector E.OpVectorLength True
                , [RType _tPrime, RType tElem, RTerm xVec])
                <- takeXFragApps xxExp
         , isNumericType tElem
         -> Just $ do
                let a'  =  annotTail a

                -- Pointer to the vector object.
                xVec'   <- convertX ExpArg ctx xVec

                -- Pointer to the vector length field, in elements.
                let xPayloadLength'
                        = A.xCastPtr a' A.rTop (A.tWord 32) (A.tWord 8)
                        $ A.xPayloadOfRaw a' A.rTop xVec'

                return  $ A.xPromote a' A.tNat (A.tWord 32)
                        $ A.xPeek a' A.rTop (A.tWord 32) xPayloadLength'


        -- Vector read.
        XCast _ CastRun xxApp@(XApp a _ _)
         | Just ( E.NameOpVector E.OpVectorRead True
                , [ RType _rPrime, RType tElem
                  , RTerm xVec,    RTerm xIndex])
                <- takeXFragApps xxApp
         , isNumericType tElem
         -> Just $ do
                let a'  =  annotTail a

                -- The element type of the vector.
                tElem'  <- convertDataPrimitiveT tElem

                -- Pointer to the vector object.
                xVec'   <- convertX ExpArg ctx xVec

                -- Index of the element that we want.
                xIndex' <- convertX ExpArg ctx xIndex

                -- Pointer to the vector length field, in elements.
                let xPayloadLength'   
                        = A.xCastPtr a' A.rTop (A.tWord 32) (A.tWord 8)
                        $ A.xPayloadOfRaw a' A.rTop xVec'

                -- Pointer to the first element value.
                let xPayloadElems'
                        = A.xCastPtr a' A.rTop tElem' (A.tWord 8)
                        $ A.xPlusPtr a' A.rTop (A.tWord 8) 
                                (A.xPayloadOfRaw a' A.rTop xVec')
                                (A.xNat a' 4)

                -- Offset to the element that we want.
                let xStart'
                        = A.xShl a' A.tNat xIndex'
                        $ A.xStoreSize2 a' tElem'

                return  $ XLet a' (LLet  (BAnon (A.tWord 32))
                                         (A.xPeek a' A.rTop (A.tWord 32) xPayloadLength'))
                        $ A.xPeekBounded a' A.rTop tElem'
                                xPayloadElems'
                                xStart'
                                (A.xShl a' A.tNat
                                        (A.xPromote a' A.tNat (A.tWord 32) (XVar a' (UIx 0)))
                                        (A.xStoreSize2 a' tElem'))

        -- Vector write.
        XCast _ CastRun xxApp@(XApp a _ _)
         | Just ( E.NameOpVector E.OpVectorWrite True
                , [ RType _rPrime, RType tElem
                  , RTerm xVec, RTerm xIndex, RTerm xValue])
                <- takeXFragApps xxApp
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

                -- Pointer to the vector length field, in elements.
                let xPayloadLength'   
                        = A.xCastPtr a' A.rTop (A.tWord 32) (A.tWord 8)
                        $ A.xPayloadOfRaw a' A.rTop xVec'

                -- Pointer to the first element value.
                let xPayloadElems'
                        = A.xCastPtr a' A.rTop tElem' (A.tWord 8)
                        $ A.xPlusPtr a' A.rTop (A.tWord 8) 
                                (A.xPayloadOfRaw a' A.rTop xVec')
                                (A.xNat a' 4)

                -- Offset to the element that we want.
                let xStart'
                        = A.xShl a' A.tNat xIndex'
                        $ A.xStoreSize2 a' tElem'

                -- Write the value.
                return  $ XLet a' (LLet  (BAnon (A.tWord 32))
                                        (A.xPeek a' A.rTop (A.tWord 32) xPayloadLength'))
                        $ A.xPokeBounded a' A.rTop tElem'
                                xPayloadElems'
                                xStart'
                                (A.xShl a' A.tNat
                                        (A.xPromote a' A.tNat (A.tWord 32) (XVar a' (UIx 0)))
                                        (A.xStoreSize2 a' tElem'))
                                xValue'

        _ -> Nothing


-- Get the size of the vector payload, in bytes.
-- 
-- * This contains the hard-coded length of the raw object payload in bytes,
--   as well as a hard-coded offset to the size field of the header.
--
{-
xVectorLength   
        :: a -> Type A.Name
        -> Exp a A.Name -> Exp a A.Name

xVectorLength a rVec xVec
 = let
        -- Read the size field of the object, 
        -- to get the total object length in bytes.
        xLengthObject  
                = A.xPromote a A.tNat (A.tWord 32)
                $ A.xPeek a rVec (A.tWord 32) 
                        (A.xPlusPtr a rVec (A.tWord 32)
                                (A.xCastPtr a rVec (A.tWord 32) A.tObj xVec)
                                (A.xNat a 4))

        -- Subtract the size of the object header,
        -- so we get payload length in bytes.
   in   A.xSub a A.tNat xLengthObject (A.xNat a 8)
-}
