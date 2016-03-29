
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
                 , [XType _ _rPrime, XType _ tA, xLength])    
                         <- takeXPrimApps xxApp
         ,  isNumericType tA
         -> Just $ do
                let a'   =  annotTail a
                xLength' <- convertX ExpArg ctx xLength         -- TODO: need length in bytes.
                return  $ XLet a' (LLet  (BAnon (A.tPtr  A.rTop A.tObj))
                                         (A.xAllocRaw a' A.rTop 0 xLength'))
                        $ XVar a' (UIx 0)

        -- Vector read.
        XCast _ CastRun xxApp@(XApp a _ _)
         | Just ( E.NameOpVector E.OpVectorRead True
                , [XType _ _rPrime, XType _ tElem, xVec, xIndex])
                        <- takeXPrimApps xxApp
         , isNumericType tElem
         -> Just $ do
                let a'  =  annotTail a
                tElem'  <- convertDataPrimitiveT tElem
                xVec'   <- convertX ExpArg ctx xVec
                xIndex' <- convertX ExpArg ctx xIndex

                return  
                 $ A.xPeekBounded a' 
                        A.rTop tElem'
                        (A.xCastPtr a' A.rTop tElem' (A.tWord 8)
                                (A.xPayloadOfRaw a' A.rTop xVec'))

                        (A.xShl a' A.tNat 
                                xIndex' 
                                (A.xStoreSize2 a' tElem'))

                        (A.xShl a' A.tNat 
                                (xVectorLength a' A.rTop xVec') 
                                (A.xStoreSize2 a' tElem'))
        _ -> Nothing


xVectorLength   
        :: a -> Type A.Name
        -> Exp a A.Name -> Exp a A.Name

xVectorLength a rVec xVec
        = A.xPromote a A.tNat (A.tWord 32)
        $ A.xPeek a rVec (A.tWord 32) 
                (A.xCastPtr a rVec (A.tWord 32) A.tObj xVec) 
                (A.xNat a 4)

                