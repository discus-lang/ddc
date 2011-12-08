
-- | Primitive types and operators for the interpreter.
--
--   These are only a subset of the primitives supported by the real compiler, there's just
--   enough to experiment with the core language. When we end up wanting to interpret full
--   Disciple programs, we should use the primops defined by the real compiler.
--
module DDCI.Core.Prim.Env
        ( primEnv
        , primDataTypeKinds
        , typeOfPrim)
where
import DDCI.Core.Prim.Name
import DDCI.Core.Prim.Base
import DDC.Type.Exp
import DDC.Type.Env
import DDC.Type.Compounds
import qualified Data.Map               as Map
import Data.Map                         (Map)


-- | Environment containing types for all the primitive bindings.
primEnv :: Env Name
primEnv
        = Env
        { envMap        = primDataTypeKinds
        , envStack      = [] }


-- | Kinds of primitive data type constructors.
primDataTypeKinds :: Map Name (Kind Name)
primDataTypeKinds
        = Map.fromList
        [ (Name "Unit",   kData)
        , (Name "Int",    kFun kRegion kData)
        , (Name "Char",   kFun kRegion kData)
        , (Name "String", kFun kRegion kData) 
        
        , (Name "U",      tUnit)]


tInt :: Region Name -> Type Name
tInt r1 = tConData1 (Name "Int") (kFun kRegion kData) r1

tUnit :: Type Name
tUnit   = tConData0 (Name "Unit") kData


-- Checking -------------------------------------------------------------------------------------
-- | Yield the type of a primitive.
typeOfPrim :: Prim -> Type Name
typeOfPrim pp
 = case pp of
        PInt _          
         -> tForall kRegion
          $ \r  -> tFun tUnit (tAlloc r)
                              (tBot kClosure)
                 $ tInt r

        -- TODO: check this is in the store typing
        PRgn _
         -> kRegion

        PPrimOp op      -> typeOfPrimOp op


-- | Yield the type of a primitive operator.
typeOfPrimOp :: PrimOp -> Type Name
typeOfPrimOp op
 = case op of
    OpAdd       -> tForalls [kRegion, kRegion, kRegion] $ \[r2, r1, r0] 
                        -> tFun (tInt r2) (tBot kEffect)
                                          (tBot kClosure)
                         $ tFun (tInt r1) (tSum kEffect  [tRead r2, tRead r1, tAlloc r0])
                                          (tSum kClosure [tShare r2])
                         $ tInt r0

