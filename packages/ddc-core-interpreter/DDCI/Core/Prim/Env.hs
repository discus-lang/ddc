
-- | Primitive types and operators for the interpreter.
--
--   These are only a subset of the primitives supported by the real compiler, there's just
--   enough to experiment with the core language. When we end up wanting to interpret full
--   Disciple programs, we should use the primops defined by the real compiler.
--
module DDCI.Core.Prim.Env
        ( primEnv
        , primDataTypeKinds

        , Prim  (..)
        , PrimOp(..)
        , makePrimLit
        , makePrimExp
        , typeOfPrim)
where
import DDCI.Core.Prim.Name
import DDC.Type.Exp
import DDC.Type.Env
import DDC.Type.Compounds
import DDC.Base.Literal
import DDC.Base.Pretty
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



-- Primitive things -------------------------------------------------------------------------------
data Prim
        = PInt    Integer
        | PPrimOp PrimOp
        deriving (Eq, Show)
        
data PrimOp
        = OpNeg
        | OpAdd
        | OpSub
        deriving (Eq, Show)


instance Pretty Prim where
 ppr pp
  = case pp of
        PInt i          -> text (show i)
        PPrimOp op      -> ppr op
        

instance Pretty PrimOp where
 ppr op
  = case op of
        OpNeg           -> text "neg"
        OpAdd           -> text "add"
        OpSub           -> text "sub"


-- Parsing ----------------------------------------------------------------------------------------
makePrimLit :: Literal  -> Maybe Prim
makePrimLit ll
 = case ll of
        LInteger i      -> Just $ PInt i
        _               -> Nothing

makePrimExp :: Name     -> Maybe Prim
makePrimExp (Name n)
 = case n of
        "neg"           -> Just $ PPrimOp OpNeg
        "add"           -> Just $ PPrimOp OpAdd
        "sub"           -> Just $ PPrimOp OpSub
        _               -> Nothing


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

