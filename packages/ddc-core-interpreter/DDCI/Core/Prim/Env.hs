
-- | Primitive types and operators for the interpreter.
--
--   These are only a subset of the primitives supported by the real compiler, there's just
--   enough to experiment with the core language. When we end up wanting to interpret full
--   Disciple programs, we should use the primops defined by the real compiler.
--
module DDCI.Core.Prim.Env
        ( primEnv
        , primDataTypeKinds)
where
import DDCI.Core.Prim.Name
import DDC.Type.Exp
import DDC.Type.Check.Env
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
        , (Name "String", kFun kRegion kData) ]
        

