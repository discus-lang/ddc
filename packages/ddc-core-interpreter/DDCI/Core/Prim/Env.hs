
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
import DDCI.Core.Token
import DDC.Type.Exp
import DDC.Type.Check.Env
import DDC.Type.Compounds
import qualified Data.Map               as Map
import Data.Map                         (Map)


-- | Environment containing types for all the primitive bindings.
primEnv :: Env Token
primEnv
        = Env
        { envMap        = primDataTypeKinds
        , envStack      = [] }


-- | Kinds of primitive data type constructors.
primDataTypeKinds :: Map Token (Kind Token)
primDataTypeKinds
        = Map.fromList
        [ (Token "Unit",   kData)
        , (Token "Int",    kFun kRegion kData)
        , (Token "Char",   kFun kRegion kData)
        , (Token "String", kFun kRegion kData) ]
        

