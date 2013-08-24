
module DDC.Source.Tetra.DataDef
        ( DataDef (..)
        , typeEnvOfDataDef)
where
import DDC.Type.Exp
import DDC.Type.Env             (TypeEnv)
import qualified DDC.Type.Env   as Env
import Control.DeepSeq


-- | Data type definitions.
data DataDef n
        = DataDef
        { -- | Data type name.
          dataDefTypeName       :: !n

          -- | Type parameters.
        , dataDefParams         :: [(n, Kind n)]

          -- | Types of data constructors.
        , dataDefCtors          :: [(n, Type n)] }
        deriving Show


instance NFData (DataDef n)


-- | Take the types of data constructors from a data type definition.
typeEnvOfDataDef :: Ord n => DataDef n -> TypeEnv n
typeEnvOfDataDef def 
 = let  bs      = [ BName n k   | (n, k) <- dataDefParams def]
   in   Env.extends 
                [BName n (foldr TForall t bs)
                        | (n, t) <- dataDefCtors def]
                Env.empty
