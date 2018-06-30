
module DDC.Core.Discus.Convert.Type.DaCon
        (convertDaCon)
where
import DDC.Core.Discus.Convert.Type.Super
import DDC.Core.Discus.Convert.Type.Base
import DDC.Core.Discus.Convert.Error
import DDC.Core.Exp.Annot
import DDC.Control.Check                        (throw)
import qualified DDC.Core.Discus.Prim           as E
import qualified DDC.Core.Salt.Name             as A


-- | Convert a data constructor definition.
convertDaCon
        :: Context
        -> DaCon E.Name (Type E.Name)
        -> ConvertM a (DaCon A.Name (Type A.Name))

convertDaCon ctx dc
 = case dc of
        DaConUnit       -> return $ DaConUnit
        DaConRecord ns  -> return $ DaConRecord ns

        DaConPrim n t
         -> do  n'      <- convertDaConNameM dc n
                t'      <- convertSuperT ctx t
                return  $ DaConPrim
                        { daConName             = n'
                        , daConType             = t' }

        DaConBound (DaConBoundName _ _ n)
         -> do  n'      <- convertDaConNameM dc n
                return  $ DaConBound (DaConBoundName Nothing Nothing n')


-- | Convert the name of a data constructor.
convertDaConNameM
        :: DaCon E.Name (Type E.Name)
        -> E.Name
        -> ConvertM a A.Name

convertDaConNameM dc nn
 = case nn of
        E.NameLitUnboxed (E.NameLitBool val)
          -> return $ A.NamePrimLit $ A.PrimLitBool val

        E.NameLitUnboxed (E.NameLitNat  val)
          -> return $ A.NamePrimLit $ A.PrimLitNat  val

        E.NameLitUnboxed (E.NameLitInt  val)
          -> return $ A.NamePrimLit $ A.PrimLitInt  val

        E.NameLitUnboxed (E.NameLitWord val bits)
          -> return $ A.NamePrimLit $ A.PrimLitWord val bits

        E.NameLitUnboxed (E.NameLitFloat val bits)
          -> return $ A.NamePrimLit $ A.PrimLitFloat val bits

        _ -> throw $ ErrorInvalidDaCon dc

