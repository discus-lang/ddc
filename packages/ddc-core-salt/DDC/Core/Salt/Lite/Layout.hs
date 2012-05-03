
-- | Layout of altebraic data.
module DDC.Core.Salt.Lite.Layout
where
import DDC.Core.Salt.Lite.Name
import DDC.Core.Salt.Platform
import DDC.Type.DataDef
import DDC.Type.Exp


-- | Given a constructor definition, 
--   get the offset of each field into the runtime object.
fieldOffsets :: DataCtor Name -> [Int]
fieldOffsets ctor
 = 


-- | Get the raw size of a field of this type, without padding.
fieldSize    :: Type Name -> Maybe Int
fieldSize tt
 = case tt of
        -- Convert type variables an constructors.
        TVar u          -> 
        TCon tc         -> convertTyCon tc

        TForall _ t     -> fieldSize tt

        TApp{}          -> 

        -- We shouldn't find any TSums, because field types always have
        -- kind data.
        TSum{}          -> Nothing

