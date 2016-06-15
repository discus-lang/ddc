
-- | Source Tetra primitive type and kind environments.
module DDC.Source.Tetra.Env
        ( typeOfPrimVal
        , typeOfPrimLit)
--        , dataDefBool)
where
import DDC.Source.Tetra.Prim
import DDC.Source.Tetra.Exp.Source
-- import DDC.Type.DataDef


-- | Take the type of a primitive name.
typeOfPrimVal  :: PrimVal -> Type
typeOfPrimVal dc
 = case dc of
        PrimValLit    l         -> typeOfPrimLit l
        PrimValArith  p         -> typePrimArith Source p
        PrimValError  p         -> typeOpError   Source p
        PrimValVector p         -> typeOpVector  Source p
        PrimValFun    p         -> typeOpFun     Source p


-- | Take the type of a primitive literal.
typeOfPrimLit   :: PrimLit -> Type
typeOfPrimLit pl
 = case pl of
        PrimLitBool     _       -> TBool
        PrimLitNat      _       -> TNat
        PrimLitInt      _       -> TInt
        PrimLitSize     _       -> TSize
        PrimLitFloat    _ bits  -> TFloat bits
        PrimLitWord     _ bits  -> TWord  bits
        PrimLitTextLit  _       -> TTextLit


-- | Data type definition for `Bool`.
{-
dataDefBool :: DataDef Name
dataDefBool
 = makeDataDefAlg (NameTyCon PrimTyConBool) 
        [] 
        (Just   [ (NameLitBool True,  []) 
                , (NameLitBool False, []) ])
-}
