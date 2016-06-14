
-- | Source Tetra primitive type and kind environments.
module DDC.Source.Tetra.Env
        ( -- * Primitive kind environment.
--          primKindEnv
          kindOfPrimName

          -- * Primitive type environment.
--        , primTypeEnv 
        , typeOfPrimName
        , typeOfPrimVal
        , typeOfPrimLit

        , dataDefBool)
where
import DDC.Source.Tetra.Prim
import DDC.Source.Tetra.Exp.Source
import DDC.Type.DataDef


-- Kinds ----------------------------------------------------------------------
-- | Kind environment containing kinds of primitive data types.
-- primKindEnv :: Env Name
-- primKindEnv = Env.setPrimFun kindOfPrimName Env.empty


-- | Take the kind of a primitive name.
--
--   Returns `Nothing` if the name isn't primitive. 
--
kindOfPrimName :: Name -> Maybe Type
kindOfPrimName nn
 = case nn of
        NameTyCon tc            -> Just $ kindPrimTyCon tc
        _                       -> Nothing


-- Types ----------------------------------------------------------------------
-- | Type environment containing types of primitive operators.
-- primTypeEnv :: Env Name
-- primTypeEnv = Env.setPrimFun typeOfPrimName Env.empty


-- | Take the type of a name,
--   or `Nothing` if this is not a value name.
typeOfPrimName :: Name -> Maybe Type
typeOfPrimName nn
 = case nn of
        NameVal n       -> Just $ typeOfPrimVal n
        _               -> Nothing


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
dataDefBool :: DataDef Name
dataDefBool
 = makeDataDefAlg (NameTyCon PrimTyConBool) 
        [] 
        (Just   [ (NameLitBool True,  []) 
                , (NameLitBool False, []) ])

