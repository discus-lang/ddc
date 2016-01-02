
-- | Source Tetra primitive type and kind environments.
module DDC.Source.Tetra.Env
        ( -- * Primitive kind environment.
          primKindEnv
        , kindOfPrimName

          -- * Primitive type environment.
        , primTypeEnv 
        , typeOfPrimName
        , typeOfPrimVal
        , typeOfPrimLit

        , dataDefBool)
where
import DDC.Source.Tetra.Prim
import DDC.Source.Tetra.Exp
import DDC.Type.DataDef
import DDC.Type.Env             (Env)
import qualified DDC.Type.Env   as Env


-- Kinds ----------------------------------------------------------------------
-- | Kind environment containing kinds of primitive data types.
primKindEnv :: Env Name
primKindEnv = Env.setPrimFun kindOfPrimName Env.empty


-- | Take the kind of a primitive name.
--
--   Returns `Nothing` if the name isn't primitive. 
--
kindOfPrimName :: Name -> Maybe (Kind Name)
kindOfPrimName nn
 = case nn of
        NameTyCon tc            -> Just $ kindPrimTyCon tc
        _                       -> Nothing


-- Types ----------------------------------------------------------------------
-- | Type environment containing types of primitive operators.
primTypeEnv :: Env Name
primTypeEnv = Env.setPrimFun typeOfPrimName Env.empty


-- | Take the type of a name,
--   or `Nothing` if this is not a value name.
typeOfPrimName :: Name -> Maybe (Type Name)
typeOfPrimName nn
 = case nn of
        NameVal n       -> Just $ typeOfPrimVal n
        _               -> Nothing


-- | Take the type of a primitive name.
typeOfPrimVal  :: PrimVal -> Type Name
typeOfPrimVal dc
 = case dc of
        PrimValLit l            -> typeOfPrimLit l
        PrimValArith p          -> typePrimArith p
        PrimValFun _p           -> error "typeOfPrimLit: finish me"


-- | Take the type of a primitive literal.
typeOfPrimLit  :: PrimLit -> Type Name
typeOfPrimLit pl
 = case pl of
        PrimLitBool     _       -> tBool
        PrimLitNat      _       -> tNat
        PrimLitInt      _       -> tInt
        PrimLitSize     _       -> error "typeOfPrimLit: finish me"
        PrimLitFloat    _ _bits -> error "typeOfPrimLit: finish me"
        PrimLitWord     _ bits  -> tWord bits
        PrimLitTextLit  _       -> tTextLit


-- | Data type definition for `Bool`.
dataDefBool :: DataDef Name
dataDefBool
 = makeDataDefAlg (NameTyCon PrimTyConBool) 
        [] 
        (Just   [ (NameLitBool True,  []) 
                , (NameLitBool False, []) ])
