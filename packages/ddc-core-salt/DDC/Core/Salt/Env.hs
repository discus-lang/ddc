
-- | Types of Disciple Core Salt primops.
module DDC.Core.Salt.Env
        ( primDataDefs
        , primKindEnv
        , primTypeEnv
        , typeOfPrimOp
        , typeOfPrimArith
        , typeOfPrimCast
        , typeOfPrimCall
        , typeOfPrimControl
        , typeOfPrimStore 
        , typeOfPrimLit
        , typeIsUnboxed)
where
import DDC.Core.Salt.Compounds.PrimArith
import DDC.Core.Salt.Compounds.PrimCast
import DDC.Core.Salt.Compounds.PrimControl
import DDC.Core.Salt.Compounds.PrimStore
import DDC.Core.Salt.Compounds
import DDC.Core.Salt.Name
import DDC.Type.DataDef
import DDC.Type.Exp.Simple
import DDC.Type.Env                             (Env)
import qualified DDC.Type.Env                   as Env


-- DataDefs -------------------------------------------------------------------
-- | Data type definitions for:
--
-- >  Type                        Constructors
-- >  ----                --------------------------
-- >  Bool#               True# False#
-- >  Nat#                0# 1# 2# ...
-- >  Int#                ... -2i# -1i# 0i# 1i# 2i# ...
-- >  Size#               0s# 1s# 2s# ...
-- >  Word{8,16,32,64}#   42w8# 123w64# ...
-- >  Float{32,64}#       (none, convert from Int#)
-- >  Tag#                (none, convert from Nat#)
--
primDataDefs :: DataDefs Name
primDataDefs
 = fromListDataDefs
        -- Bool#
        [ makeDataDefAlg
                (NamePrimTyCon PrimTyConBool)
                []
                (Just   [ (NamePrimLit (PrimLitBool True),  [])
                        , (NamePrimLit (PrimLitBool False), []) ])
        -- Nat#
        , makeDataDefAlg (NamePrimTyCon PrimTyConNat)        [] Nothing

        -- Int#
        , makeDataDefAlg (NamePrimTyCon PrimTyConInt)        [] Nothing

        -- Size#
        , makeDataDefAlg (NamePrimTyCon PrimTyConSize)       [] Nothing

        -- Word# 8, 16, 32, 64
        , makeDataDefAlg (NamePrimTyCon (PrimTyConWord 8))   [] Nothing
        , makeDataDefAlg (NamePrimTyCon (PrimTyConWord 16))  [] Nothing
        , makeDataDefAlg (NamePrimTyCon (PrimTyConWord 32))  [] Nothing
        , makeDataDefAlg (NamePrimTyCon (PrimTyConWord 64))  [] Nothing

        -- Float# 32, 64
        , makeDataDefAlg (NamePrimTyCon (PrimTyConFloat 32)) [] Nothing
        , makeDataDefAlg (NamePrimTyCon (PrimTyConFloat 64)) [] Nothing

        -- Tag#
        , makeDataDefAlg (NamePrimTyCon PrimTyConTag)        [] Nothing

        -- TextLit#
        , makeDataDefAlg (NamePrimTyCon PrimTyConTextLit)    [] Nothing

        -- Ptr#
        , makeDataDefAlg (NamePrimTyCon PrimTyConPtr)        [] Nothing
        ]


-- Kinds ----------------------------------------------------------------------
-- | Kind environment containing kinds of primitive data types.
primKindEnv :: Env Name
primKindEnv = Env.setPrimFun kindOfName Env.empty


-- | Take the kind of a name, 
--   or `Nothing` if this is not a type name.
kindOfName :: Name -> Maybe (Kind Name)
kindOfName nn
 = case nn of
        NameObjTyCon      -> Just $ kData
        NamePrimTyCon tc  -> Just $ kindOfPrimTyCon tc
        NameVar "rT"      -> Just $ kRegion
        _                 -> Nothing


-- | Take the kind of a primitive name.
--
--   Returns `Nothing` if the name isn't primitive. 
--
kindOfPrimTyCon :: PrimTyCon -> Kind Name
kindOfPrimTyCon tc
 = case tc of
        PrimTyConVoid    -> kData
        PrimTyConBool    -> kData
        PrimTyConNat     -> kData
        PrimTyConInt     -> kData
        PrimTyConSize    -> kData
        PrimTyConWord  _ -> kData
        PrimTyConFloat _ -> kData
        PrimTyConAddr    -> kData
        PrimTyConPtr     -> kRegion `kFun` kData `kFun` kData
        PrimTyConTag     -> kData
        PrimTyConVec   _ -> kData `kFun` kData
        PrimTyConTextLit -> kData


-- Types ----------------------------------------------------------------------
-- | Type environment containing types of primitive operators.
primTypeEnv :: Env Name
primTypeEnv = Env.setPrimFun typeOfName Env.empty


-- | Take the type of a name,
--   or `Nothing` if this is not a value name.
typeOfName :: Name -> Maybe (Type Name)
typeOfName nn
 = case nn of
        NamePrimOp p        -> Just $ typeOfPrimOp p
        NamePrimLit lit     -> Just $ typeOfPrimLit lit
        _                   -> Nothing


-- | Take the type of a primitive operator.
typeOfPrimOp :: PrimOp -> Type Name
typeOfPrimOp pp
 = case pp of
        PrimArith    op -> typeOfPrimArith    op
        PrimCast     cc -> typeOfPrimCast     cc
        PrimCall     pc -> typeOfPrimCall     pc
        PrimControl  pc -> typeOfPrimControl  pc
        PrimStore    ps -> typeOfPrimStore    ps


-- | Take the type of a primitive literal.
typeOfPrimLit :: PrimLit -> Type Name
typeOfPrimLit lit
 = case lit of
        PrimLitVoid           -> tVoid
        PrimLitBool    _      -> tBool
        PrimLitNat     _      -> tNat
        PrimLitInt     _      -> tInt
        PrimLitSize    _      -> tSize
        PrimLitWord    _ bits -> tWord  bits
        PrimLitFloat   _ bits -> tFloat bits
        PrimLitTextLit _      -> tTextLit
        PrimLitTag     _      -> tTag


-- PrimCall -------------------------------------------------------------------
-- | Take the type of a primitive call operator.
typeOfPrimCall :: PrimCall -> Type Name
typeOfPrimCall cc
 = case cc of
        PrimCallStd  arity
         -> makePrimCallStdType  arity

        PrimCallTail arity       
         -> makePrimCallTailType arity


-- | Make the type of the @callN#@ and @tailcallN@ primitives.
makePrimCallStdType :: Int -> Type Name
makePrimCallStdType arity
 = let Just t   = tFunOfList ([tAddr] ++ replicate arity tAddr ++ [tAddr])
   in  t
        

-- | Make the type of the @callN#@ and @tailcallN@ primitives.
makePrimCallTailType :: Int -> Type Name
makePrimCallTailType arity
 = let  tSuper   = foldr tFun 
                         (TVar (UIx 0))
                         (reverse [TVar (UIx i) | i <- [1..arity]])

        tCall    = foldr TForall (tSuper `tFun` tSuper) 
                         [BAnon k | k <- replicate (arity + 1) kData]

   in   tCall


-------------------------------------------------------------------------------
-- | Check if a type is an unboxed data type.
typeIsUnboxed :: Type Name -> Bool
typeIsUnboxed tt
 = case tt of
        TVar{}          -> False

        -- All plain constructors are unboxed.
        TCon (TyConBound _ k)
         | isDataKind k -> True

        TCon _          -> False

        TForall _ t     -> typeIsUnboxed t

        -- Pointers to objects are boxed.
        TApp{}
         | Just (_tR, tTarget)  <- takeTPtr tt
         , tTarget == tObj
         -> False

        TApp t1 t2      
         -> typeIsUnboxed t1 || typeIsUnboxed t2

        TSum{}          -> False


