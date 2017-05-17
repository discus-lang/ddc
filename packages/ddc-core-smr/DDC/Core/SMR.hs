{-# LANGUAGE OverloadedStrings #-}
module DDC.Core.SMR
        ( H.Name
        , H.Module      (..)
        , H.Decl        (..)
        , H.Exp         (..)
        , H.Ref         (..)
        , smrOfTetraModule)
where
import qualified DDC.Core.Check                 as E
import qualified DDC.Core.Module                as E
import qualified DDC.Core.Exp                   as E
import qualified DDC.Core.Tetra.Prim            as E
import qualified DDC.Type.DataDef               as E
import qualified DDC.Type.Env                   as E

import qualified DDC.Core.SMR.Core.Exp          as H
import qualified DDC.Core.SMR.Source.Pretty     ()
import qualified Data.Text                      as T

type HExp       = H.Exp H.Name H.Name

infixl 4 @@
(@@) = H.XApp
sym  = H.XSym


-- | Convert a Core Tetra module to Shimmer code.
smrOfTetraModule
        :: E.DataDefs E.Name                            -- ^ Data type declarations.
        -> E.KindEnv  E.Name                            -- ^ Kind environment.
        -> E.TypeEnv  E.Name                            -- ^ Type environment
        -> E.Module  (E.AnTEC a E.Name) E.Name          -- ^ Tetra module.
        -> Either String (H.Module H.Name H.Name)       -- ^ Shimmer module.

smrOfTetraModule _defs _kenv _tenv mm
 = let
        xsBind  = case E.moduleBody mm of
                        E.XLet _ (E.LRec bxs) _
                          -> map convertDecl bxs
                        _ -> []

   in   Right $ H.Module xsBind


-- | Convert a term declaration to shimmer.
convertDecl 
        :: (E.Bind E.Name, E.Exp (E.AnTEC a E.Name) E.Name)
        -> H.Decl H.Name H.Name

convertDecl (b, x)
 = H.DTerm 
        (nameOfBind b)
        (sym "ddc-decl-term" @@ convertExp x)


-- Type -------------------------------------------------------------------------------------------
-- | Convert a Type to Shimmer.
convertType :: E.Type E.Name -> HExp
convertType tt
 = case tt of 
        E.TCon tc               -> sym "ddc-type-con"    @@ convertTyCon tc
        E.TVar u                -> sym "ddc-type-var"    @@ convertBound u
        E.TAbs b t              -> sym "ddc-type-abs"    @@ convertBind  b  @@ convertType t
        E.TApp t1 t2            -> convertType t1        @@ convertType  t2
        E.TForall b1 t2         -> sym "ddc-type-forall" @@ convertBind  b1 @@ convertType t2
        E.TSum _ts              -> sym "ddc-type-sum"


-- | Convert a TyCon to Shimmer.
convertTyCon :: E.TyCon E.Name -> HExp
convertTyCon tc
 = case tc of
        E.TyConSort{}           -> sym "ddc-fixme"
        E.TyConKind{}           -> sym "ddc-fixme"
        E.TyConWitness{}        -> sym "ddc-fixme"
        E.TyConSpec  tcs        -> sym "ddc-tycon-tccon" @@ convertTcCon tcs
        E.TyConBound u _        -> sym "ddc-tycon-bound" @@ convertBound u
        E.TyConExists{}         -> sym "ddc-fixme"


-- | Convert a TcCon to Shimmer.
convertTcCon :: E.TcCon -> HExp
convertTcCon tc
 = case tc of
        E.TcConUnit             -> sym "ddc-tccon-unit"
        E.TcConFunExplicit      -> sym "ddc-tccon-fun"
        E.TcConFunImplicit      -> sym "ddc-tccon-implicit"
        E.TcConSusp             -> sym "ddc-tccon-susp"
        E.TcConRecord  _        -> sym "ddc-tccon-record"
        E.TcConRead             -> sym "ddc-tccon-read"
        E.TcConHeadRead         -> sym "ddc-tccon-headread"
        E.TcConDeepRead         -> sym "ddc-tccon-deepread"
        E.TcConWrite            -> sym "ddc-tccon-write"
        E.TcConDeepWrite        -> sym "ddc-tccon-deepwrite"
        E.TcConAlloc            -> sym "ddc-tccon-alloc"
        E.TcConDeepAlloc        -> sym "ddc-tccon-deepalloc"        


-- Exp ---------------------------------------------------------------------------------------------
-- | Convert an expression to Shimmer.
convertExp :: E.Exp (E.AnTEC a E.Name) E.Name -> HExp
convertExp xx
 = case xx of
        E.XPrim  _ _            -> sym "ddc-exp-prim"
        E.XCon   _ _            -> sym "ddc-exp-con"

        E.XVar   _ u            -> sym "ddc-exp-var" @@ convertBound u

        E.XAbs   _ p x          -> sym "ddc-exp-abs" @@ convertParam p @@ convertExp x
        E.XApp   _ x1 a2        -> convertExp x1     @@ convertArg a2

        E.XLet   _ _ _          -> sym "ddc-exp-let"
        E.XCase  _ _ _          -> sym "ddc-exp-case"
        E.XCast  _ _ _          -> sym "ddc-exp-cast"


-- | Convert a Param to Shimmer.
convertParam :: E.Param E.Name -> HExp
convertParam pp
 = case pp of
        E.MType _               -> sym "ddc-param-type"
        E.MTerm b               -> sym "ddc-param-term" @@ convertBind b
        E.MImplicit _           -> sym "ddc-param-implicit"


-- | Convert an Arg to Shimmer.
convertArg   :: E.Arg (E.AnTEC a E.Name) E.Name -> HExp
convertArg aa
 = case aa of
        E.RType t               -> sym "ddc-arg-type"     @@ convertType t
        E.RTerm x               -> sym "ddc-arg-term"     @@ convertExp  x
        E.RWitness  _           -> sym "ddc-arg-witness"
        E.RImplicit a           -> sym "ddc-arg-implicit" @@ convertArg  a


-- Bound / Bind -----------------------------------------------------------------------------------
-- | Convert a binding to Shimmer.
convertBind :: E.Bind E.Name -> HExp
convertBind bb
 = case bb of
        E.BName n t             -> sym "ddc-bind-name"   @@ convertName n @@ convertType t

        _                       -> error "nameOfBind: none"

-- | Convert a bound occurrence to Shimmer.
convertBound :: E.Bound E.Name -> HExp
convertBound uu
 = case uu of
        E.UName n               -> sym "ddc-bound-name" @@ convertName n
        E.UPrim n _             -> sym "ddc-bound-prim" @@ convertName n
        _                       -> sym "ddc-bound"


-- Name -------------------------------------------------------------------------------------------
-- | Convert a Tetra name to Shimmer.
convertName :: E.Name -> HExp
convertName name
 = case name of
        E.NameVar n             -> sym "ddc-name-var" @@ sym (T.pack $ cleanName n)
        E.NameCon n             -> sym "ddc-name-con" @@ sym (T.pack $ cleanName n)
        E.NameExt n s           -> sym "ddc-name-ext" @@ convertName n @@ sym (T.pack $ cleanName s)
        E.NameTyConTetra _      -> sym "ddc-name-tetra-tycon"
        E.NameDaConTetra _      -> sym "ddc-name-tetra-dacon"
        E.NameOpError _ _       -> sym "ddc-name-op-error"
        E.NameOpFun   _         -> sym "ddc-name-op-fun"
        E.NameOpVector _ _      -> sym "ddc-name-op-vector"
        E.NamePrimTyCon  _      -> sym "ddc-name-prim-tycon"
        E.NamePrimArith  _ _    -> sym "ddc-name-prim-arith"
        E.NamePrimCast   _ _    -> sym "ddc-name-prim-cast"
        E.NameLitBool _         -> sym "ddc-name-lit-bool"
        E.NameLitNat  _         -> sym "ddc-name-lit-nat"
        E.NameLitInt  _         -> sym "ddc-name-lit-int"
        E.NameLitSize _         -> sym "ddc-name-lit-size"
        E.NameLitWord _ _       -> sym "ddc-name-lit-word"
        E.NameLitFloat _ _      -> sym "ddc-name-lit-float"
        E.NameLitChar _         -> sym "ddc-name-lit-char"
        E.NameLitTextLit _      -> sym "ddc-name-lit-text"
        E.NameLitUnboxed _      -> sym "ddc-name-lit-unboxed"
        E.NameHole              -> sym "ddc-name-hole"


-------------------------------------------------------------------------------
nameOfBind  :: E.Bind E.Name -> H.Name
nameOfBind bb
 = case bb of
        E.BName (E.NameVar n) _ -> T.pack n
        _                       -> error "nameOfBind: none"


cleanName :: String -> String
cleanName []            = []
cleanName ('$' : ss)    = '-' : cleanName ss
cleanName (c : ss)      = c : cleanName ss
