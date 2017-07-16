
module DDC.Core.Tetra.Convert.Type.Data
        ( convertDataB
        , convertDataU
        , convertDataT
        , convertDataPrimitiveT)
where
import DDC.Core.Tetra.Convert.Type.Region
import DDC.Core.Tetra.Convert.Type.Base
import DDC.Core.Tetra.Convert.Boxing
import DDC.Core.Tetra.Convert.Error
import DDC.Core.Exp.Annot
import DDC.Type.DataDef
import DDC.Control.Check                        (throw)
import qualified DDC.Core.Tetra.Prim            as E
import qualified DDC.Core.Salt.Compounds        as A
import qualified DDC.Core.Salt.Name             as A
import qualified DDC.Core.Salt.Env              as A
import qualified DDC.Type.Env                   as Env
import qualified Data.Map                       as Map
import qualified Data.Set                       as Set
import Control.Monad
import DDC.Data.Pretty


-- | Convert a value binder with a representable type.
--   This is used for the binders of function arguments, which must have
--   representatable types to adhere to some calling convention.
convertDataB :: Context -> Bind E.Name -> ConvertM a (Bind A.Name)
convertDataB ctx bb
  = case bb of
        BNone t         -> liftM  BNone (convertDataT ctx t)
        BAnon t         -> liftM  BAnon (convertDataT ctx t)
        BName n t       -> liftM2 BName (convertBindNameM n) (convertDataT ctx t)


-- | Convert a value bound.
--   These refer to function arguments or let-bound values,
--   and hence must have representable types.
convertDataU :: Bound E.Name -> ConvertM a (Maybe (Bound A.Name))
convertDataU uu
  = case uu of
        UIx i
         -> return $ Just $ UIx i

        UName n
         -> do  n'      <- convertBindNameM n
                return $ Just $ UName n'

        -- When converting primops, use the type directly specified by the
        -- Salt language instead of converting it from Tetra. The types from
        -- each language definition may not be inter-convertible.
        UPrim n _
         -> case n of
                E.NamePrimArith op True
                  -> return
                  $  Just $ UPrim (A.NamePrimOp (A.PrimArith op))
                                  (A.typeOfPrimArith op)

                E.NamePrimCast op True
                  -> return
                  $  Just $ UPrim (A.NamePrimOp (A.PrimCast  op))
                                  (A.typeOfPrimCast  op)

                _ -> return Nothing


-- | Convert a value type from Core Tetra to Core Salt.
--
--   Value types have kind Data and can be passed to, and returned from
--   functions. Functional types themselves are converted to generic
--   boxed form (Ptr# rTop Obj#)
--
convertDataT :: Context -> Type E.Name -> ConvertM a (Type A.Name)
convertDataT ctx tt
 = case tt of
        -- Convert value type variables and constructors.
        TVar u
         -> case Env.lookup u (contextKindEnv ctx) of
             Just k
              -- Parametric data types are represented as generic objects,
              -- where the region those objects are in is named after the
              -- original type name.
              |  isDataKind k
              -> return $ A.tPtr A.rTop A.tObj

              | otherwise
              -> throw $ ErrorMalformed
                       $ "Invalid value type " ++ (renderIndent $ ppr tt)

             Nothing
              -> throw $ ErrorUnbound u

        -- Convert unapplied type constructors.
        TCon{}    -> convertDataAppT ctx tt

        -- We should not find any type abstractions.
        TAbs{}    -> throw $ ErrorMalformed $ "Invalid type abstraction."

        -- Convert type constructor applications.
        TApp{}    -> convertDataAppT ctx tt

        -- Polymorphic value.
        TForall bParam tBody
         -> let kenv'   = Env.extend bParam (contextKindEnv ctx)
                ctx'    = ctx { contextKindEnv = kenv' }
            in  convertDataT ctx' tBody

        -- Resentable types always have kind Data, but type sums cannot.
        TSum{}    -> throw $ ErrorUnexpectedSum


-- | Convert some data type from Core Tetra to Core Salt.
convertDataAppT :: Context -> Type E.Name -> ConvertM a (Type A.Name)
convertDataAppT ctx tt

        -- Ambient TyCons ---------------------------------
        -- The Unit type.
        | Just (TyConSpec TcConUnit, [])                <- takeTyConApps tt
        =       return $ A.tPtr A.rTop A.tObj

        -- The suspended computation type.
        | Just (TyConSpec TcConSusp, [_tEff, tResult])  <- takeTyConApps tt
        = do   convertDataT ctx tResult

        -- Record types.
        | Just (TyConSpec (TcConRecord ns), tsArgs)     <- takeTyConApps tt
        , length ns == length tsArgs
        = do    return $ A.tPtr A.rTop A.tObj


        -- Primitive TyCons -------------------------------
        -- We don't handle the numeric types here, because they should have
        -- been converted to explicitly unboxed form by the boxing transform.

        -- The Void# type.
        | Just (E.NamePrimTyCon E.PrimTyConVoid,     [])  <- takePrimTyConApps tt
        =      return A.tVoid

        -- The Ptr# type.
        | Just  ( E.NamePrimTyCon E.PrimTyConPtr
                , [_tR, _tA])       <- takePrimTyConApps tt
        = do    return $ A.tPtr A.rTop A.tObj


        -- Numeric TyCons ---------------------------------
        -- These are represented in boxed form.
        | Just (E.NamePrimTyCon n, [])  <- takePrimTyConApps tt
        , True <- case n of
                        E.PrimTyConBool         -> True
                        E.PrimTyConNat          -> True
                        E.PrimTyConInt          -> True
                        E.PrimTyConWord _       -> True
                        E.PrimTyConFloat _      -> True
                        _                       -> False
        =       return $ A.tPtr A.rTop A.tObj


        -- Tetra TyCons -----------------------------------

        -- Explicitly unboxed numeric types.
        -- In Salt, unboxed numeric values are represented directly as
        -- values of the corresponding machine type.
        | Just  ( E.NameTyConTetra E.TyConTetraU
                , [tNum])       <- takePrimTyConApps tt
        , isNumericType tNum
        = do   tNum'   <- convertDataPrimitiveT tNum
               return tNum'

        -- Explicitly unboxed text literals.
        -- These are represented as pointers to static memory.
        | Just  ( E.NameTyConTetra E.TyConTetraU
                , [tStr])       <- takePrimTyConApps tt
        , isTextLitType tStr
        = do    return $ A.tPtr A.rTop (A.tWord 8)

        -- The F# type (reified function)
        | Just  ( E.NameTyConTetra E.TyConTetraF
                , [_])          <- takePrimTyConApps tt
        =       return  $ A.tPtr A.rTop A.tObj

        -- The C# type (reified function)
        | Just  ( E.NameTyConTetra E.TyConTetraC
                , [_])          <- takePrimTyConApps tt
        =       return  $ A.tPtr A.rTop A.tObj

        -- Boxed text literals.
        -- The box holds a pointer to the string data.
        | Just (E.NamePrimTyCon E.PrimTyConTextLit, [])
                <- takePrimTyConApps tt
        =      return   $ A.tPtr A.rTop A.tObj


        -- Boxed functions --------------------------------
        | Just _        <- takeTFun tt
        =       return $ A.tPtr A.rTop A.tObj


        -- Boxed vectors of unboxed values-----------------
        | Just  ( E.NameTyConTetra E.TyConTetraVector
                , [_, _])      <- takePrimTyConApps tt
        =       return $ A.tPtr A.rTop A.tObj


        -- Foreign boxed data types -----------------------
        --   If these have a primary region then we use that,
        --   otherwise they are represnted in generic boxed form.
        | Just (TyConBound (UName n) _, args) <- takeTyConApps tt
        , Set.member n (contextForeignBoxedTypeCtors ctx)
        = case args of
                tR : _
                 | TVar u       <- tR
                 , Just k       <- Env.lookup u (contextKindEnv ctx)
                 , isRegionKind k
                 -> do  tR'     <- convertRegionT ctx tR
                        return  $ A.tPtr tR' A.tObj

                _ ->    return  $ A.tPtr A.rTop A.tObj


        -- User defined TyCons ----------------------------
        -- A user-defined data type with a primary region.
        --   These are converted to generic boxed objects in the same region.
        | Just (TyConBound (UName n) _, tR : _args) <- takeTyConApps tt
        , Map.member n (dataDefsTypes $ contextDataDefs ctx)
        , TVar u       <- tR
        , Just k       <- Env.lookup u (contextKindEnv ctx)
        , isRegionKind k
        = do   tR'     <- convertRegionT ctx tR
               return  $ A.tPtr tR' A.tObj

        -- A user-defined data type without a primary region.
        --   These are converted to generic boxed objects in the top-level region.
        | Just (TyConBound (UName n) _, _) <- takeTyConApps tt
        , Map.member n (dataDefsTypes $ contextDataDefs ctx)
        = do   return  $ A.tPtr A.rTop A.tObj

        -- ISSUE #377: Look through type equations during Tetra to Salt transform.
        | Just (TyConBound (UName n) _, []) <- takeTyConApps tt
        , Just t' <- Map.lookup n (contextTypeEqns ctx)
        = convertDataAppT ctx t'

        -- Application of an abstract type constructor like (m a)
        -- From the type we can't know what region the object it allocated into.
        | TVar _ : _    <- takeTApps tt
        = do    return  $ A.tPtr A.rTop A.tObj

        | otherwise
        = throw   $ ErrorMalformed
                  $  "Invalid type constructor application "
                  ++ (renderIndent $ ppr tt)


-- | Convert a primitive type directly to its Salt form.
convertDataPrimitiveT :: Type E.Name -> ConvertM a (Type A.Name)
convertDataPrimitiveT tt
 | Just (E.NamePrimTyCon n, [])  <- takePrimTyConApps tt
 = case n of
        E.PrimTyConBool         -> return $ A.tBool
        E.PrimTyConNat          -> return $ A.tNat
        E.PrimTyConInt          -> return $ A.tInt
        E.PrimTyConSize         -> return $ A.tSize
        E.PrimTyConWord  bits   -> return $ A.tWord bits
        E.PrimTyConFloat bits   -> return $ A.tFloat bits

        E.PrimTyConTextLit      -> return $ A.tTextLit

        _ -> throw $ ErrorMalformed $ "Invalid primitive type " ++ (renderIndent $ ppr tt)

 | otherwise
 = throw $  ErrorMalformed $  "Invalid primitive type " ++ (renderIndent $ ppr tt)

