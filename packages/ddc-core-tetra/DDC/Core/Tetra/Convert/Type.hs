
module DDC.Core.Tetra.Convert.Type
        ( -- * Kind conversion.
          convertK
        
          -- * Type conversion.
        , convertRegionT
        , convertNumericT

        , convertCapabilityT
        , convertCapabilityB

        , convertTypeB
        , convertTypeU

        , convertSuperT
        , convertSuperB

        , convertValueT
        , convertValueB
        , convertValueU

          -- * Data constructor conversion.
        , convertDaCon

          -- * Names
        , convertBindNameM

          -- * Prime regions
        , saltPrimeRegionOfDataType)
where
import DDC.Core.Tetra.Convert.Type.Base
import DDC.Core.Tetra.Convert.Boxing
import DDC.Core.Tetra.Convert.Error
import DDC.Core.Exp
import DDC.Type.Env
import DDC.Type.DataDef
import DDC.Type.Compounds
import DDC.Type.Predicates
import DDC.Control.Monad.Check                  (throw)
import qualified DDC.Core.Tetra.Prim            as E
import qualified DDC.Core.Salt.Env              as A
import qualified DDC.Core.Salt.Name             as A
import qualified DDC.Core.Salt.Compounds        as A
import qualified DDC.Type.Env                   as Env
import qualified Data.Map                       as Map
import qualified Data.Set                       as Set
import Control.Monad
import DDC.Base.Pretty


-- Kind -------------------------------------------------------------------------------------------
-- | Convert a kind from Core Tetra to Core Salt.
convertK :: Kind E.Name -> ConvertM a (Kind A.Name)
convertK kk
        | TCon (TyConKind kc) <- kk
        = return $ TCon (TyConKind kc)

        | otherwise
        = throw $ ErrorMalformed 
                $ "Invalid kind " ++ (renderIndent $ ppr kk)


-- Region Types -----------------------------------------------------------------------------------
-- | Convert a region type to Salt.
convertRegionT :: Context -> Type E.Name -> ConvertM a (Type A.Name)
convertRegionT ctx tt
        | TVar u        <- tt
        , Just k        <- Env.lookup u (contextKindEnv ctx)
        , isRegionKind k
        = return $ A.rTop

        | otherwise
        = throw  $ ErrorMalformed 
                 $ "Invalid region type " ++ (renderIndent $ ppr tt)


-- Capability Types -------------------------------------------------------------------------------
-- | Convert a capability / coeffect type to Salt.
--   Works for Read#, Write#, Alloc#
convertCapabilityT :: Context -> Type E.Name -> ConvertM a (Type A.Name)
convertCapabilityT ctx tt
         | Just (TyConSpec tc, [tR])    <- takeTyConApps tt
         = do   tR'     <- convertRegionT ctx tR
                case tc of
                 TcConRead       -> return $ tRead  tR'
                 TcConWrite      -> return $ tWrite tR'
                 TcConAlloc      -> return $ tAlloc tR'
                 _ -> throw $ ErrorMalformed 
                            $ "Malformed capability type " ++ (renderIndent $ ppr tt)

        | otherwise
        = throw $ ErrorMalformed 
                $ "Malformed capability type " ++ (renderIndent $ ppr tt)


-- Numeric Types ----------------------------------------------------------------------------------
-- | Convert a numeric type directly to its Salt form.
--   Works for Bool#, Nat#, Int#, WordN# and Float#
convertNumericT :: Type E.Name -> ConvertM a (Type A.Name)
convertNumericT tt
        | Just (E.NamePrimTyCon n, [])  <- takePrimTyConApps tt
        = case n of
                E.PrimTyConBool         -> return $ A.tBool
                E.PrimTyConNat          -> return $ A.tNat
                E.PrimTyConInt          -> return $ A.tInt
                E.PrimTyConSize         -> return $ A.tSize
                E.PrimTyConWord  bits   -> return $ A.tWord bits
                E.PrimTyConFloat bits   -> return $ A.tFloat bits
                _ -> throw $ ErrorMalformed 
                           $ "Invalid machine type " ++ (renderIndent $ ppr tt)

        | otherwise
        = throw $ ErrorMalformed 
                $ "Invalid machine type " ++ (renderIndent $ ppr tt)


-- Super Types ------------------------------------------------------------------------------------
-- | Types of supercombinators and constructors.
--      
convertSuperT :: Context -> Type E.Name -> ConvertM a (Type A.Name)
convertSuperT ctx tt
 = case tt of
        -- We pass exising quantifiers of Region variables to the Salt language,
        -- and convert quantifiers of data types to the punned name of
        -- their top-level region.
        TForall b t     

         | isRegionKind (typeOfBind b)
         -> do  let ctx' = extendKindEnv b ctx
                b'      <- convertTypeB  b
                t'      <- convertSuperT ctx' t
                return  $ TForall b' t'

--        This doesn't work with run/box
         | isDataKind   (typeOfBind b)
         , BName (E.NameVar str) _   <- b
         , str'         <- str ++ "$r"
         , b'           <- BName (A.NameVar str') kRegion
         -> do
                let ctx' = extendKindEnv b ctx
                t'      <- convertSuperT ctx' t
                return  $ TForall b' t'

         |  otherwise
         -> do  let ctx' = extendKindEnv b ctx
                convertSuperT ctx' t

        TApp{}
         -- Convert Tetra function types to Salt function types.
         | Just (t1, t2)  <- takeTFun tt
         -> do  t1'       <- convertValueT ctx t1
                t2'       <- convertSuperT ctx t2
                return    $ tFunPE t1' t2'

        -- Other types are just the values.
        _ -> convertValueT ctx tt


-- Data Types -------------------------------------------------------------------------------------
-- | Convert a value type from Core Tetra to Core Salt.
--
--   Value types have kind Data and can be passed to, and returned from
--   functions. Functional types themselves are converted to generic
--   boxed form (Ptr# rTop Obj#)
--
convertValueT :: Context -> Type E.Name -> ConvertM a (Type A.Name)
convertValueT ctx tt
 = case tt of
        -- Convert value type variables and constructors.
        TVar u
         -> case Env.lookup u (contextKindEnv ctx) of
             Just k
              -- Parametric data types are represented as generic objects,   
              -- where the region those objects are in is named after the
              -- original type name.                                    -- TODO: use saltPrimREgion
              | isDataKind k
              -- UName (E.NameVar str)  <- u
              -- str'    <- str ++ "$r"
              -- u'      <- UName (A.NameVar str')
              -- return $ A.tPtr (TVar u') A.tObj
              -> return $ A.tPtr A.rTop A.tObj

              | otherwise    
              -> throw $ ErrorMalformed 
                       $ "Invalid value type " ++ (renderIndent $ ppr tt)

             Nothing 
              -> throw $ ErrorInvalidBound u

        -- We should not find any polymorphic values.
        TForall{} -> throw $ ErrorMalformed
                           $ "Invalid polymorphic value type."

        -- Convert unapplied type constructors.
        TCon{}    -> convertValueAppT ctx tt

        -- Convert type constructor applications.
        TApp{}    -> convertValueAppT ctx tt

        -- Resentable types always have kind Data, but type sums cannot.
        TSum{}    -> throw $ ErrorUnexpectedSum


-- | Convert some value type from Core Tetra to Core Salt.
convertValueAppT :: Context -> Type E.Name -> ConvertM a (Type A.Name)
convertValueAppT ctx tt

        -- Ambient TyCons ---------------------------------
        -- The Unit type.
        | Just (TyConSpec TcConUnit, [])                <- takeTyConApps tt
        =       return $ A.tPtr A.rTop A.tObj

        -- The Suspended Computation type.
        | Just (TyConSpec TcConSusp, [_tEff, tResult])  <- takeTyConApps tt
        = do   convertValueT ctx tResult
        

        -- Primitive TyCons -------------------------------
        -- We don't handle the numeric types here, because they should have
        -- been converted to explicitly unboxed form by the boxing transform.

        -- The Void# type.
        | Just (E.NamePrimTyCon E.PrimTyConVoid,     [])  <- takePrimTyConApps tt
        =      return A.tVoid

        -- The Ptr# type.
        | Just  ( E.NamePrimTyCon E.PrimTyConPtr
                , [tR, tA])       <- takePrimTyConApps tt
        = do    tR'     <- convertRegionT ctx tR       
                tA'     <- convertValueT  ctx tA
                return  $ A.tPtr tR' tA'                -- TODO: wrong, need boxed version.


        -- Numeric TyCons ---------------------------------
        -- These are represented in boxed form.
        | Just (E.NamePrimTyCon n, [])  <- takePrimTyConApps tt
        , True <- case n of
                        E.PrimTyConBool         -> True
                        E.PrimTyConNat          -> True
                        E.PrimTyConInt          -> True
                        E.PrimTyConWord _       -> True
                        _                       -> False
        =       return $ A.tPtr A.rTop A.tObj


        -- Tetra TyCons -----------------------------------

        -- Explicitly unboxed numeric types.
        -- In Salt, unboxed numeric values are represented directly as 
        -- values of the corresponding machine type.
        | Just  ( E.NameTyConTetra E.TyConTetraU
                , [tNum])       <- takePrimTyConApps tt
        , isNumericType tNum
        = do   tNum'   <- convertNumericT tNum
               return tNum'

        -- Explicitly unboxed strings.
        -- These are represented as pointers to immutable, anchored memory.
        | Just  ( E.NameTyConTetra E.TyConTetraU
                , [tStr])       <- takePrimTyConApps tt
        , isStringType tStr
        = do    return $ A.tPtr A.rTop (A.tWord 8)

        -- The F# type (reified function)
        | Just  ( E.NameTyConTetra E.TyConTetraF
                , [_])          <- takePrimTyConApps tt
        =       return  $ A.tPtr A.rTop A.tObj

        -- The C# type (reified function)
        | Just  ( E.NameTyConTetra E.TyConTetraC
                , [_])          <- takePrimTyConApps tt
        =       return  $ A.tPtr A.rTop A.tObj

        -- Boxed strings.
        | Just (E.NameTyConTetra E.TyConTetraString, [])
                <- takePrimTyConApps tt
        =      return   $ A.tPtr A.rTop A.tObj


        -- Boxed functions --------------------------------
        | Just _        <- takeTFun tt
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
        | Just (TyConBound (UName n) _, _)          <- takeTyConApps tt
        , Map.member n (dataDefsTypes $ contextDataDefs ctx)
        = do   return  $ A.tPtr A.rTop A.tObj

        | otherwise
        =      throw   $ ErrorMalformed 
                       $  "Invalid type constructor application "
                       ++ (renderIndent $ ppr tt)


-- Binds ------------------------------------------------------------------------------------------
-- | Convert a type binder.
--   These are formal type parameters.
convertTypeB    :: Bind E.Name -> ConvertM a (Bind A.Name)
convertTypeB bb
 = case bb of
        BNone k         -> liftM  BNone (convertK k)
        BAnon k         -> liftM  BAnon (convertK k)
        BName n k       -> liftM2 BName (convertBindNameM n) (convertK k)


-- | Convert a witness binder.
convertCapabilityB :: Context -> Bind E.Name -> ConvertM a (Bind A.Name)
convertCapabilityB ctx bb
 = case bb of
        BNone t         -> liftM  BNone (convertCapabilityT ctx t)
        BAnon t         -> liftM  BAnon (convertCapabilityT ctx t)
        BName n t       -> liftM2 BName (convertBindNameM n) (convertCapabilityT ctx t)


-- | Convert a super binder to Salt
convertSuperB :: Context -> Bind E.Name -> ConvertM a (Bind A.Name)
convertSuperB ctx bb
  = case bb of
        BNone t         -> liftM  BNone (convertSuperT ctx t)        
        BAnon t         -> liftM  BAnon (convertSuperT ctx t)
        BName n t       -> liftM2 BName (convertBindNameM n) (convertSuperT ctx t)


-- | Convert a value binder with a representable type.
--   This is used for the binders of function arguments, which must have
--   representatable types to adhere to some calling convention. 
convertValueB :: Context -> Bind E.Name -> ConvertM a (Bind A.Name)
convertValueB ctx bb
  = case bb of
        BNone t         -> liftM  BNone (convertValueT ctx t)        
        BAnon t         -> liftM  BAnon (convertValueT ctx t)
        BName n t       -> liftM2 BName (convertBindNameM n) (convertValueT ctx t)



-- | Convert the name of a Bind.
convertBindNameM :: E.Name -> ConvertM a A.Name
convertBindNameM nn
 = case nn of
        E.NameVar str 
          -> return $ A.NameVar str

        E.NameExt n str      
          -> do  n'      <- convertBindNameM n
                 return  $ A.NameExt n' str

        _ -> throw $ ErrorInvalidBinder nn


-- Bounds -----------------------------------------------------------------------------------------
-- | Convert a type bound.
--   These are bound by formal type parametrs.
convertTypeU    :: Bound E.Name -> ConvertM a (Bound A.Name)
convertTypeU uu
 = case uu of
        UIx i                   
         -> return $ UIx i

        UName (E.NameVar str)   
         -> return $ UName (A.NameVar str)

        UPrim (E.NameVar str) k
         -> do  k'      <- convertK k
                return $ UPrim (A.NameVar str) k'

        _ -> throw $ ErrorInvalidBound uu


-- | Convert a value bound.
--   These refer to function arguments or let-bound values, 
--   and hence must have representable types.
convertValueU :: Bound E.Name -> ConvertM a (Bound A.Name)
convertValueU uu
  = case uu of
        UIx i                   
         -> return $ UIx i

        UName n
         -> do  n'      <- convertBindNameM n
                return $ UName n'

        -- When converting primops, use the type directly specified by the 
        -- Salt language instead of converting it from Tetra. The types from
        -- each language definition may not be inter-convertible.
        UPrim n _
         -> case n of
                E.NamePrimArith op      
                  -> return $ UPrim (A.NamePrimOp (A.PrimArith op)) 
                                    (A.typeOfPrimArith op)

                E.NamePrimCast op
                  -> return $ UPrim (A.NamePrimOp (A.PrimCast  op)) 
                                    (A.typeOfPrimCast  op)

                _ -> throw $ ErrorInvalidBound uu


-- DaCon ------------------------------------------------------------------------------------------
-- | Convert a data constructor definition.
convertDaCon :: Context -> DaCon E.Name -> ConvertM a (DaCon A.Name)
convertDaCon ctx dc
 = case dc of
        DaConUnit       
         -> return DaConUnit

        DaConPrim n t
         -> do  n'      <- convertDaConNameM dc n
                t'      <- convertSuperT ctx t
                return  $ DaConPrim
                        { daConName             = n'
                        , daConType             = t' }

        DaConBound n
         -> do  n'      <- convertDaConNameM dc n
                return  $ DaConBound
                        { daConName             = n' }


-- | Convert the name of a data constructor.
convertDaConNameM :: DaCon E.Name -> E.Name -> ConvertM a A.Name
convertDaConNameM dc nn
 = case nn of
        E.NameLitUnboxed (E.NameLitBool val)       
          -> return $ A.NameLitBool val

        E.NameLitUnboxed (E.NameLitNat  val)
          -> return $ A.NameLitNat  val

        E.NameLitUnboxed (E.NameLitInt  val)
          -> return $ A.NameLitInt  val

        E.NameLitUnboxed (E.NameLitWord val bits)
          -> return $ A.NameLitWord val bits

        _ -> throw $ ErrorInvalidDaCon dc


-- Prime Region -----------------------------------------------------------------------------------
-- | Given the type of some data value, determine what prime region to use 
--   for the object in the Salt language. The supplied type must have kind
--   Data, else you'll get a bogus result.
--
--   Boxed data types whose first parameter is a region, by convention that
--   region is the prime one.
--     List r1 a  =>  r1 
--
--   Boxed data types that do not have a region as the first parameter,
--   these are allocated into the top-level region.
--     Unit       => rTop
--     B# Nat#    => rTop
--     
--   Functions are also allocated into the top-level region.
--     a -> b     => rTop
--     forall ... => rTop
--
--   For completely parametric data types we use a region named after the
--   associated type variable.
--     a          => a$r
--
--   For types with an abstract constructor, we currently reject them outright.
--   There's no way to tell what region an object of such a type should be 
--   allocated into. In future we should add a supertype of regions, and treat
--   such objects as belong to the Any region.
--   See [Note: Salt conversion for higher kinded type arguments]
--     c a b      => ** NOTHING **
--   
--   Unboxed and index types don't refer to boxed objects, so they don't have
--   associated prime regions.
--     Nat#       => ** NOTHING **
--     U# Nat#    => ** NOTHING **
--
saltPrimeRegionOfDataType
        :: KindEnv E.Name 
        -> Type E.Name 
        -> ConvertM a (Type A.Name)

saltPrimeRegionOfDataType kenv tt
        -- Boxed data types with an attached primary region variable.
        | TCon _ : TVar u : _   <- takeTApps tt
        , Just k                <- Env.lookup u kenv
        , isRegionKind k
        = do    -- u'      <- convertTypeU u
                return  A.rTop

        -- Boxed data types without an attached primary region variable.
        -- This also covers the function case.
        | TCon _ : _           <- takeTApps tt
        = do    return  A.rTop

        -- Quantified types.
        | TForall{}     <- tt
        = do    return  A.rTop

        -- Completely parametric data types.
        | TVar u        <- tt
        , Just k        <- Env.lookup u kenv
        , isDataKind k
        = do    return  A.rTop

{-      -- TODO: Putting these in separate regions doesn't work with box/run.
        | TVar u        <- tt
        , Just k        <- Env.lookup u kenv
        , isDataKind k
        , UName (E.NameVar str) <- u
        , str'          <- str ++ "$r"
        , u'            <- UName (A.NameVar str')
        = do    return  $ TVar u'
-}

        | otherwise
        = throw $ ErrorMalformed       
                $ "Cannot take prime region from " ++ (renderIndent $ ppr tt)

