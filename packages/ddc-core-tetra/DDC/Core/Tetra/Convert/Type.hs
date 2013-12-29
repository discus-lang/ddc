
module DDC.Core.Tetra.Convert.Type
        ( convertT
        , convertPrimT

        , convertDC
        , convertB
        , convertU

        , convertBindNameM
        , convertBoundNameM)
where
import DDC.Core.Tetra.Convert.Base
import DDC.Core.Exp
import DDC.Type.Env
import DDC.Type.Compounds
import DDC.Type.Predicates
import DDC.Control.Monad.Check                  (throw)
import qualified DDC.Core.Tetra.Prim            as E
import qualified DDC.Core.Salt.Name             as A
import qualified DDC.Core.Salt.Compounds        as A
import qualified DDC.Core.Salt.Runtime          as A
import qualified DDC.Type.Env                   as Env
import Control.Monad


-- | Convert a type from Tetra to Salt.
convertT     :: KindEnv E.Name -> Type E.Name -> ConvertM a (Type A.Name)
convertT     = convertT' False


-- | Convert the type of a primitive from Tetra to Salt.
--
--   The types of primitives are closed, so we don't need to pass
--   a kind environment as with `convertT`.
convertPrimT :: Type E.Name -> ConvertM a (Type A.Name)
convertPrimT = convertT' True Env.empty


-- Type -----------------------------------------------------------------------
-- | Convert the type of a user-defined function or argument.
--
--   The types of primops have quantifiers that quantify over unboxed types.
--     For example:  @add# :: [a : *]. a -> a -> a@
--   When converting these types we need to keep the quantifier, as well 
--   as the named type variable.
--
convertT' 
        :: Bool                 -- ^ Whether this is the type of a primop.
        -> KindEnv E.Name       -- ^ Kind environment.
        -> Type    E.Name       -- ^ Type to convert.
        -> ConvertM a (Type A.Name)

convertT' isPrimType kenv tt                                    -- TODO: isPrimType is unused.
 = let down = convertT' isPrimType kenv
   in case tt of
        -- Convert type variables and constructors.
        TVar u
         -> case Env.lookup u kenv of
             Just t
              | isRegionKind t || isDataKind   t
              -> liftM TVar $ convertU u

              | otherwise
              -> error $ "convertT': unexpected var kind " ++ show tt           -- TODO: real error

             Nothing 
              -> error $ "convertT': type var not in kind environment " ++ show tt -- TODO: real error


        -- Convert unapplied type constructors.
        TCon tc 
         -> convertTyCon tc

        -- Strip off effect and closure quantifiers.
        -- The Salt fragment doesn't care about these.
        TForall b t     
         |   isRegionKind (typeOfBind b)
          || isDataKind   (typeOfBind b)
         -> let kenv'   = Env.extend b kenv
            in  liftM2 TForall  (convertB  kenv' b) 
                                (convertT' isPrimType kenv' t)

         |  otherwise
         -> let kenv'   = Env.extend b kenv
            in  convertT' isPrimType kenv' t

        -- Convert applications.
        TApp{}  
         -- Strip off effect and closure information.
         |  Just (t1, _, _, t2)                 <- takeTFunEC tt
         -> liftM2 tFunPE (down t1) (down t2)

         -- Witness application are passed through to Salt.
         |  Just (tc@(TyConWitness _), args)    <- takeTyConApps tt
         -> liftM2 tApps (convertTyCon tc) (mapM down args)
         
         -- Convert pointers to primitive values.
         |  Just (tcPtr, [tR, tArg])            <- takeTyConApps tt
         ,  TyConBound (UPrim (E.NamePrimTyCon A.PrimTyConPtr) _) _ 
                                                <- tcPtr
         -> do  tR'     <- down tR
                tArg'   <- down tArg
                return  $ A.tPtr tR' tArg'

         -- Explicitly Boxed numeric types.
         |  Just ( E.NameTyConTetra E.TyConTetraB 
                 , _tNum)                        <- takePrimTyConApps tt
         ->     return  $ A.tPtr A.rTop A.tObj       
                                                                        -- TODO: check tNum numeric
         -- Explicitly Unboxed numeric types.
         |  Just ( E.NameTyConTetra E.TyConTetraU
                 , [tNum])                       <- takePrimTyConApps tt
         -> do  tNum'   <- down tNum
                return tNum'                                            -- TODO: check tNum numeric

         -- Generic data types are represented in boxed form.   
         -- TODO: ensure tR is a region var before using it as one.
         |  Just (_, tR : _args)                <- takeTyConApps tt
         -> do  tR'     <- down tR
                return  $ A.tPtr tR' A.tObj
         
         | otherwise
         -> throw $ ErrorMalformed "Bad type-type application."

        -- We shouldn't find any TSums in the type of data.
        TSum{}          
         | isBot tt     -> throw $ ErrorBotAnnot
         | otherwise    -> throw $ ErrorUnexpectedSum


-- | Convert a simple type constructor to a Salt type.
convertTyCon :: TyCon E.Name -> ConvertM a (Type A.Name)
convertTyCon tc
 = case tc of
        -- Higher universe constructors are passed through unharmed.
        TyConSort    c           -> return $ TCon $ TyConSort    c 
        TyConKind    c           -> return $ TCon $ TyConKind    c 
        TyConWitness c           -> return $ TCon $ TyConWitness c 

        -- Handle baked-in unit and function constructors.
        TyConSpec    TcConFunEC  -> return $ TCon $ TyConSpec TcConFunEC
        TyConSpec    TcConUnit   -> return $ A.tPtr A.rTop A.tObj

        -- Convert primitive unboxed TyCons to Salt form.
        TyConBound   (UPrim n _)  _
         ->     convertTyConPrim n

        -- Boxed data values are represented in generic form.
        _ -> error "convertTyCon: cannot convert type"                          -- TODO: real error


-- | Convert a primitive type constructor to Salt form.
convertTyConPrim :: E.Name -> ConvertM a (Type A.Name)
convertTyConPrim n
 = case n of
        E.NamePrimTyCon pc      
          -> return $ TCon $ TyConBound (UPrim (A.NamePrimTyCon pc) kData) kData
        _ -> error $ "convertTyConPrim: unknown prim name " ++ show n           -- TODO: real error


-- Binds ----------------------------------------------------------------------
-- | Convert a Bind.
convertB :: KindEnv E.Name -> Bind E.Name -> ConvertM a (Bind A.Name)
convertB kenv bb
  = case bb of
        BNone t         -> liftM  BNone (convertT kenv t)        
        BAnon t         -> liftM  BAnon (convertT kenv t)
        BName n t       -> liftM2 BName (convertBindNameM n) (convertT kenv t)


-- | Convert the name of a Bind.
convertBindNameM :: E.Name -> ConvertM a A.Name
convertBindNameM nn
 = case nn of
        E.NameVar str   -> return $ A.NameVar str
        _               -> throw $ ErrorInvalidBinder nn


-- Bounds ---------------------------------------------------------------------
-- | Convert a Bound.
convertU :: Bound E.Name -> ConvertM a (Bound A.Name)
convertU uu
  = case uu of
        UIx i           -> liftM  UIx   (return i)
        UName n         -> liftM  UName (convertBoundNameM uu n)
        UPrim n t       -> liftM2 UPrim (convertBoundNameM uu n) (convertPrimT t)


-- | Convert the name of a Bound.
convertBoundNameM :: Bound E.Name -> E.Name -> ConvertM a A.Name
convertBoundNameM u nn
 = case nn of
        E.NameVar str           -> return $ A.NameVar str
        E.NamePrimArith op      -> return $ A.NamePrimOp (A.PrimArith op)
        E.NamePrimCast  op      -> return $ A.NamePrimOp (A.PrimCast  op)
        _                       -> throw  $ ErrorInvalidBound u


-- DaCon ----------------------------------------------------------------------
-- | Convert a data constructor definition.
convertDC 
        :: KindEnv E.Name 
        -> DaCon   E.Name 
        -> ConvertM a (DaCon A.Name)

convertDC kenv dc
 = case dc of
        DaConUnit       
         -> return DaConUnit

        DaConPrim n t
         -> do  n'      <- convertDaConNameM dc n
                t'      <- convertT kenv t
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
        E.NameLitBool val       -> return $ A.NameLitBool val
        E.NameLitNat  val       -> return $ A.NameLitNat  val
        E.NameLitInt  val       -> return $ A.NameLitInt  val
        E.NameLitWord val bits  -> return $ A.NameLitWord val bits
        _                       -> throw $ ErrorInvalidDaCon dc
