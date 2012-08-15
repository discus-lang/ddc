
module DDC.Core.Lite.Convert.Type
        ( convertT
        , convertPrimT

        , convertDC
        , convertB
        , convertU

        , convertBindNameM
        , convertBoundNameM)
where
import DDC.Core.Lite.Convert.Base
import DDC.Core.Exp
import DDC.Type.Env
import DDC.Type.Compounds
import DDC.Type.Predicates
import DDC.Type.Check.Monad              (throw)
import qualified DDC.Core.Lite.Name      as L
import qualified DDC.Core.Salt.Name      as O
import qualified DDC.Core.Salt.Compounds as O
import qualified DDC.Core.Salt.Runtime   as O
import qualified DDC.Type.Env            as Env
import Control.Monad


convertT     :: KindEnv L.Name -> Type L.Name -> ConvertM a (Type O.Name)
convertT     = convertT' False

convertPrimT :: Type L.Name -> ConvertM a (Type O.Name)
convertPrimT = convertT' True Env.empty


-- Type -----------------------------------------------------------------------
-- | Convert the type of a user-defined function or argument.
--
--   The types of primops have quantifiers that quantify over unboxed types.
--     For example:  @add# :: [a : *]. a -> a -> a@
--   When converting these types we need to keep the quantifier, as well 
--   as the named type variable.
--
--   When converting user types, we instead strip quantifiers and replace 
--   type variables by a generic pointer type:

--   @
--        head :: [r : %]. [a : *]. List r a -> a
--    =>  head :: [r : %]. Ptr# r Obj -> Ptr# _ Obj
--   @
--
--   We don't know what the primary region of the returned value is,
--   so fill in its region varible with a hole '_'. 
--
--   TODO: Convert data type quantifiers into region quantifiers instead,
--   and pass around the correct head region: 
--       @head :: [r1 r2 : %]. Ptr# r1 Obj -> Ptr# r2 Obj@
--
convertT' 
        :: Bool                 -- ^ Whether this is the type of a primop.
        -> KindEnv  L.Name      -- ^ Kind environment.
        -> Type L.Name          -- ^ Type to convert.
        -> ConvertM a (Type O.Name)

convertT' isPrimType kenv tt
 = let down = convertT' isPrimType kenv
   in case tt of
        -- Convert type variables and constructors.
        TVar u
         -> case Env.lookup u kenv of
             Just t
              | isRegionKind t || isDataKind   t
              -> liftM TVar $ convertU u

              | otherwise
              -> error $ "convertT': unexpected var kind " ++ show tt

             Nothing 
              -> error $ "convertT': type var not in kind environment " ++ show tt


        -- Convert unapplied type constructors.
        TCon tc 
         -> convertTyCon tc

        -- Strip off foralls, as the Salt fragment doesn't care about quantifiers.
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
         |  Just (t1, _, _, t2)  <- takeTFun tt
         -> liftM2 tFunPE (down t1) (down t2)

         -- Witness application are passed through to Salt.
         |  Just (tc@(TyConWitness _), args) <- takeTyConApps tt
         -> liftM2 tApps (convertTyCon tc) (mapM down args)
         
         -- Boxed data values are represented in generic form.
         |  Just (_, args) <- takeTyConApps tt
         -> do r <- down $ head args                                                    -- TODO: don't use 'head' function
               return $ O.tPtr r O.tObj
         
         | otherwise
         -> throw $ ErrorMalformed "Bad type-type application."

        -- We shouldn't find any TSums.
        TSum{}          
         | isBot tt     -> throw $ ErrorBotAnnot
         | otherwise    -> throw $ ErrorUnexpectedSum


-- | Convert a simple type constructor to a Salt type.
convertTyCon :: TyCon L.Name -> ConvertM a (Type O.Name)
convertTyCon tc
 = case tc of
        -- Higher universe constructors are passed through unharmed.
        TyConSort    c           -> return $ TCon $ TyConSort    c 
        TyConKind    c           -> return $ TCon $ TyConKind    c 
        TyConWitness c           -> return $ TCon $ TyConWitness c 

        -- Handle baked-in unit and function constructors.
        TyConSpec    TcConFun    -> return $ TCon $ TyConSpec TcConFun
        TyConSpec    TcConUnit   -> return $ O.tPtr O.rTop O.tObj

        -- Convert primitive unboxed TyCons to Salt form.
        TyConBound   (UPrim n _)  _
         ->     convertTyConPrim n

        -- Boxed data values are represented in generic form.
        _ -> error "Lite.Convert.convertTyCon: sorry"


-- | Convert a primitive type constructor to Salt form.
convertTyConPrim :: L.Name -> ConvertM a (Type O.Name)
convertTyConPrim n
 = case n of
        L.NamePrimTyCon pc      
          -> return $ TCon $ TyConBound (UPrim (O.NamePrimTyCon pc) kData) kData
        _ -> error $ "convertTyConPrim: unknown prim name " ++ show n


-- Names ----------------------------------------------------------------------
convertDC 
        :: KindEnv L.Name 
        -> DaCon L.Name 
        -> ConvertM a (DaCon O.Name)

convertDC kenv dc
 = case daConName dc of
        DaConUnit
         -> error "DDC.Core.Lite.convertDC: not converting unit DaConName"

        DaConNamed n
         -> do  n'      <- convertBoundNameM n
                t'      <- convertT kenv (daConType dc)
                return  $ DaCon
                        { daConName             = DaConNamed n'
                        , daConType             = t'
                        , daConIsAlgebraic      = daConIsAlgebraic dc }


convertB :: KindEnv L.Name -> Bind L.Name -> ConvertM a (Bind O.Name)
convertB kenv bb
  = case bb of
        BNone t         -> liftM  BNone (convertT kenv t)        
        BAnon t         -> liftM  BAnon (convertT kenv t)
        BName n t       -> liftM2 BName (convertBindNameM n) (convertT kenv t)


convertU :: Bound L.Name -> ConvertM a (Bound O.Name)
convertU uu
  = case uu of
        UIx i           -> liftM  UIx   (return i)
        UName n         -> liftM  UName (convertBoundNameM n)
        UPrim n t       -> liftM2 UPrim (convertBoundNameM n) (convertPrimT t)


convertBindNameM :: L.Name -> ConvertM a O.Name
convertBindNameM nn
 = case nn of
        L.NameVar str   -> return $ O.NameVar str
        _               -> throw $ ErrorInvalidBinder nn


convertBoundNameM :: L.Name -> ConvertM a O.Name
convertBoundNameM nn
 = case nn of
        L.NameVar str           -> return $ O.NameVar str
        L.NamePrimOp op         -> return $ O.NamePrim (O.PrimOp op)
        L.NameBool val          -> return $ O.NameBool val
        L.NameNat  val          -> return $ O.NameNat  val
        L.NameInt  val          -> return $ O.NameInt  val
        L.NameWord val bits     -> return $ O.NameWord val bits
        _                       -> error $ "toSaltX: convertBoundName"

