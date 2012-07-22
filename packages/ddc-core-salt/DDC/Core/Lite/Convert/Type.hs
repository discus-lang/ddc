
module DDC.Core.Lite.Convert.Type
        ( convertT
        , convertPrimT

        , convertB
        , convertU

        , convertBindNameM
        , convertBoundNameM)
where
import DDC.Core.Lite.Convert.Base
import DDC.Core.Exp
import DDC.Type.Compounds
import DDC.Type.Predicates
import DDC.Type.Check.Monad              (throw)
import qualified DDC.Core.Lite.Name      as L
import qualified DDC.Core.Salt.Name      as O
import qualified DDC.Core.Salt.Compounds as O
import Control.Monad


convertT     :: Type L.Name -> ConvertM a (Type O.Name)
convertT     = convertT' False

convertPrimT :: Type L.Name -> ConvertM a (Type O.Name)
convertPrimT = convertT' True


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
        -> Type L.Name          -- ^ Type to convert.
        -> ConvertM a (Type O.Name)

convertT' isPrimType tt
 = let down = convertT' isPrimType
   in case tt of
        -- Convert type variables and constructors.
        TVar u

         |   isRegionKind (typeOfBound u)
          || isDataKind (typeOfBound u)
         -> liftM TVar $ convertU u

         | otherwise    
         -> error $ "convertT': unexpected var kind" ++ show tt

        -- Convert unapplied type constructors.
        TCon tc 
         -> convertTyCon tc

        -- Strip off foralls, as the Salt fragment doesn't care about quantifiers.
        TForall b t     
         |   isRegionKind (typeOfBind b)
          || isDataKind   (typeOfBind b)
         ->  liftM2 TForall (convertB b) (down t)

         |  isRegionKind (typeOfBind b)
         -> liftM2 TForall (convertB b) (down t) 

         |  otherwise
         -> down t

        -- Convert applications.
        TApp{}  
         -- Strip off effect and closure information.
         |  Just (t1, _, _, t2)  <- takeTFun tt
         -> liftM2 tFunPE (down t1) (down t2)

         -- Boxed data values are represented in generic form.
         |  Just (_, args) <- takeTyConApps tt
         -> do r <- down $ head args
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
        TyConSpec    c           -> return $ TCon $ TyConSpec    c 

        -- Primitive boxed zero-arity constructors (like Unit)
        --  are represented in generic form.
        -- TODO: Use a real region variable instead of a hole.
        TyConBound   (UPrim (L.NameDataTyCon L.DataTyConUnit) _)
         ->     return $ O.tPtr (TVar (UHole kRegion)) O.tObj

        -- Convert primitive unboxed TyCons to Salt form.
        TyConBound   (UPrim n _) 
         ->     convertTyConPrim n

        -- Boxed data values are represented in generic form.
        TyConBound   u
         -> do  r <- convertT (typeOfBound u)
                return $ O.tPtr r O.tObj



-- | Convert a primitive type constructor to Salt form.
convertTyConPrim :: L.Name -> ConvertM a (Type O.Name)
convertTyConPrim n
 = case n of
        L.NamePrimTyCon pc      
          -> return $ TCon $ TyConBound (UPrim (O.NamePrimTyCon pc) kData)
        _ -> error $ "convertTyConPrim: unknown prim name " ++ show n


-- Names ----------------------------------------------------------------------
convertB :: Bind L.Name -> ConvertM a (Bind O.Name)
convertB bb
  = case bb of
        BNone t         -> liftM  BNone (convertT t)        
        BAnon t         -> liftM  BAnon (convertT t)
        BName n t       -> liftM2 BName (convertBindNameM n) (convertT t)


convertU :: Bound L.Name -> ConvertM a (Bound O.Name)
convertU uu
  = case uu of
        UIx i t         -> liftM2 UIx   (return i) (convertT t)
        UName n t       -> liftM2 UName (convertBoundNameM n) (convertT t)
        UPrim n t       -> liftM2 UPrim (convertBoundNameM n) (convertPrimT t)

        -- This hole type is a hack that appears as the body of a module.
        -- Make sure it stays a hole and doesn't turn into a Ptr Obj.
        UHole t
         |  TVar (UHole k)             <- t
         ,  TCon (TyConKind KiConData) <- k
         -> return $ UHole (TVar (UHole (TCon (TyConKind KiConData))))

         | otherwise 
         -> error $ "convertU: unexpected hole kind" ++ (show t)


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

