
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
import DDC.Type.Check.Monad             (throw)
import qualified DDC.Core.Lite.Name     as L
import qualified DDC.Core.Salt.Name     as O
import qualified DDC.Core.Salt.Env      as O
import Control.Monad


-- Type -----------------------------------------------------------------------
-- | Convert the type of a user-defined function or argument.
convertT     :: Type L.Name -> ConvertM a (Type O.Name)
convertT        = convertT' True

-- | Convert the type of a primop.
--   With primop types we need to keep quantifiers.
convertPrimT :: Type L.Name -> ConvertM a (Type O.Name)
convertPrimT    = convertT' False

-- TODO: It would be better to decide what type arguments to erase 
--       at the binding point of the function, and pass this information down
--       into the tree while we're doing the main conversion.
convertT' :: Bool -> Type L.Name -> ConvertM a (Type O.Name)
convertT' stripForalls tt
 = let down = convertT' stripForalls
   in case tt of
        -- Convert type variables and constructors.
        TVar u
         --  Boxed objects are represented as a generic ptr to object.
         |  isDataKind (typeOfBound u)
         -> if stripForalls
                then do r <- down $ typeOfBound u
                        return $ O.tPtr r O.tObj
                else liftM TVar (convertU u)

         -- Keep region variables.
         | isRegionKind (typeOfBound u)
         -> liftM TVar $ convertU u

         | otherwise    
         -> error $ "convertT': unexpected var kind" ++ show tt

        -- Convert unapplied type constructors.
        TCon tc 
         -> convertTyCon tc

        -- Strip off foralls, as the Salt fragment doesn't care about quantifiers.
        TForall b t     
         | stripForalls && not (isRegionKind (typeOfBind b))
         -> down t

         | otherwise    
         -> liftM2 TForall (convertB b) (convertPrimT t) 

        TApp{}  
         -- Strip off effect and closure information.
         |  Just (t1, _, _, t2)  <- takeTFun tt
         -> liftM2 tFunPE (down t1) (down t2)

         -- Boxed data values are represented in generic form.
         |  Just (_, args) <- takeTyConApps tt
         -> do r <- down $ head args
               return $ O.tPtr r O.tObj
         
         | otherwise
         -> throw $ ErrorMalformed

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

