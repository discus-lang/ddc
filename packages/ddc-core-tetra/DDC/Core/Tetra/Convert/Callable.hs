
module DDC.Core.Tetra.Convert.Callable
        ( Callable (..)
        , typeOfCallable
        , consOfCallable
        , takeCallablesOfModule)
where
import DDC.Core.Tetra.Convert.Error
import DDC.Core.Module
import DDC.Core.Compounds
import DDC.Core.Exp
import qualified DDC.Core.Call                  as Call

import DDC.Core.Annot.AnTEC
import Control.Monad
import Data.Maybe
import Data.Map                                 (Map)
import qualified DDC.Core.Tetra.Prim            as E
import qualified Data.Map                       as Map


-- | Enough information to call a top-level super or imported thing.
--
--   Directly callable things are required to be in a normalised format
--   with their type parameters, value parameters and boxings in that order.
--
data Callable
        -- | A directly callable super in the current module.
        = CallableSuperLocal
        { callableType          :: Type E.Name
        , callableCons          :: [Call.Cons E.Name]
        , callableTypeArgs      :: Int          -- TODO: ditch arity numbers in favour of cons.
        , callableValueArgs     :: Int
        , callableBoxes         :: Int  }


        -- | A directly callable super in a different module.
        | CallableSuperOther
        { callableType          :: Type E.Name
        , callableCons          :: [Call.Cons E.Name] 
        , callableTypeArgs      :: Int          -- TODO: ditch arity numbers in favour of cons.
        , callableValueArgs     :: Int
        , callableBoxes         :: Int  }

        -- | A function imported from Sea land.
        | CallableImportSea
        { callableType          :: Type E.Name
        , callableCons          :: [Call.Cons E.Name]
        , callableTypeArgs      :: Int          -- TODO: ditch arity numbers in favour of cons.
        , callableValueArgs     :: Int 
        , callableBoxedReturn   :: Int }
        deriving (Show)


-- | Take the Tetra type of a callable thing.
typeOfCallable :: Callable -> Type E.Name
typeOfCallable cc
 = case cc of
        CallableSuperLocal t _ _ _ _    -> t
        CallableSuperOther t _ _ _ _    -> t
        CallableImportSea  t _ _ _ _    -> t


-- | Take the call constructors from a `Callable`.
consOfCallable :: Callable -> [Call.Cons E.Name]
consOfCallable cc
 = case cc of
        CallableSuperLocal _ cs _ _ _     -> cs
        CallableSuperOther _ cs _ _ _     -> cs
        CallableImportSea  _ cs _ _ _     -> cs


-- Get callable things from the current module.
takeCallablesOfModule
        :: Module (AnTEC a E.Name) E.Name
        -> Either (Error a) (Map E.Name Callable)

takeCallablesOfModule mm
 = do
        -- Get callable local supers.
        let checkCallable (BName n _) (Just callable)
                = return (n, callable)
            checkCallable b Nothing = Left $ ErrorSuperNotPrenex b
            checkCallable b _       = Left $ ErrorSuperUnnamed   b

        nsCallableSuperLocal
                <- mapM (uncurry checkCallable)
                $  mapTopBinds (\b x -> (b, takeCallableFromSuper b x)) mm

        nsCallableImport
                <- liftM catMaybes
                $  mapM (uncurry takeCallableFromImport)
                $  moduleImportValues mm

        return  $ Map.fromList $ nsCallableSuperLocal ++ nsCallableImport


-- | Take a callable from an `ImportValue`, or Nothing if there isn't one.
takeCallableFromImport
        :: E.Name               -- ^ Name of the imported thing.
        -> ImportValue E.Name   -- ^ Import definition.
        -> Either (Error a) (Maybe (E.Name, Callable))

takeCallableFromImport n im

 -- A thing imported from some other module.
 --  We determine the call pattern from its type and arity information, 
 --  which comes in the interface file. We need the arity information
 --  because the super may return a functional value, which we cannot 
 --  direct from its logical type alone.
 | ImportValueModule _ _ tThing (Just arity) <- im
 , (nTypes, nValues, nBoxes)                 <- arity
 = let rCons = Call.takeStdCallConsFromTypeArity tThing nTypes nValues nBoxes
   in  case rCons of
        Nothing -> error "type/arity mismatch"  -- TODO: lift to either
        Just cs -> return $ Just (n, CallableSuperOther tThing cs nTypes nValues nBoxes)

 -- A thing imported from sea land.
 --  We determine the call pattern directly from the type.
 --  Things imported from Sea land do not return functional values, 
 --  so every parameter in the type is a real parameter in the call pattern.
 --
 -- TODO: Lock down the type of things that can be imported.
 --       We can only exchange primitives and abstract data values,
 --       not algebraic data that we contruct on the Tetra side.
 | ImportValueSea _ ty  <- im
 = let
        -- TODO: shift the call pattern split into DDC.Core.Call.
        (bsTy,  ty')     = fromMaybe ([], ty) $ takeTForalls ty
        (tsArg, tResult) = takeTFunArgResult ty'

        isSuspReturn
         = case takeTyConApps tResult of
                Just (TyConSpec TcConSusp, _) -> True
                _                             -> False

        csType  = map Call.ConsType  bsTy
        csValue = map Call.ConsValue tsArg
        csBoxes = if isSuspReturn then [Call.ConsBox] else []
        cs      = csType ++ csValue ++ csBoxes

        nType   = length csType
        nValue  = length csValue
        nBoxes  = length csBoxes

   in   return $ Just (n, CallableImportSea ty cs nType nValue nBoxes)

 | otherwise
 = return Nothing


-- | Take the standard call pattern from the body of a super combinator.
takeCallableFromSuper :: Bind E.Name -> Exp a E.Name -> Maybe Callable
takeCallableFromSuper b xx
 = do   
        -- Take the complete call pattern of the super.
        let cs     =  Call.takeCallCons xx

        -- Split the pattern into the components for a standard call.
        -- If this is not a standard call then this returns Nothing.
        (csType, csValue, csBoxes)
                   <- Call.splitStdCallCons cs

        -- Build numeric arity information.
        let nType  = length csType
        let nValue = length csValue
        let nBoxes = length csBoxes

        return $ CallableSuperLocal (typeOfBind b) cs nType nValue nBoxes

