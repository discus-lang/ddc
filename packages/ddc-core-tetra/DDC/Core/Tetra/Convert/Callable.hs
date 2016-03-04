
module DDC.Core.Tetra.Convert.Callable
        ( Callable       (..)
        , CallableSource (..)
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


-- | Enough information to call a super.
--
--   Callable supers are must use the standard call convention, 
--   with their type parameters, value parameters and boxings in that order.
--
data Callable
        -- | A directly callable super in the current module.
        = Callable
        { callableSource        :: CallableSource
        , callableType          :: Type E.Name
        , callableCons          :: [Call.Cons E.Name] }
        deriving (Show)


-- | The source of a callable super.
data CallableSource
        -- | Callable super is defined in the current module.
        = CallableSuperLocal

        -- | Callable thing is a super 
        | CallableSuperOther

        -- | Callable super is imported from sea land.
        | CallableImportSea
        deriving Show


-- | Take the Tetra type of a callable thing.
typeOfCallable :: Callable -> Type E.Name
typeOfCallable (Callable _ t _)  = t


-- | Take the call constructors from a `Callable`.
consOfCallable :: Callable -> [Call.Cons E.Name]
consOfCallable (Callable _ _ cs) = cs


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
 = case Call.takeStdCallConsFromTypeArity tThing nTypes nValues nBoxes of
        Nothing 
         -> Left   $ ErrorSuperArityMismatch n tThing (nTypes, nValues, nBoxes)

        Just cs 
         -> return $ Just (n, Callable CallableSuperOther tThing cs)

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

   in   return $ Just (n, Callable CallableImportSea ty cs)

 | otherwise
 = return Nothing


-- | Take the standard call pattern from the body of a super combinator.
takeCallableFromSuper :: Bind E.Name -> Exp a E.Name -> Maybe Callable
takeCallableFromSuper b xx
 = do   let cs     =  Call.takeCallCons xx
        return $ Callable CallableSuperLocal (typeOfBind b) cs

