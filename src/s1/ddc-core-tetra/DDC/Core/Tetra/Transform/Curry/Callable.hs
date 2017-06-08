
module DDC.Core.Tetra.Transform.Curry.Callable
        ( Callable       (..)
        , CallableSource (..)
        , typeOfCallable
        , consOfCallable
        , takeCallablesOfModule
        , takeCallableFromImport
        , takeCallableFromSuper)
where
import DDC.Core.Tetra.Transform.Curry.Error
import DDC.Core.Module
import DDC.Core.Exp
import DDC.Core.Exp.Annot.AnTEC
import Control.Monad
import Data.Maybe
import Data.Map                         (Map)
import qualified DDC.Core.Call          as Call
import qualified DDC.Core.Tetra.Prim    as E
import qualified Data.Map               as Map


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
        -> Either Error (Map E.Name Callable)

takeCallablesOfModule mm
 = do
        -- Get callables from imported things.
        nsCallableImport
                <- liftM catMaybes
                $  mapM (uncurry takeCallableFromImport)
                $  moduleImportValues mm

        -- Get callable top-level supers.
        nsCallableSuperLocal
                <- mapM (uncurry takeCallableFromSuper)
                $  mapTopBinds (\b x -> (b, x)) mm

        return  $ Map.fromList $ nsCallableSuperLocal ++ nsCallableImport


-- | Take a `Callable` from an `ImportValue`, or Nothing if there isn't one.
takeCallableFromImport
        :: E.Name                               -- ^ Name of the imported thing.
        -> ImportValue E.Name (Type E.Name)     -- ^ Import definition.
        -> Either Error (Maybe (E.Name, Callable))

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
 -- ISSUE #348: Restrict types of things that can be foreign imported.
 --    The parameter and result type of imported functions should have
 --    primitive type only, but we don't check this fact. We should also 
 --    check that each imported function has the standard call pattern.
 --
 | ImportValueSea _ ty  <- im
 = let  cs      = Call.takeCallConsFromType ty
   in   return $ Just (n, Callable CallableImportSea ty cs)

 | otherwise
 = return Nothing


-- | Take the standard call pattern from the body of a super combinator.
takeCallableFromSuper 
        :: Bind E.Name 
        -> Exp a E.Name 
        -> Either Error (E.Name, Callable)

takeCallableFromSuper (BName n t) xx
 = do   let cs     =  Call.takeCallConsFromExp xx
        return $ (n, Callable CallableSuperLocal t cs)

takeCallableFromSuper b _
 =      Left $ ErrorSuperUnnamed b

