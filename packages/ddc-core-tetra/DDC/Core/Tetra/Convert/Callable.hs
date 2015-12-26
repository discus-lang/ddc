
module DDC.Core.Tetra.Convert.Callable
        ( Callable (..)
        , takeCallablesOfModule)
where
import DDC.Core.Tetra.Convert.Error
import DDC.Core.Module
import DDC.Core.Compounds
import DDC.Core.Exp
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
        { callableTypeArgs      :: Int
        , callableValueArgs     :: Int
        , callableBoxes         :: Int }

        -- | A directly callable super in a different module.
        | CallableSuperOther
        { callableTypeArgs      :: Int
        , callableValueArgs     :: Int
        , callableBoxes         :: Int }

        -- | A function imported from Sea land.
        | CallableImportSea
        { callableTypeArgs      :: Int
        , callableValueArgs     :: Int 
        , callableBoxedReturn   :: Bool }
        deriving (Show)


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
                $  mapTopBinds (\n x -> (n, takeCallableFromSuper x)) mm

        -- Get callable imported things.
        let checkImport n (ImportValueModule _ _ _ (Just (aType, aValue, nBoxes)))
                =    return $ Just (n, CallableSuperOther aType aValue nBoxes)

            -- For things imported from Sea land,
            -- we determine their call patterns from their types.

            -- TODO: Lock down the type of things that can be imported.
            --       We can only exchange primitives and abstract data values,
            --       not algebraic data that we contruct on the Tetra side.

            checkImport n (ImportValueSea _ ty)
                = do let (bsTy,  ty')     = fromMaybe ([], ty) $ takeTForalls ty
                     let (tsArg, tResult) = takeTFunArgResult ty'

                     let isSuspReturn
                           = case takeTyConApps tResult of
                                Just (TyConSpec TcConSusp, _) -> True
                                _                             -> False

                     return $ Just (n, CallableImportSea 
                                        (length bsTy) (length tsArg) isSuspReturn)

            checkImport _ _
                = return $ Nothing

        nsCallableImport
                <- liftM catMaybes
                $  mapM (uncurry checkImport)
                $  moduleImportValues mm

        return  $ Map.fromList $ nsCallableSuperLocal ++ nsCallableImport


-- | Take the call pattern from the body of a super combinator.
takeCallableFromSuper :: Exp a n -> Maybe Callable
takeCallableFromSuper xx
 = case takePrenexCallPattern xx of
        Just (ks, ts, nBoxes)
                -> Just $ CallableSuperLocal (length ks) (length ts) nBoxes
        Nothing -> Nothing

