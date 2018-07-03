
-- | Name resolution.
module DDC.Core.Check.Context.Resolve
        ( TyConThing(..)
        , resolveTyConThing
        , lookupTypeSyn)
where
import DDC.Core.Check.Base
import DDC.Core.Check.Context.Oracle    (TyConThing(..))
import qualified DDC.Core.Check.Context.Oracle  as Oracle
import qualified DDC.Core.Env.EnvT      as EnvT
import qualified DDC.Core.Env.EnvX      as EnvX
import qualified Data.Map.Strict        as Map



-- | Resolve the name of a type constructor or synonym.
--   If we can't find one then throw an error in the `CheckM` monad.
resolveTyConThing
        :: (Ord n, Show n)
        => Context n -> n -> CheckM a n (TyConThing n, Kind n)

resolveTyConThing ctx n
 -- Look for a data type declaration in the current module.
 | dataDefs      <- EnvX.envxDataDefs $ contextEnvX ctx
 , Just dataType <- Map.lookup n (dataDefsTypes dataDefs)
 = return ( TyConThingData n dataType
          , kindOfDataType dataType)

 -- Look for a data type or synonym declaration in an imported module.
 | Just oracle  <- contextOracle ctx
 =   Oracle.resolveTyConThing oracle n
 >>= \case
        Nothing    -> throw  $ ErrorType $ ErrorTypeUndefinedTypeCtor (UName n)
        Just thing -> return $ (thing, Oracle.kindOfTyConThing thing)

 | otherwise
 = error "no oracle" -- throw $ ErrorType $



-- | Lookup the definition of a type synonym.
--   If we can't fine one then return Nothing.
lookupTypeSyn
        :: (Ord n, Show n)
        => Context n -> n -> CheckM a n (Maybe (Type n))

lookupTypeSyn ctx n
 -- Look for synonyom in the current module.
 | Just tR  <- Map.lookup n $ EnvT.envtEquations $ contextEnvT ctx
 = return $ Just tR

 -- Look for synonym in imported modules.
 | Just oracle <- contextOracle ctx
 = do   mThing  <- Oracle.resolveTyConThing oracle n
        case mThing of
         Just (TyConThingSyn _ _ t) -> return $ Just t
         Just _  -> return Nothing
         Nothing -> return Nothing

 -- We can't find one.
 | otherwise
 = return Nothing









{- This was from the old kind judgment.

             -- The kinds of abstract imported type constructors are in the
             -- global kind environment.
             | Just k'          <- EnvT.lookupName n (contextEnvT ctx0)
             , UniverseSpec     <- uni
             -> return (TCon (TyConBound u k'), k')

             -- User defined data type constructors must be in the set of
             -- data defs. Attach the real kind why we're here.
             | Just def         <- Map.lookup n $ dataDefsTypes
                                                $ EnvX.envxDataDefs
                                                $ contextEnvX ctx0
             , UniverseSpec     <- uni
             -> let k'   = kindOfDataType def
                in  return (TCon (TyConBound u k'), k')

             -- For type synonyms, just re-check the right of the binding.
             | Just t'          <- Map.lookup n $ EnvT.envtEquations
                                                $ contextEnvT ctx0
             -> do  (tt', k', _) <- checkTypeM config ctx0 uni t' mode
                    return (tt', k')

             -- We don't have a type for this constructor.
             |  otherwise
             -> throw $ C.ErrorType $ ErrorTypeUndefinedTypeCtor u
-}
