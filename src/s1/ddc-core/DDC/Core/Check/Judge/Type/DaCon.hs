{-# OPTIONS_HADDOCK hide #-}
module DDC.Core.Check.Judge.Type.DaCon
        (checkDaConM)
where
import DDC.Core.Check.Judge.Type.Base
import qualified Data.Map       as Map
import Prelude                  as L

-- | Check a data constructor, returning its type.
checkDaConM
        :: (Ord n, Eq n, Show n)
        => Config n
        -> Context n            -- ^ Type checker context.
        -> Exp a n              -- ^ The full expression for error messages.
        -> a                    -- ^ Annotation for error messages.
        -> DaCon n (Type n)     -- ^ Data constructor to check.
        -> CheckM a n (DaCon n (Type n), Type n)

checkDaConM _config ctx xx a dc
 = case dc of
    -- Type type of the unit data constructor is baked-in.
    DaConUnit
     -> return (dc, tUnit)

    DaConRecord ns
     -> return (dc, tForalls (map (const kData) ns)
        $ \tsArg -> tFunOfParamResult tsArg
                 $  tApps (TCon (TyConSpec (TcConRecord ns))) tsArg)

    -- Primitive data constructors need to have a corresponding data type,
    -- but there may be too many constructors to list, like with Int literals.
    --
    -- The mode field in the data type declaration says what to expect.
    --    If the mode is 'Small' the data constructor needs to be listed,
    --    If the mode is 'Large' (like Int) there are too many to enumerate.
    --
    -- The type of the constructor needs to be attached so we can determine
    --  what data type it belongs to.
    DaConPrim nCtor
     | Just tPrim       <- lookupType (UName nCtor) ctx
     , tResult          <- snd $ takeTFunArgResult $ eraseTForalls tPrim
     , Just (TyConBound (UName nType) _)
                        <- fmap fst $ takeTyConApps tResult
     , Just dataType    <- Map.lookup nType $ dataDefsTypes $ contextDataDefs ctx
     -> case dataTypeMode dataType of
         DataModeSingle -> return (dc, tPrim)

         DataModeSmall nsCtors
          | L.elem nCtor nsCtors -> return (dc, tPrim)
          | otherwise            -> throw $ ErrorUndefinedCtor a xx

         DataModeLarge  -> return (dc, tPrim)

     | otherwise        -> throw $ ErrorUndefinedCtor a xx

    -- Bound data constructors are always algebraic and Small, so there needs
    --   to be a data definition that gives the type of the constructor.
    -- FIXME: use the module and type names.
    DaConBound (DaConBoundName _ _ nCtor)
     | Just tPrim       <- lookupType (UName nCtor) ctx
     -> return (DaConPrim nCtor, tPrim)

     | Just ctorDef     <- Map.lookup nCtor (dataDefsCtors $ contextDataDefs ctx)
     -> return (dc, typeOfDataCtor ctorDef)

     | otherwise        -> throw $ ErrorUndefinedCtor a xx

