
module DDC.Core.Tetra.Convert.Type
        ( -- * Kind conversion.
          convertK
        
          -- * Type conversion.
        , convertRegionT

        , convertCapabilityT
        , convertCapabilityB


        , convertTypeB
        , convertTypeU

        , convertCtorT
        , convertSuperConsT

        , convertDataT
        , convertDataPrimitiveT
        , convertValueB
        , convertValueU

          -- * Data constructor conversion.
        , convertDaCon

          -- * Names
        , convertBindNameM

          -- * Prime regions
        , saltPrimeRegionOfDataType)
where
import DDC.Core.Tetra.Convert.Type.Region
import DDC.Core.Tetra.Convert.Type.DaCon
import DDC.Core.Tetra.Convert.Type.Data
import DDC.Core.Tetra.Convert.Type.Base
import DDC.Core.Tetra.Convert.Error
import DDC.Core.Call
import DDC.Core.Annot.Exp
import DDC.Type.Compounds
import DDC.Type.Predicates
import DDC.Control.Monad.Check                  (throw)
import qualified DDC.Core.Tetra.Prim            as E
import qualified DDC.Core.Salt.Env              as A
import qualified DDC.Core.Salt.Name             as A
import Control.Monad
import DDC.Base.Pretty


-- Binds ------------------------------------------------------------------------------------------
-- | Convert a witness binder.
convertCapabilityB :: Context -> Bind E.Name -> ConvertM a (Bind A.Name)
convertCapabilityB ctx bb
 = case bb of
        BNone t         -> liftM  BNone (convertCapabilityT ctx t)
        BAnon t         -> liftM  BAnon (convertCapabilityT ctx t)
        BName n t       -> liftM2 BName (convertBindNameM n) (convertCapabilityT ctx t)


-- | Convert a value binder with a representable type.
--   This is used for the binders of function arguments, which must have
--   representatable types to adhere to some calling convention. 
convertValueB :: Context -> Bind E.Name -> ConvertM a (Bind A.Name)
convertValueB ctx bb
  = case bb of
        BNone t         -> liftM  BNone (convertDataT ctx t)        
        BAnon t         -> liftM  BAnon (convertDataT ctx t)
        BName n t       -> liftM2 BName (convertBindNameM n) (convertDataT ctx t)



-- Bounds -----------------------------------------------------------------------------------------
-- | Convert a type bound.
--   These are bound by formal type parametrs.
convertTypeU    :: Bound E.Name -> ConvertM a (Bound A.Name)
convertTypeU uu
 = case uu of
        UIx i                   
         -> return $ UIx i

        UName (E.NameVar str)   
         -> return $ UName (A.NameVar str)

        UPrim (E.NameVar str) k
         -> do  k'      <- convertK k
                return $ UPrim (A.NameVar str) k'

        _ -> throw $ ErrorInvalidBound uu


-- | Convert a value bound.
--   These refer to function arguments or let-bound values, 
--   and hence must have representable types.
convertValueU :: Bound E.Name -> ConvertM a (Bound A.Name)
convertValueU uu
  = case uu of
        UIx i                   
         -> return $ UIx i

        UName n
         -> do  n'      <- convertBindNameM n
                return $ UName n'

        -- When converting primops, use the type directly specified by the 
        -- Salt language instead of converting it from Tetra. The types from
        -- each language definition may not be inter-convertible.
        UPrim n _
         -> case n of
                E.NamePrimArith op      
                  -> return $ UPrim (A.NamePrimOp (A.PrimArith op)) 
                                    (A.typeOfPrimArith op)

                E.NamePrimCast op
                  -> return $ UPrim (A.NamePrimOp (A.PrimCast  op)) 
                                    (A.typeOfPrimCast  op)

                _ -> throw $ ErrorInvalidBound uu


-- Capability Types -------------------------------------------------------------------------------
-- | Convert a capability / coeffect type to Salt.
--   Works for Read#, Write#, Alloc#
convertCapabilityT :: Context -> Type E.Name -> ConvertM a (Type A.Name)
convertCapabilityT ctx tt
         | Just (TyConSpec tc, [tR])    <- takeTyConApps tt
         = do   tR'     <- convertRegionT ctx tR
                case tc of
                 TcConRead       -> return $ tRead  tR'
                 TcConWrite      -> return $ tWrite tR'
                 TcConAlloc      -> return $ tAlloc tR'
                 _ -> throw $ ErrorMalformed 
                            $ "Malformed capability type " ++ (renderIndent $ ppr tt)

        | otherwise
        = throw $ ErrorMalformed 
                $ "Malformed capability type " ++ (renderIndent $ ppr tt)


---------------------------------------------------------------------------------------------------
-- | Convert the Tetra type of a super with the given call pattern to Salt.
--
--   This type conversion mirrors the `convertSuperXT` converesion function
--   except that we only know the call pattern of the function, rather than
--   its defining expression.
-- 
convertSuperConsT
        :: Context 
        -> [Cons E.Name]
        -> Type E.Name 
        -> ConvertM a (Type A.Name)

convertSuperConsT ctx0 cs0 tt0
 = convertAbsType ctx0 cs0 tt0
 where
        -- Accepting type abstractions --------------------
        convertAbsType ctx cs tt
         = case cs of
                -- TODO: check kinds.
                ConsType _k : cs'
                  | TForall bParam tBody <- tt
                  -> convertConsType ctx bParam cs' tBody
                _ -> convertAbsValue ctx cs tt

        convertConsType ctx bParam cs tBody
         -- Erase higher kinded type abstractions.
         | Just _       <- takeKFun $ typeOfBind bParam
         = do   let ctx' = extendKindEnv bParam ctx
                convertAbsType ctx' cs tBody

         -- Erase effect abstractions.
         | isEffectKind $ typeOfBind bParam
         = do   let ctx' = extendKindEnv bParam ctx
                convertAbsType ctx' cs tBody

         -- Retain region abstractions.
         | isRegionKind $ typeOfBind bParam
         = do   bParam'  <- convertTypeB bParam
                let ctx' =  extendKindEnv bParam ctx
                tBody'   <- convertAbsType ctx' cs tBody
                return   $  TForall bParam' tBody'

         -- Convert data type abstractions to region abstractions.
         | isDataKind $ typeOfBind bParam
         , BName (E.NameVar str) _ <- bParam
         , str'          <- str ++ "$r"
         , bParam'       <- BName (A.NameVar str') kRegion
         = do   let ctx' =  extendKindEnv bParam ctx
                tBody'   <- convertAbsType ctx' cs tBody
                return   $  TForall bParam' tBody'

         -- Some other type abstraction we can't convert.
         | otherwise
         = error "convertSuperConsT: nope"


        -- Accepting value abstractions -------------------
        convertAbsValue ctx cs tt
         = case cs of
                -- TODO: check types.
                ConsValue tParam : cs'
                  | Just (_tArg, tBody)  <- takeTFun tt
                  -> convertConsValue ctx tParam cs' tBody
                _ -> convertDataT ctx tt


        convertConsValue ctx tParam cs tBody
         = do   tParam'  <- convertDataT   ctx tParam
                tBody'   <- convertAbsValue ctx cs tBody
                return   $  tFun tParam' tBody'


