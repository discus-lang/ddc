
module DDC.Core.Flow.Transform.Schedule.Lifting
        ( Lifting (..)
        , LiftEnv

          -- * Lifting Types
        , liftType
        , liftTypeOfBind
        , liftWorker

          -- * Lowering Types
        , lowerSeriesRate)
where
import DDC.Core.Flow.Transform.Schedule.Fail
import DDC.Core.Flow.Compounds
import DDC.Core.Flow.Exp
import DDC.Core.Flow.Prim
import Control.Monad
import Data.List


-- | Lifting config controls how many elements should be processed 
--   per loop iteration.
data Lifting
        = Lifting
        { -- How many elements to process for each loop iteration.
          liftingFactor         :: Int 

          -- Decide whether this vector operator is implementable.
        , liftingOkPrimVector   :: PrimVector -> TypeF -> Bool }


-- | Map original variable to lifted version.
type LiftEnv
        = [(BindF, BindF)]


-- | Try to lift the given type.
liftType :: Lifting -> TypeF -> Maybe TypeF 
liftType l tt
 | liftingFactor l == 1
 = Just tt

 | Just (NamePrimTyCon (PrimTyConFloat 32), []) 
        <- takePrimTyConApps tt
 = Just (tVec (liftingFactor l) tt)

 | otherwise
 = Nothing


-- | Try to lift the type of a binder.
liftTypeOfBind :: Lifting -> BindF -> Maybe BindF
liftTypeOfBind l b
 = case b of
        BName n t       -> liftM (BName n) (liftType l t)
        BAnon   t       -> liftM BAnon     (liftType l t)
        BNone   t       -> liftM BNone     (liftType l t)


-- | Try to lift a first-order worker expression to work on elements of vector
--   type instead of scalars.
liftWorker :: Lifting -> LiftEnv -> ExpF -> Either Fail ExpF
liftWorker lifting env xx
 = let down     = liftWorker lifting env
   in  case xx of
        XApp (XVar (UPrim (NamePrimArith prim) _)) (XType tElem)
         |  Just prim' <- liftPrimArithToVector (liftingFactor lifting) prim
         -> Right $ XApp (XVar (UPrim (NamePrimVector prim') (typePrimVector prim')))
                         (XType tElem)

        XApp x1 x2      
         -> do  x1'     <- down x1
                x2'     <- down x2
                return  $  XApp x1' x2'

        XVar u
         | Just (_, bL) 
                    <- find (\(bS', _) -> boundMatchesBind u bS') env
         , Just uL  <- takeSubstBoundOfBind bL
         -> Right (XVar uL)

        _ -> error $ "no lift " ++ show xx


-- Down -----------------------------------------------------------------------
-- | Lower the rate of a series,
--   to account for lifting of the code that consumes it.
lowerSeriesRate :: Lifting -> TypeF -> Maybe TypeF 
lowerSeriesRate lifting tt
 | Just (NameTyConFlow TyConFlowSeries, [tK, tA])
        <- takePrimTyConApps tt
 , c    <- liftingFactor lifting
 = Just (tSeries (tDown c tK) tA)

 | otherwise
 = Nothing

