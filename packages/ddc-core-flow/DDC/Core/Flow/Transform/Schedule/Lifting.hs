
module DDC.Core.Flow.Transform.Schedule.Lifting
        ( Lifting (..)
        , ScalarEnv
        , LiftEnv

          -- * Lifting Types
        , liftType
        , liftTypeOfBind
        , liftWorker

          -- * Lowering Types
        , lowerSeriesRate)
where
import DDC.Core.Flow.Transform.Schedule.Error
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
          liftingFactor         :: Int }
        deriving (Eq, Show)


-- | Scalar values in scope.
type ScalarEnv
        = [BindF]

-- | Map original variable to lifted version.
type LiftEnv
        = [(BindF, BindF)]


-- | Try to lift the given type.
liftType :: Lifting -> TypeF -> Maybe TypeF 
liftType l tt
        | liftingFactor l == 1 
        = Just tt

        | elem tt 
                [ tFloat 32, tFloat 64
                , tWord  8,  tWord  16, tWord  32, tWord  64
                , tInt
                , tNat ]

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


-- | Try to lift a first-order worker expression to so it operates on elements
--   of vec type instead of scalars.
liftWorker :: Lifting -> ScalarEnv -> LiftEnv -> ExpF -> Either Error ExpF
liftWorker lifting envScalar envLift xx
 = let down     = liftWorker lifting envScalar envLift
   in  case xx of
        XVar u
         -- Replace vars by their vector version.
         | Just (_, bL) <- find (\(bS', _) -> boundMatchesBind u bS') envLift
         , Just uL      <- takeSubstBoundOfBind bL
         -> Right (XVar uL)

         -- Replicate scalar vars.
         -- ISSUE #328: Element type for rep opretora is hard coded to Float32
         | any (boundMatchesBind u) envScalar
         , nPrim        <- PrimVecRep (liftingFactor lifting)
         , tPrim        <- typePrimVec nPrim
         -> Right $ XApp (XApp  (XVar (UPrim (NamePrimVec nPrim) tPrim))
                                (XType $ tFloat 32))
                         xx

        -- Replicate literals.
        -- ISSUE #328: Element type for rep opretora is hard coded to Float32
        XCon dc
         | DaConPrim (NameLitFloat _ 32) _
                    <- dc
         , nPrim    <- PrimVecRep (liftingFactor lifting)
         , tPrim    <- typePrimVec nPrim
         -> Right $ XApp (XApp (XVar (UPrim (NamePrimVec nPrim) tPrim)) 
                               (XType $ tFloat 32))
                         xx

        -- Replace scalar primops by vector versions.
        XApp (XVar (UPrim (NamePrimArith prim) _)) (XType tElem)
         |  Just prim'  <- liftPrimArithToVec (liftingFactor lifting) prim
         -> Right $ XApp (XVar (UPrim (NamePrimVec prim') (typePrimVec prim')))
                         (XType tElem)


        -- Boiler plate application.
        XApp x1 x2      
         -> do  x1'     <- down x1
                x2'     <- down x2
                return  $  XApp x1' x2'


        _ -> Left (ErrorCannotLiftExp xx)


-- Down -----------------------------------------------------------------------
-- | Lower the rate of a series,
--   to account for lifting of the code that consumes it.
lowerSeriesRate :: Lifting -> TypeF -> Maybe TypeF 
lowerSeriesRate lifting tt
 | Just (NameTyConFlow TyConFlowSeries, [tP, tK, tA])
        <- takePrimTyConApps tt
 , c    <- liftingFactor lifting
 = Just (tSeries tP (tDown c tK) tA)

 | otherwise
 = Nothing

