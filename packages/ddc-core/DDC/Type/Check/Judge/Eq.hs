
module DDC.Type.Check.Judge.Eq
        (makeEq)
where
import DDC.Type.Check.Config
import DDC.Type.Check.Base


-- | Make two types equivalent to each other,
--   or throw the provided error if this is not possible.
makeEq  :: (Eq n, Ord n, Pretty n)
        => Config n
        -> Context n
        -> Type n
        -> Type n
        -> Error n
        -> CheckM n (Context n)

makeEq config ctx0 tL tR err

 -- EqLSolve
 | Just iL <- takeExists tL
 , not $ isTExists tR
 = do   let Just ctx1   = updateExists [] iL tR ctx0
        return ctx1

 -- EqRSolve
 | Just iR <- takeExists tR
 , not $ isTExists tL
 = do   let Just ctx1   = updateExists [] iR tL ctx0
        return ctx1

 -- EqLReach
 --  Both types are existentials, and the left is bound earlier in the stack.
 --  CAREFUL: The returned location is relative to the top of the stack,
 --           hence we need lL > lR here.
 | Just iL <- takeExists tL,    Just lL <- locationOfExists iL ctx0
 , Just iR <- takeExists tR,    Just lR <- locationOfExists iR ctx0
 , lL > lR
 = do   let Just ctx1   = updateExists [] iR tL ctx0
        return ctx1

 -- EqRReach
 --  Both types are existentials, and the right is bound earlier in the stack.
 --  CAREFUL: The returned location is relative to the top of the stack,
 --           hence we need lR > lL here.
 | Just iL <- takeExists tL,    Just lL <- locationOfExists iL ctx0
 , Just iR <- takeExists tR,    Just lR <- locationOfExists iR ctx0
 , lR > lL
 = do   let Just ctx1   = updateExists [] iL tR ctx0
        return ctx1

 -- EqVar
 | TVar u1      <- tL
 , TVar u2      <- tR
 , u1 == u2
 =      return ctx0

 -- EqCon
 | TCon tc1     <- tL
 , TCon tc2     <- tR
 , equivTyCon tc1 tc2
 =      return ctx0

 -- EqApp
 | TApp tL1 tL2 <- tL
 , TApp tR1 tR2 <- tR
 = do
        ctx1    <- makeEq config ctx0 tL1  tR1  err
        tL2'    <- applyContext ctx1 tL2
        tR2'    <- applyContext ctx1 tR2
        ctx2    <- makeEq config ctx0 tL2' tR2' err

        return ctx2

 -- Error
 | otherwise
 =      throw err

