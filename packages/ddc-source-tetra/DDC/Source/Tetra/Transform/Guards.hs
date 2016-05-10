{-# LANGUAGE TypeFamilies #-}

-- | Desugaring Source Tetra guards to simple case-expressions.
module DDC.Source.Tetra.Transform.Guards
        ( desugarGuards )
where
import DDC.Source.Tetra.Transform.BoundX
import DDC.Source.Tetra.Compounds
import DDC.Source.Tetra.Exp.Annot
import DDC.Type.Exp


-- | Desugar some guards to a case-expression.
--   At runtime, if none of the guards match then run the provided fail action.
desugarGuards
        :: forall a
        .  [GGuardedExp (Annot a)]  -- ^ Guarded expressions to desugar.
        -> GExp (Annot a)           -- ^ Failure action.
        -> GExp (Annot a)

desugarGuards gs0 fail0
 = go gs0 fail0
 where
        -- Desugar list of guarded expressions.
        go [] cont
         = cont

        go [g]   cont
         = go1 g cont

        go (g : gs) cont
         = go1 g (go gs cont)

        -- Desugar single guarded expression.
        go1 (GExp x1) _
         = x1

        go1 (GGuard GDefault   gs) cont
         = go1 gs cont

        -- Simple cases where we can avoid introducing the continuation.
        go1 (GGuard (GPred g1)   (GExp x1)) cont
         = XCase g1
                [ AAlt makePTrue [GExp x1]
                , AAlt PDefault  [GExp cont] ]

        go1 (GGuard (GPat p1 g1) (GExp x1)) cont
         = XCase g1
                [ AAlt p1        [GExp x1]
                , AAlt PDefault  [GExp cont]]

        -- Cases that use a continuation function as a join point.
        -- We need this when desugaring general pattern alternatives,
        -- as each group of guards can be reached from multiple places.
        go1 (GGuard (GPred x1) gs) cont
         = XLet  (LLet (BAnon (tBot kData)) (makeXBox cont))
         $ XCase (liftX 1 x1)
                [ AAlt makePTrue [GExp (go1 (liftX 1 gs) (makeXRun (XVar (UIx 0))))]
                , AAlt PDefault  [GExp                   (makeXRun (XVar (UIx 0))) ]]

        go1 (GGuard (GPat p1 x1) gs) cont
         = XLet  (LLet (BAnon (tBot kData)) (makeXBox cont))
         $ XCase (liftX 1 x1)
                [ AAlt p1        [GExp (go1 (liftX 1 gs) (makeXRun (XVar (UIx 0))))]
                , AAlt PDefault  [GExp                   (makeXRun (XVar (UIx 0))) ]]
        

