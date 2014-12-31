
module DDC.Source.Tetra.Transform.Guards
        ( xCaseOfGuards )
where
import DDC.Source.Tetra.Exp
import DDC.Source.Tetra.Prim

-- | Deguar some guards to a case-expression.
xCaseOfGuards
        :: Show a
        => a 
        -> [GuardedExp a Name]
        -> Exp a Name

xCaseOfGuards a gs 
 = go gs
 where
        go [GExp x1]
         = x1

        go (GGuard (GPred g1) (GExp x1) : ggs)
         = XCase a g1
                [ AAlt pTrue    [GExp x1]
                , AAlt PDefault [GExp (go ggs)]]

        go (GGuard GDefault   (GExp x1)  : _)
         = x1

        go _    = error $ "ddc-source-tetra: bad alts" 
                        ++ show gs
        

pTrue = (PData (DaConPrim (NameLitBool True) tBool) [])
