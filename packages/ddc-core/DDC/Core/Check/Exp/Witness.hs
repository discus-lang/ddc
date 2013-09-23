
module DDC.Core.Check.Exp.Witness
        (checkWit)
where
import DDC.Core.Transform.Reannotate
import DDC.Core.Check.Exp.Base
import qualified DDC.Type.Sum   as Sum
import qualified Data.Set       as Set


checkWit :: Checker a n
checkWit !table !ctx (XWitness a w1) _
 = do   let config      = tableConfig table
        let kenv        = tableKindEnv table
        let tenv        = tableTypeEnv table

        -- Check the witness.
        (w1', t1)       <- checkWitnessM config kenv tenv ctx w1
        let w1TEC = reannotate fromAnT w1'

        returnX a
                (\z -> XWitness z w1TEC)
                t1
                (Sum.empty kEffect)
                Set.empty
                ctx

checkWit _ _ _ _
 = error "ddc-core.checkWit: no match"
