
module DDC.Core.Check.Judge.Type.Witness
        (checkWit)
where
import DDC.Core.Check.Witness
import DDC.Core.Check.Judge.Type.Base
import qualified DDC.Type.Sum           as Sum


checkWit :: Checker a n
checkWit !table !ctx _mode _demand 
        (XWitness a w1)
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
                ctx

checkWit _ _ _ _ _
 = error "ddc-core.checkWit: no match"
