
module DDC.Core.Check
        ( checkExp,     typeOfExp,     typeOfExp'
        , checkWitness, typeOfWitness, typeOfWitness'
        , typeOfWiCon
        , Error(..))
where
import DDC.Core.Check.CheckError
import DDC.Core.Check.CheckExp
import DDC.Core.Check.CheckWitness
