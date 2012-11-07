
-- | Type checker for the Disciple Core language.
module DDC.Core.Check
        ( -- * Configuration
          Config(..)

          -- * Checking Modules
        , checkModule

          -- * Checking Expressions
        , checkExp,     typeOfExp

          -- * Checking Witnesses
        , checkWitness, typeOfWitness
        , typeOfWiCon

          -- * Annotations
        , AnTEC(..)

          -- * Error messages
        , Error(..))
where
import DDC.Core.Check.Error
import DDC.Core.Check.ErrorMessage      ()
import DDC.Core.Check.CheckModule
import DDC.Core.Check.CheckExp
import DDC.Core.Check.CheckWitness
                
