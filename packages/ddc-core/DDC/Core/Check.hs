
-- | Type checker for the Disciple Core language.
-- 
--   The checking functions in this module do not do language fragment
--   compliance checks. This needs to be done separately via "DDC.Core.Fragment".
module DDC.Core.Check
        ( -- * Configuration
          Config(..)
        , configOfProfile

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
                
