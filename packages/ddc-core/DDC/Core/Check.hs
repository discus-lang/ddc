
-- | Type checker for the Disciple Core language.
-- 
--   The functions in this module do not check for language fragment compliance.
--   This needs to be done separately via "DDC.Core.Fragment".
--
module DDC.Core.Check
        ( -- * Configuration
          Config(..)
        , configOfProfile

          -- * Checking Modules
        , checkModule

          -- * Checking Expressions
        , Mode     (..)
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
import DDC.Core.Check.Module
import DDC.Core.Check.Exp
import DDC.Core.Check.Witness
                
