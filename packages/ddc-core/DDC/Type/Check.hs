
module DDC.Type.Check
        ( -- * Kinds of Types
          checkType
        , kindOfType
        , kindOfType'

          -- * Kinds of Constructors
        , sortOfKiCon
        , kindOfTwCon
        , kindOfTcCon
        
          -- * Errors
        , Error(..))
where
import DDC.Type.Check.CheckType
import DDC.Type.Check.CheckCon