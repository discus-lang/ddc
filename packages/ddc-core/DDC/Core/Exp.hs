
-- | Abstract syntax for the Disciple core language.
module DDC.Core.Exp 
        ( module DDC.Type.Exp

          -- * Computation expressions
        , Exp     (..)
        , DaCon   (..)
        , DaConName(..)
        , Cast    (..)
        , Lets    (..)
        , LetMode (..)
        , Alt     (..)
        , Pat     (..)
                        
          -- * Witnesses expressions
        , Witness (..)
        , WiCon   (..)
        , WbCon   (..))
where
import DDC.Core.Exp.Base
import DDC.Core.Exp.NFData      ()
import DDC.Core.DaCon
import DDC.Type.Exp

