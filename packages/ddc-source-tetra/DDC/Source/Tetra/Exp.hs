
-- | Definition of Source Tetra Expressions.
module DDC.Source.Tetra.Exp
        ( module DDC.Type.Exp

        -- * Expressions
        , Exp           (..)
        , Lets          (..)
        , Alt           (..)
        , Pat           (..)
        , Clause        (..)
        , GuardedExp    (..)
        , Guard         (..)
        , Cast          (..)

        -- * Witnesses
        , Witness       (..)

        -- * Data Constructors
        , DaCon         (..)

        -- * Witness Constructors
        , WiCon         (..))
where
import DDC.Type.Exp
import DDC.Source.Tetra.Exp.Base
