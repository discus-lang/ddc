
module DDC.Type.Check.Context
        ( Direction     (..)
        , Context       (..)
        , emptyContext
        , Elem          (..))
where
import DDC.Type.Exp


-- | Direction used for bidirectional type checking.
data Direction n
        -- | Check the type of an expression against this one.
        = Check (Type n)

        -- | Synthesise the type of the expression.
        | Synth
        deriving Show


data Context n
        = Context [Elem n]
        
data Elem n
        = ElemKind (Binder n) (Kind n)
        | ElemType (Binder n) (Type n)


emptyContext :: Context n
emptyContext 
        = Context []
