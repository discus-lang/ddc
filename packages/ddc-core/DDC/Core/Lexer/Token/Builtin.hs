
module DDC.Core.Lexer.Token.Builtin
        ( Builtin (..)
        , sayBuiltin)
where
import DDC.Core.Exp
import DDC.Base.Pretty


-------------------------------------------------------------------------------
-- | Builtin name tokens.
data Builtin
        = BSoCon        SoCon
        | BKiCon        KiCon
        | BTwCon        TwCon
        | BTcCon        TcCon

        | BPure
        | BEmpty

        | BDaConUnit
        deriving (Eq, Show)


-------------------------------------------------------------------------------
-- | Yield the string name of a Builtin.
sayBuiltin :: Builtin -> String
sayBuiltin bb
 = case bb of
        BSoCon sc       -> renderPlain $ ppr sc
        BKiCon ki       -> renderPlain $ ppr ki
        BTwCon kw       -> renderPlain $ ppr kw
        BTcCon tc       -> renderPlain $ ppr tc
        BPure           -> "Pure"
        BEmpty          -> "Empty"
        BDaConUnit      -> "()"