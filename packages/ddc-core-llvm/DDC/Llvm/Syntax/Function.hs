
module DDC.Llvm.Syntax.Function
        ( Section       (..)
        , Function      (..))
where
import DDC.Llvm.Syntax.Instr
import DDC.Llvm.Syntax.Type
import DDC.Llvm.Syntax.Attr


-- | A LLVM Function
data Function
        = Function 
        { -- | The signature of this declared function.
          funDecl          :: FunctionDecl

          -- | The function parameter names.
        , funParams        :: [String]

          -- | The function attributes.
        , funAttrs         :: [FuncAttr]

          -- | The section to put the function into,
        , funSection       :: Section

          -- | The body of the functions.
        , funBlocks        :: [Block]
        }


-- | The section name to put the function in.
data Section
        -- | Let the LLVM decide what section to put this in.
        = SectionAuto

        -- | Put it in this specific section.
        | SectionSpecific String
        deriving (Eq, Show)

