
module DDC.Llvm.Function
        ( -- * Function
          LlvmFunction  (..)
        , LlvmFunctions

          -- * Blocks
        , LlvmBlock     (..)
        , LlvmBlocks)
where
import DDC.Llvm.Statement
import DDC.Llvm.Var
import DDC.Llvm.Attr
import DDC.Llvm.Type
        

-- Function -------------------------------------------------------------------
-- | A LLVM Function
data LlvmFunction 
        = LlvmFunction 
        { -- | The signature of this declared function.
          funcDecl  :: LlvmFunctionDecl

          -- | The functions arguments
        , funcArgs  :: [LMString]

          -- | The function attributes.
        , funcAttrs :: [LlvmFuncAttr]

          -- | The section to put the function into,
        , funcSect  :: LMSection

          -- | The body of the functions.
        , funcBody  :: LlvmBlocks
        }

type LlvmFunctions  
        = [LlvmFunction]


-- Block ----------------------------------------------------------------------
-- | A block of LLVM code.
data LlvmBlock 
        = LlvmBlock 
        { -- | The code label for this block
          blockLabel :: LlvmBlockId

          -- | A list of LlvmStatement's representing the code for this block.
          -- This list must end with a control flow statement.
        , blockStmts :: [LlvmStatement]
        }

type LlvmBlocks 
        = [LlvmBlock]
