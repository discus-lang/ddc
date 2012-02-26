
module DDC.Llvm.Base
        ( LMString
        , Unique
        , LlvmBlockId)
where

-- | A String in LLVM
type LMString 
        = String

-- | Unique id.
type Unique
        = Int

-- | Block labels
type LlvmBlockId 
        = Unique



