
module DDC.Llvm.Syntax
        ( -- * Modules
          Module        (..)
        , lookupCallConv

          -- * Global variables
        , Global        (..)
        , typeOfGlobal
        , varOfGlobal

          -- * Static data
        , Static        (..)
        , typeOfStatic

          -- * Function declarations
        , FunctionDecl  (..)
        , ParamListType (..)
        , Param         (..)
        , Align         (..)

          -- * Functions
        , Function      (..)
        , Section       (..)

          -- * Blocks
        , Block         (..)
        , defVarsOfBlock

          -- * Block labels
        , Label         (..)

          -- * Annotated Instructions
        , AnnotInstr    (..)
        , annotNil
        , annotWith

          -- * Instructions
        , Instr         (..)
        , branchTargetsOfInstr
        , defVarOfInstr

          -- * Metadata
        , Metadata      (..)
        , MDecl         (..)
        , MRef          (..)
        , rval
        , tbaaNode

          -- * Expression types
        , Type          (..)
        , TypeAlias     (..)
        , isInt
        , isFloat
        , isPointer
        , takeBytesOfType

          -- * Expressions
        , Exp           (..)
        , typeOfExp

          -- * Variables
        , Var           (..)
        , nameOfVar
        , typeOfVar

          -- * Names
        , Name          (..)    

          -- * Literals
        , Lit           (..)
        , typeOfLit

          -- * Primitive operators
        , Op            (..)
        , ICond         (..)
        , FCond         (..)
        , Conv          (..)

          -- * Attributes
        , FuncAttr      (..)
        , ParamAttr     (..)
        , CallConv      (..)
        , CallType      (..)
        , Linkage       (..))
where
import DDC.Llvm.Syntax.Attr
import DDC.Llvm.Syntax.Exp
import DDC.Llvm.Syntax.Function
import DDC.Llvm.Syntax.Instr
import DDC.Llvm.Syntax.Metadata
import DDC.Llvm.Syntax.Module
import DDC.Llvm.Syntax.Prim
import DDC.Llvm.Syntax.Type
