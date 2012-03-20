
module DDC.Llvm.Function
        ( Function  (..)
        , Block     (..))
where
import DDC.Llvm.Statement
import DDC.Llvm.Attr
import DDC.Llvm.Type
import DDC.Llvm.Var
import DDC.Base.Pretty


-- Function -------------------------------------------------------------------
-- | A LLVM Function
data Function 
        = Function 
        { -- | The signature of this declared function.
          functionDecl          :: FunctionDecl

          -- | The function parameter names.
        , functionParams        :: [String]

          -- | The function attributes.
        , functionAttrs         :: [FuncAttr]

          -- | The section to put the function into,
        , functionSection       :: Section

          -- | The body of the functions.
        , functionBlocks        :: [Block]
        }


instance Pretty Function where
 ppr (Function decl paramNames attrs sec body) 
  = let attrDoc = hsep $ map ppr attrs
        secDoc  = case sec of
                        SectionAuto       -> empty
                        SectionSpecific s -> text "section" <+> (dquotes $ text s)

    in text "define" 
        <+> pprFunctionHeader decl paramNames
                <+> attrDoc <+> secDoc
        <$> lbrace
        <$> vcat (map ppr body)
        <$> rbrace


-- | Print out a function defenition header.
pprFunctionHeader :: FunctionDecl -> [String] -> Doc
pprFunctionHeader 
        (FunctionDecl name linkage callConv tReturn varg params alignment)
        nsParam
  = let varg'  = case varg of
                      VarArgs | null params -> text "..."
                              | otherwise   -> text ", ..."
                      _otherwise            -> empty

        align' = case alignment of
                        AlignmentNone       -> empty
                        AlignmentBytes b    -> text " align" <+> ppr b

        args'  = [ ppr ty <+> hsep (map ppr attrs) <+> text "%" <> text nParam
                        | Parameter ty attrs <- params
                        | nParam             <- nsParam ]

    in ppr linkage
        <+> ppr callConv
        <+> ppr tReturn
        <+> text "@" <> text name
        <>  lparen 
        <>  (hcat $ punctuate (comma <> space) args') <> varg' 
        <>  rparen 
        <>  align'


-- Block ----------------------------------------------------------------------
-- | A block of LLVM code.
data Block 
        = Block 
        { -- | The code label for this block
          blockLabel :: LlvmBlockId

          -- | A list of LlvmStatement's representing the code for this block.
          -- This list must end with a control flow statement.
        , blockStmts :: [LlvmStatement]
        }


instance Pretty Block where
 ppr _ = text "BLOCK"

