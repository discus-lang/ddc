
module DDC.Llvm.Function
        ( -- * Function
          LlvmFunction  (..)
        , LlvmSection   (..)

          -- * Blocks
        , LlvmBlock     (..)
        , LlvmBlocks)
where
import DDC.Llvm.Statement
import DDC.Llvm.Attr
import DDC.Llvm.Type
import DDC.Base.Pretty


-- Function -------------------------------------------------------------------
-- | A LLVM Function
data LlvmFunction 
        = LlvmFunction 
        { -- | The signature of this declared function.
          funcDecl      :: LlvmFunctionDecl

          -- | The function parameter names.
        , funcParams    :: [String]

          -- | The function attributes.
        , funcAttrs     :: [FuncAttr]

          -- | The section to put the function into,
        , funcSect      :: LlvmSection

          -- | The body of the functions.
        , funcBody      :: LlvmBlocks
        }


instance Pretty LlvmFunction where
 ppr (LlvmFunction decl paramNames attrs sec body) 
  = let attrDoc = hsep $ map ppr attrs
        secDoc  = case sec of
                        LlvmSectionAuto       -> empty
                        LlvmSectionSpecific s -> text "section" <+> (dquotes $ text s)

    in text "define" 
        <+> pprLlvmFunctionHeader decl paramNames
                <+> attrDoc <+> secDoc
        <$> lbrace
        <$> vcat (map ppr body)
        <$> rbrace


-- | Print out a function defenition header.
pprLlvmFunctionHeader :: LlvmFunctionDecl -> [String] -> Doc
pprLlvmFunctionHeader 
        (LlvmFunctionDecl name linkage callConv tReturn varg params alignment)
        nsParam
  = let varg' = case varg of
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


-- Section --------------------------------------------------------------------
-- | The section name to put the function in.
data LlvmSection
        -- | Let the LLVM decide what section to put this in.
        = LlvmSectionAuto

        -- | Put it in this specific section.
        | LlvmSectionSpecific String
        deriving (Eq, Show)



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

instance Pretty LlvmBlock where
 ppr _ = text "BLOCK"

