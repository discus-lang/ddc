
module DDC.Llvm.Function
        ( Section   (..)
        , Function  (..))
where
import DDC.Llvm.Instr
import DDC.Base.Pretty


-- Section --------------------------------------------------------------------
-- | The section name to put the function in.
data Section
        -- | Let the LLVM decide what section to put this in.
        = SectionAuto

        -- | Put it in this specific section.
        | SectionSpecific String
        deriving (Eq, Show)


-- Function -------------------------------------------------------------------
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
                        AlignNone       -> empty
                        AlignBytes b    -> text " align" <+> ppr b

        args'  = [ ppr ty <+> hsep (map ppr attrs) <+> text "%" <> text nParam
                        | Param ty attrs <- params
                        | nParam         <- nsParam ]

    in ppr linkage
        <+> ppr callConv
        <+> ppr tReturn
        <+> text "@" <> text name
        <>  lparen 
        <>  (hcat $ punctuate (comma <> space) args') <> varg' 
        <>  rparen 
        <>  align'

