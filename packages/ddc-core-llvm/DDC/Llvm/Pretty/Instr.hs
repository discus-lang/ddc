
module DDC.Llvm.Pretty.Instr where
import DDC.Llvm.Syntax.Instr
import DDC.Llvm.Syntax.Exp
import DDC.Llvm.Syntax.Metadata
import DDC.Llvm.Syntax.Attr
import DDC.Llvm.Pretty.Exp
import DDC.Llvm.Pretty.Prim     ()
import DDC.Llvm.Pretty.Metadata ()
import Data.List
import qualified Data.Foldable  as Seq
import DDC.Base.Pretty


instance Pretty Label where
 ppr (Label str)        = text str


instance  Pretty Block where
 ppr (Block label instrs)
        =    ppr label <> colon
        <$$> indent 8 (vcat $ map ppr $ Seq.toList instrs)


instance Pretty AnnotInstr where
 ppr (AnnotInstr instr []) = ppr instr
 ppr (AnnotInstr instr mds)
  = let pprWithTag (MDecl ref Tbaa{}) = text "!tbaa"  <> space <> ppr ref
        pprWithTag (MDecl ref Debug)  = text "!debug" <> space <> ppr ref
    in  ppr  instr
        <>   comma <> (hcat $ replicate 4 space)
        <>   (hcat $ punctuate (comma <> space) (map pprWithTag mds))


instance Pretty Instr where
 ppr ii
  = let -- Pad binding occurrence of variable.
        padVar  var
         = fill 12 (ppr $ nameOfVar var)

    in  case ii of
        -- Meta-instructions -------------------------------
        IComment strs           
         -> vcat $ map (semi <+>) $ map text strs

        ISet dst val
         -> hsep [ fill 12 (ppr $ nameOfVar dst)
                 , equals
                 , ppr val ]

        INop 
         -> text "nop"

        -- Phi nodes --------------------------------------
        IPhi vDst expLabels
         -> padVar vDst
                <+> equals
                <+> text "phi"
                <+> ppr (typeOfVar vDst)
                <+> hcat
                     (intersperse (comma <> space)
                        [ brackets
                                (   pprPlainX xSrc
                                <>  comma
                                <+> text "%" <> ppr label)
                        | (xSrc, label)         <- expLabels ])

        -- Terminator Instructions ------------------------
        IReturn Nothing         
         -> text "ret void"

        IReturn (Just value)    
         -> text "ret" <+> ppr value

        IBranch label
         -> text "br label %"  <> ppr label

        IBranchIf cond labelTrue labelFalse
         -> hsep [ text "br"
                 , ppr cond,      comma
                 , ppr labelTrue, comma
                 , ppr labelFalse ]

        ISwitch x1 lDefault alts
         -> text "switch"
                <+> ppr x1 <> comma
                <+> text "label %" <> ppr lDefault
                <+> lbracket
                <+> (hsep [ ppr discrim 
                                <> comma
                                <> text "label %" <> ppr dest
                                | (discrim, dest) <- alts ])
                <+> rbracket

        IUnreachable
         -> text "unreachable"

        -- Memory Operations ------------------------------
        ILoad vDst x1
         -> padVar vDst
                <+> equals
                <+> text "load"
                <+> ppr x1

        IStore xDst xSrc
         -> text "store"
                <+> ppr xSrc  <> comma
                <+> ppr xDst

        -- Binary Operations ------------------------------
        IOp vDst op x1 x2
         -> padVar vDst
                <+> equals
                <+> ppr op      <+> ppr (typeOfExp x1)
                <+> pprPlainX x1 <> comma 
                <+> pprPlainX x2

        -- Conversion operations --------------------------
        IConv vDst conv xSrc
         -> padVar vDst
                <+> equals
                <+> ppr conv
                <+> ppr xSrc
                <+> text "to"
                <+> ppr (typeOfVar vDst)

        -- Other operations -------------------------------
        IICmp vDst icond x1 x2
         -> padVar vDst
                <+> equals
                <+> text "icmp"  <+> ppr icond  <+> ppr (typeOfExp x1)
                <+> pprPlainX x1 <> comma
                <+> pprPlainX x2

        IFCmp vDst fcond x1 x2
         -> padVar vDst
                <+> equals
                <+> text "fcmp"  <+> ppr fcond  <+> ppr (typeOfExp x1)
                <+> pprPlainX x1 <> comma
                <+> pprPlainX x2

        ICall mdst callType callConv tResult name xsArgs attrs
         -> let call'
                 = case callType of
                        CallTypeTail    -> text "tail call"
                        _               -> text "call"
                dst'
                 = case mdst of
                        Nothing         -> empty
                        Just dst        -> fill 12 (ppr $ nameOfVar dst) <+> equals <> space

            in dst' 
                <> hsep  [ call'
                         , case callConv of
                                Nothing -> empty
                                Just cc -> ppr cc
                         , ppr tResult
                         , ppr name
                         , encloseSep lparen rparen (comma <> space) (map ppr xsArgs)
                         , hsep $ map ppr attrs ]

