{-# LANGUAGE TypeFamilies #-}
module DDC.Llvm.Pretty.Instr where
import DDC.Llvm.Syntax.Attr
import DDC.Llvm.Syntax.Exp
import DDC.Llvm.Syntax.Instr
import DDC.Llvm.Syntax.Metadata
import DDC.Llvm.Syntax.Prim
import DDC.Llvm.Syntax.Type
import DDC.Llvm.Pretty.Exp
import DDC.Llvm.Pretty.Prim     ()
import DDC.Llvm.Pretty.Metadata ()
import DDC.Llvm.Pretty.Base
import DDC.Data.Pretty
import Data.List
import qualified Data.Foldable  as Seq


-------------------------------------------------------------------------------
instance Pretty Label where
 ppr (Label str)        = text str


-------------------------------------------------------------------------------
instance  Pretty Block where
 data PrettyMode Block
        = PrettyModeBlock
        { modeBlockConfig :: Config }

 pprDefaultMode
        = PrettyModeBlock
        { modeBlockConfig = defaultConfig }

 pprModePrec 
        (PrettyModeBlock config) prec 
        (Block label instrs)
  = let downAnnotInstr
         = pprModePrec (PrettyModeAnnotInstr config) prec

    in    ppr label <>  colon 
     <$$> indent 8 (vcat $ map downAnnotInstr $ Seq.toList instrs)


-------------------------------------------------------------------------------
instance Pretty AnnotInstr where
 data PrettyMode AnnotInstr
        = PrettyModeAnnotInstr
        { modeAnnotInstrConfig :: Config }

 pprDefaultMode
        = PrettyModeAnnotInstr
        { modeAnnotInstrConfig = defaultConfig }

 pprModePrec (PrettyModeAnnotInstr config) prec ainstr
  = case ainstr of
        AnnotInstr instr []
         -> pprModePrec (PrettyModeInstr config) prec instr

        AnnotInstr instr mds
         -> let pprWithTag (MDecl ref Tbaa{}) = text "!tbaa"  <> space <> ppr ref
                pprWithTag (MDecl ref Debug)  = text "!debug" <> space <> ppr ref
            in  pprModePrec (PrettyModeInstr config) prec instr
                 <> comma <> (hcat $ replicate 4 space)
                 <> (hcat $ punctuate (comma <> space) (map pprWithTag mds))


-------------------------------------------------------------------------------
instance Pretty Instr where
 data PrettyMode Instr
        = PrettyModeInstr
        { modeInstrConfig :: Config }

 pprDefaultMode 
        = PrettyModeInstr
        { modeInstrConfig = defaultConfig }

 pprModePrec (PrettyModeInstr config) _prec ii
  = let 
        -- Pad binding occurrence of variable.
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
                 , ppr cond,                         comma
                 , text "label %" <> ppr labelTrue,  comma
                 , text "label %" <> ppr labelFalse ]

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
        IAlloca vDst tVal
         -> padVar vDst
                <+> equals
                <+> text "alloca"
                <+> ppr tVal

        ILoad vDst x1
         -- From LLVM 3.7 we need to give the type of the source pointer
         -- as well as the type of the result of the load.
         |  configWantsLoadReturnTypes config
         -> padVar vDst
                <+> equals 
                <+> text "load" 
                <+> ppr (typeOfVar vDst) <> comma       -- Type of result.
                <+> ppr x1                              -- Pointer type of source.

         -- Before LLVM 3.7 we only needed to give the type of the source pointer.
         |  otherwise
         -> padVar vDst 
                <+> equals 
                <+> text "load" <+> ppr x1

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

        IGet vDst xSrc os
         -- From LLVM 3.7 we need to give the type of the source pointer
         -- as well as the type of the result of the load.      
         |  configWantsLoadReturnTypes config
         ,  XVar (Var _ (TPointer t)) <- xSrc
         -> padVar vDst
                <+> equals
                <+> text "getelementptr"
                <+> ppr t <> comma              -- Type of result
                <+> (hcat $ punctuate (text ", ") $ (ppr xSrc : map ppr os))

         -- Before LLVM 3.7 we only needed to give the type of the source pointer.
         |  otherwise
         -> padVar vDst
                <+> equals
                <+> text "getelementptr"
                <+> (hcat $ punctuate (text ", ") $ (ppr xSrc : map ppr os))

        -- Other operations -------------------------------
        ICmp vDst (ICond icond) x1 x2
         -> padVar vDst
                <+> equals
                <+> text "icmp"  <+> ppr icond  <+> ppr (typeOfExp x1)
                <+> pprPlainX x1 <> comma
                <+> pprPlainX x2

        ICmp vDst (FCond fcond) x1 x2
         -> padVar vDst
                <+> equals
                <+> text "fcmp"  <+> ppr fcond  <+> ppr (typeOfExp x1)
                <+> pprPlainX x1 <> comma
                <+> pprPlainX x2

        ICall mdst callType callConv tResult name xsArgs attrs
         -> let call'
                 = case callType of
                        CallTypeTail -> text "tail call"
                        _            -> text "call"
                dst'
                 = case mdst of
                        Nothing      -> empty
                        Just dst     -> fill 12 (ppr $ nameOfVar dst) <+> equals <> space

                convSuffix' 0 = empty
                convSuffix' _ = space

                conv'
                 = case callConv of
                        Nothing -> empty
                        Just cc -> ppr cc

                convSpace'
                 = width conv' convSuffix'


            in dst' <> call' <+> convSpace' <> ppr tResult <+> ppr name
                   <+> encloseSep lparen rparen (comma <> space) (map ppr xsArgs)
                   <+> hsep (map ppr attrs)

