
module DDC.Llvm.Pretty.Metadata where
import DDC.Llvm.Syntax.Metadata
import DDC.Llvm.Pretty.Type             ()
import DDC.Base.Pretty


instance Pretty Metadata where
 ppr mt
  = case mt of
         Tbaa (MDNode ops) 
          -> text "!" <> encloseSep lbrace rbrace 
                                   (comma <> space) (map ppr ops)

         Debug  
          -> text "DEBUGMD"


instance Pretty MDecl where
  ppr (MDecl ref m) =  ppr ref 
                    <> space <> equals <> space 
                    <> text "metadata" <> space
                    <> ppr m

instance Pretty (MRef) where
  ppr (MRef i) = text ("!" ++ show i)


instance Pretty MDString where
  ppr (MDString s) = text "!" <> (dquotes $ text s)
  

instance Pretty MDNode where
  ppr (MDNode ns) = text "!" <> braces (ppr ns)


instance Pretty MDNodeOp where
 ppr elt
  = case elt of
         OpNull        -> text "null"
         OpMDString ms -> text "metadata" <> space <> ppr ms
         OpMDNode   ns -> text "metadata" <> space <> ppr ns
         OpMDRef    r  -> text "metadata" <> space <> ppr r 
         OpBool     b  -> text "i32"      <> space <> text (if b then "1" else "0")
         OpType     t  -> ppr t 


