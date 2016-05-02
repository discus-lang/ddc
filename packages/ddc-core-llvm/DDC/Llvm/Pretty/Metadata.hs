{-# LANGUAGE TypeFamilies #-}
module DDC.Llvm.Pretty.Metadata where
import DDC.Llvm.Syntax.Metadata
import DDC.Llvm.Pretty.Type             ()
import DDC.Base.Pretty


-------------------------------------------------------------------------------
instance Pretty Metadata where
 data PrettyMode Metadata
        = PrettyModeMetadata
        { modeMetadataMetadataAsValue   :: Bool }

 pprDefaultMode
        = PrettyModeMetadata
        { modeMetadataMetadataAsValue   = False  }

 pprModePrec    mode prec md
  = pprMetaData mode prec md

pprMetaData (PrettyModeMetadata asValue) _ mt
  = let downMDNodeOp = pprMDNodeOp 
                        (PrettyModeMDNodeOp asValue) 
                        (0 :: Int)
    in case mt of
         Tbaa (MDNode ops) 
          -> text "!" <> encloseSep lbrace rbrace 
                                (comma <> space) 
                                (map downMDNodeOp ops)
         Debug  
          -> text "DEBUGMD"


-------------------------------------------------------------------------------
instance Pretty MDecl where
 data PrettyMode MDecl
        = PrettyModeMDecl
        { modeMDeclMetadataAsValue      :: Bool }

 pprDefaultMode
        = PrettyModeMDecl
        { modeMDeclMetadataAsValue      = False }

 pprModePrec mode prec md
  = pprMDecl mode prec md

pprMDecl (PrettyModeMDecl asValue) _ (MDecl ref m) 
 | asValue
 = ppr ref <> space <> equals <> space 
           <> text "metadata" <> space
           <> pprMetaData (PrettyModeMetadata asValue) (0 :: Int) m

 | otherwise
 = ppr ref <> space <> equals <> space 
           <> pprMetaData (PrettyModeMetadata asValue) (0 :: Int) m


-------------------------------------------------------------------------------
instance Pretty (MRef) where
 ppr (MRef i) 
  = text ("!" ++ show i)


-------------------------------------------------------------------------------
instance Pretty MDString where
 ppr (MDString s)
  = text "!" <> (dquotes $ text s)
  

-------------------------------------------------------------------------------
instance Pretty MDNode where
 ppr (MDNode ns) 
  = text "!" <> braces (ppr ns)


-------------------------------------------------------------------------------
instance Pretty MDNodeOp where
 data PrettyMode MDNodeOp
        = PrettyModeMDNodeOp
        { modeMDNodeOpMetadataAsValue   :: Bool }

 pprDefaultMode
        = PrettyModeMDNodeOp
        { modeMDNodeOpMetadataAsValue   = False }

 pprModePrec    mode prec elt
  = pprMDNodeOp mode prec elt

pprMDNodeOp mode _prec elt
 | modeMDNodeOpMetadataAsValue mode
 = case elt of
        OpNull        -> text "null"
        OpMDString ms -> text "metadata" <> space <> ppr ms
        OpMDNode   ns -> text "metadata" <> space <> ppr ns
        OpMDRef    r  -> text "metadata" <> space <> ppr r 
        OpBool     b  -> text "i32" <> space <> text (if b then "1" else "0")
        OpType     t  -> ppr t 

 | otherwise
 = case elt of
        OpNull        -> text "null"
        OpMDString ms -> ppr ms
        OpMDNode   ns -> ppr ns
        OpMDRef    r  -> space <> ppr r 
        OpBool     b  -> text "i32" <> space <> text (if b then "1" else "0")
        OpType     t  -> ppr t 

