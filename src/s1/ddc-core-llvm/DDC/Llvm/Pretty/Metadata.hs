{-# LANGUAGE TypeFamilies #-}
module DDC.Llvm.Pretty.Metadata where
import DDC.Llvm.Syntax.Metadata
import DDC.Llvm.Pretty.Type             ()
import DDC.Llvm.Pretty.Base
import DDC.Data.Pretty


-------------------------------------------------------------------------------
instance Pretty Metadata where
 data PrettyMode Metadata
        = PrettyModeMetadata
        { modeMetadataConfig    :: Config }

 pprDefaultMode
        = PrettyModeMetadata
        { modeMetadataConfig    = defaultConfig  }

 pprModePrec    mode prec md
  = pprMetaData mode prec md

pprMetaData (PrettyModeMetadata config) _ mt
  = let downMDNodeOp = pprMDNodeOp
                        (PrettyModeMDNodeOp config)
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
        { modeMDeclConfig       :: Config }

 pprDefaultMode
        = PrettyModeMDecl
        { modeMDeclConfig       = defaultConfig }

 pprModePrec mode prec md
  = pprMDecl mode prec md

pprMDecl (PrettyModeMDecl config) _ (MDecl ref m)
 | configWantsMetadataAsValue config
 = ppr ref <> space <> equals <> space
           <> text "metadata" <> space
           <> pprMetaData (PrettyModeMetadata config) (0 :: Int) m

 | otherwise
 = ppr ref <> space <> equals <> space
           <> pprMetaData (PrettyModeMetadata config) (0 :: Int) m


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
        { modeMDNodeOpConfig   :: Config }

 pprDefaultMode
        = PrettyModeMDNodeOp
        { modeMDNodeOpConfig   = defaultConfig }

 pprModePrec    mode prec elt
  = pprMDNodeOp mode prec elt

pprMDNodeOp (PrettyModeMDNodeOp config) _prec elt
 | configWantsMetadataAsValue config
 = case elt of
        OpNull        -> text "null"
        OpMDString ms -> text "metadata" <> space <> ppr ms
        OpMDNode   ns -> text "metadata" <> space <> ppr ns
        OpMDRef    r  -> text "metadata" <> space <> ppr r
        OpBool     b  -> text "i64" <> space <> text (if b then "1" else "0")
        OpType     t  -> ppr t

 | otherwise
 = case elt of
        OpNull        -> text "null"
        OpMDString ms -> ppr ms
        OpMDNode   ns -> ppr ns
        OpMDRef    r  -> space <> ppr r
        OpBool     b  -> text "i64" <> space <> text (if b then "1" else "0")
        OpType     t  -> ppr t

