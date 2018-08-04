{-# LANGUAGE OverloadedStrings #-}

module DDC.Llvm.Write.Metadata where
import DDC.Llvm.Syntax.Metadata
import DDC.Llvm.Write.Type      ()
import DDC.Llvm.Write.Base


instance Write Config Metadata where
 write o mt
  = case mt of
        Debug
         -> text o "DEBUGMD"

        Tbaa (MDNode ops)
         -> do text o "!"; braces o (punc' o "," (map (write o) ops))


instance Write Config MDecl where
 write o (MDecl ref m)
  | configWantsMetadataAsValue o
  = do write o ref; text o " = metadata "; write o m

  | otherwise
  = do write o ref; text o " = "; write o m


instance Write Config MDNodeOp where
 write o op
  | configWantsMetadataAsValue o
  = case op of
        OpNull          -> text o "null"
        OpMDString ms   -> do text o "metadata"; space o; write o ms
        OpMDNode   ns   -> do text o "metadata"; space o; write o ns
        OpMDRef    r    -> do text o "metadata"; space o; write o r
        OpBool     b    -> do text o "i64"; space o; text o (if b then "1" else "0")
        OpType     t    -> write o t

  | otherwise
  = case op of
        OpNull          -> text  o "null"
        OpMDString ms   -> write o ms
        OpMDNode   ns   -> write o ns
        OpMDRef    r    -> do space o; write o r
        OpBool     b    -> do text  o "i64"; space o; text o (if b then "1" else "0")
        OpType     t    -> write o t


instance Write Config MDNode where
 write o (MDNode ns)
  = do text o "!"; braces o (punc' o "," (map (write o) ns))


instance Write Config MDString where
 write o (MDString s)
  = do text o "!"; dquotes o (string o s)


instance Write Config MRef where
 write o (MRef i)
  = do text o "!"; write o i


