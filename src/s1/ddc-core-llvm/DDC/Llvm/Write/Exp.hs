
module DDC.Llvm.Write.Exp where
import DDC.Llvm.Write.Type      ()
import DDC.Llvm.Write.Prim      ()
import DDC.Llvm.Syntax.Exp
import DDC.Llvm.Write.Base


instance Write Config Name where
 write o nn
  = case nn of
        NameGlobal str  -> do text o "@"; string o str
        NameLocal  str  -> do text o "%"; string o str


instance Write Config Var where
 write o vv
  = case vv of
        Var n t         -> do write o n; space o; write o t


instance Write Config Lit where
 write o ll
  = case ll of
        LitInt   _ i    -> write o i
        LitFloat _ f    -> write o f
        LitNull  _      -> text o "null"
        LitUndef _      -> text o "undef"

        LitString _ txEnc _
         -> do text o "c"; dquotes o (text o txEnc)


instance Write Config Exp where
 write o xx
  = case xx of
        XVar v          -> write o v
        XLit l          -> do write o (typeOfExp xx); write o l
        XUndef _        -> text o "undef"
        XConv _ c x     -> parens o $ do write o c; write o x

        XGet _ x is
         -> parens o $ do
                text  o "getelementptr"
                space o
                punc' o ", " $ (write o x : map (string o . show) is)

        XAdd t x1 x2
         -> parens o $ punc' o " " [text o "#ADD", write o t, write o x1, write o x2]