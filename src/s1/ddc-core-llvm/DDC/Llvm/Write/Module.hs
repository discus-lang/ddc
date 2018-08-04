
module DDC.Llvm.Write.Module where
import DDC.Llvm.Syntax.Module
import DDC.Llvm.Syntax.Type
import DDC.Llvm.Syntax.Exp
import DDC.Llvm.Write.Function
import DDC.Llvm.Write.Exp       ()
import DDC.Llvm.Write.Type      ()
import DDC.Llvm.Write.Base



instance Write Config Module where
 write o (Module _comments aliases globals decls funcs mdecls)
  = do
        vwrite  o aliases
        vwrite  o globals
        vwrite' o [ do text o "declare "; writeFunctionDeclWithNames o decl Nothing
                  | decl <- decls ]
        line    o
        vwrite  o funcs

        line    o
        vwrite  o mdecls


instance Write Config Global where
 write o gg
  = case gg of
        GlobalStatic (Var name _) static
         -> do write o name; text o " = global "; write o static

        GlobalExternal (Var name t)
         -> do write o name; text o " = external global "; write o t


instance Write Config Static where
 write o ss
  = case ss of
        StaticComment s
         -> do text o "; "; string o s

        StaticLit l
         -> write o l

        StaticUninitType t
         -> do  write o t; text o " undef"

        StaticStr   s t
         -> do  write o t; text o " c\""; string o s; text o "\\00\""

        StaticArray ds t
         -> do  write o t; text o " ["; punc o ", " ds; text o "]"

        StaticStruct ds t
         -> do  write o t; text o "<{"; punc o ", " ds; text o "}>"

        StaticPointer (Var n t)
         -> do  write o t; text o "* "; write o n

        StaticBitc v t
         -> do  write o t; text o " bitcast "
                parens o $ do write o v; text o " to "; write o t

        StaticPtoI v t
         -> do  write o t; text o " ptrtoint "
                parens o $ do write o v; text o " to "; write o t

        StaticAdd s1 s2
         | isFloat (typeOfStatic s1)
         -> do  write o (typeOfStatic s1)
                text o " fadd ("; write o s1; text o ", "; write o s2; text o ")"

         | otherwise
         -> do  write o (typeOfStatic s1)
                text o " add (";  write o s1; text o ", "; write o s2; text o ")"

        StaticSub s1 s2
         | isFloat (typeOfStatic s1)
         -> do  write o (typeOfStatic s1)
                text o " fsub ("; write o s1; text o ", "; write o s2; text o ")"

         | otherwise
         -> do  write o (typeOfStatic s1)
                text o " sub (";  write o s1; text o ", "; write o s2; text o ")"

