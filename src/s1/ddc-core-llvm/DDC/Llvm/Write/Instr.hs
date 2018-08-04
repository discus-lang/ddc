
module DDC.Llvm.Write.Instr where
import DDC.Llvm.Syntax
import DDC.Llvm.Write.Exp       ()
import DDC.Llvm.Write.Metadata  ()
import DDC.Llvm.Write.Base
import qualified Data.Foldable as Seq


instance Write Config Block where
 write o (Block l is)
  = do
        write   o l; text o ": "; line o
        vwrite' o [ do text o "    "; write o ai | ai <- Seq.toList is ]


instance Write Config AnnotInstr where
 write o ii
  = case ii of
        AnnotInstr i []
         -> write o i

        AnnotInstr i mds
         -> let writeWithTag (MDecl ref Tbaa{})
                 = do text o "!tbaa ";  write o ref

                writeWithTag (MDecl ref Debug)
                 = do text o "!debug "; write o ref
            in do
                write o i; text o ", "
                punc' o ", " $ map writeWithTag mds


instance Write Config Instr where
 write o ii
  = case ii of
        -- Meta-instructions -------------------------------
        IComment strs
         -> mapM_ (\s -> do string o s; text o ";") strs

        ISet dst val
         -> do write o (nameOfVar dst); text o " = "; write o val

        INop
         -> text o "nop"

        -- Phi nodes --------------------------------------
        IPhi vDst xls
         -> do  write o (nameOfVar vDst)
                text  o " = phi "
                write o (typeOfVar vDst)

                punc' o ", "
                 [ brackets o $ do write o x; text o ", %"; write o l
                 | (x, l) <- xls ]

        -- Terminator Instructions ------------------------
        IReturn Nothing
         ->     text o "ret void"

        IReturn (Just x)
         -> do  text o "ret "; write o (typeOfExp x); space o; write o x

        IBranch l
         -> do  text o "br label %"; write o l

        IBranchIf xCond lTrue lFalse
         -> do  text  o "br "
                write o (typeOfExp xCond); space o
                write o xCond
                text  o ", label %"; write o lTrue
                text  o ", label %"; write o lFalse

        ISwitch x1 lDefault alts
         -> do  text  o "switch "
                write o (typeOfExp x1); space o; write o x1
                text  o ", label %"; write o lDefault
                space o
                brackets o $ punc' o " "
                  [ do  write o (typeOfLit lDiscrim); space o; write o lDiscrim
                        text  o ", label %"; write o lDest
                  | (lDiscrim, lDest) <- alts ]

        IUnreachable
         ->     text o "unreachable"

        -- Memory Operations ------------------------------
        IAlloca vDst tVal
         -> do  write o (nameOfVar vDst); text o " = alloca "; write o tVal

        ILoad vDst x1
         -- From LLVM 3.7 we need to give the type of the source pointer
         -- as well as the type of the result of the load.
         |  configWantsLoadReturnTypes o
         -> do  write o (nameOfVar vDst)
                text  o " = load "
                write o (typeOfVar vDst)
                text  o ", "
                write o (typeOfExp x1); space o; write o x1

         -- Before LLVM 3.7 we only needed to give the type of the source pointer.
         | otherwise
         -> do  write o (nameOfVar vDst)
                text  o " = load "
                write o x1

        IStore xDst xSrc
         -> do  text  o "store "
                write o (typeOfExp xSrc); space o; write o xSrc
                text  o ", "
                write o (typeOfExp xDst); space o; write o xDst

        -- Binary Operations ------------------------------
        IOp vDst op x1 x2
         -> do  write o (nameOfVar vDst)
                text o " = "
                write o op;   text o " "
                write o (typeOfVar vDst); space o
                write o x1;   text o ", "
                write o x2

        -- Conversion operations --------------------------
        IConv vDst conv xSrc
         -> do  write o (nameOfVar vDst)
                text  o " = "
                write o conv; text o " "
                write o (typeOfExp xSrc); space o; write o xSrc
                text  o " to "
                write o (typeOfVar vDst)

        IGet vDst xSrc offs
         -- From LLVM 3.7 we need to give the type of the source pointer
         -- as well as the type of the result of the load.
         |  configWantsLoadReturnTypes o
         ,  XVar (Var _ (TPointer t)) <- xSrc
         -> do  write o (nameOfVar vDst)
                text  o " = getelementptr "
                write o t; text o ", "
                punc' o ", "
                   $ ( do write o (typeOfExp xSrc); space o; write o xSrc)
                   : [ do write o (typeOfExp xOff); space o; write o xOff | xOff <- offs]

         -- Before LLVM 3.7 we only needed to give the type of the source pointer.
         |  otherwise
         -> do  write o (nameOfVar vDst)
                text  o " = getelementptr "
                punc' o ", "
                   $ ( do write o (typeOfExp xSrc); space o; write o xSrc)
                   : [ do write o (typeOfExp xOff); space o; write o xOff | xOff <- offs]

        -- Other operations -------------------------------
        ICmp vDst (ICond icond) x1 x2
         -> do  write o (nameOfVar vDst);  text o " = icmp "
                write o icond; text o " "
                write o (typeOfExp x1)
                write o x1;    text o ", "
                write o x2

        ICmp vDst (FCond fcond) x1 x2
         -> do  write o (nameOfVar vDst)
                text o " = fcmp "
                write o fcond; text o " "
                write o (typeOfExp x1)
                write o x1;    text o ", "
                write o x2

        ICall mdst callType callConv tResult name xsArgs attrs
         -> do
                (case mdst of
                  Nothing       -> return ()
                  Just vDst     -> do write o (nameOfVar vDst); text o " = ")

                (case callType of
                  CallTypeStd   -> text o "call"
                  CallTypeTail  -> text o "tail call")

                (case callConv of
                  Nothing       -> return ()
                  Just cc       -> do space o; write o cc; space o)

                write  o tResult; space o
                write  o name
                parens o $ punc' o ", "
                        [ do write o (typeOfExp x); space o; write o x
                        | x <- xsArgs ]
                space  o
                punc   o " " attrs


instance Write Config Label where
 write o (Label str)
  = string o str

