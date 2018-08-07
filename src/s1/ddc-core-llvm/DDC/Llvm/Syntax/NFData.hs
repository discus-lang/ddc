
module DDC.Llvm.Syntax.NFData where
import DDC.Llvm.Syntax.Attr
import DDC.Llvm.Syntax.Prim
import DDC.Llvm.Syntax.Type
import DDC.Llvm.Syntax.Exp
import DDC.Llvm.Syntax.Metadata
import DDC.Llvm.Syntax.Instr
import DDC.Llvm.Syntax.Function
import DDC.Llvm.Syntax.Module
import Control.DeepSeq


instance NFData FuncAttr where
 rnf !_ = ()


instance NFData ParamAttr where
 rnf !_ = ()


instance NFData CallConv where
 rnf !_ = ()


instance NFData Linkage where
 rnf !_ = ()


instance NFData CallType where
 rnf !_ = ()


instance NFData Op where
 rnf !_ = ()


instance NFData Cond where
 rnf !_ = ()


instance NFData ICond where
 rnf !_ = ()


instance NFData FCond where
 rnf !_ = ()


instance NFData Conv where
 rnf !_ = ()


instance NFData TypeAlias where
 rnf (TypeAlias tx t) = rnf tx `seq` rnf t


instance NFData Type where
 rnf tt
  = case tt of
        TStruct ts      -> rnf ts
        TFunction decl  -> rnf decl
        _               -> ()


instance NFData Align where
 rnf !_ = ()


instance NFData Param where
 rnf (Param t attrs) = rnf t `seq` rnf attrs


instance NFData ParamListType where
 rnf !_ = ()


instance NFData FunctionDecl where
 rnf (FunctionDecl name linkage callconv ty psListType ps align mgc)
  =     rnf name `seq` rnf linkage `seq` rnf callconv `seq` rnf ty
  `seq` rnf psListType `seq` rnf ps `seq` rnf align `seq` rnf mgc


instance NFData Exp where
 rnf xx
  = case xx of
        XVar v          -> rnf v
        XLit l          -> rnf l
        XUndef t        -> rnf t
        XConv  t c x    -> rnf t `seq` rnf c  `seq` rnf x
        XGet   t x xs   -> rnf t `seq` rnf x  `seq` rnf xs
        XAdd   t x1 x2  -> rnf t `seq` rnf x1 `seq` rnf x2


instance NFData Var where
 rnf (Var n t)
  = rnf n `seq` rnf t


instance NFData Name where
 rnf nn
  = case nn of
        NameGlobal str  -> rnf str
        NameLocal  str  -> rnf str


instance NFData Lit where
 rnf ll
  = case ll of
        LitInt    t i     -> rnf t `seq` rnf i
        LitFloat  t d     -> rnf t `seq` rnf d
        LitString s1 s2 i -> rnf s1 `seq` rnf s2 `seq` rnf i
        LitNull   t       -> rnf t
        LitUndef  t       -> rnf t


instance NFData Label where
 rnf (Label str)
  = rnf str


instance NFData Block where
 rnf (Block l as)
  = rnf l `seq` rnf as


instance NFData Metadata where
 rnf mm
  = case mm of
        Tbaa md         -> rnf md
        Debug           -> ()


instance NFData MDecl where
 rnf (MDecl ref md)
  = rnf ref `seq` rnf md


instance NFData MRef where
 rnf !_ = ()


instance NFData MDString where
 rnf (MDString str) = rnf str


instance NFData MDNode where
 rnf (MDNode xs) = rnf xs


instance NFData MDNodeOp where
 rnf mm
  = case mm of
        OpNull          -> ()
        OpMDString md   -> rnf md
        OpMDNode md     -> rnf md
        OpMDRef md      -> rnf md
        OpBool b        -> rnf b
        OpType t        -> rnf t


instance NFData AnnotInstr where
 rnf (AnnotInstr i mdecls)
  = rnf i `seq` rnf mdecls


instance NFData Instr where
 rnf ii
  = case ii of
        IComment ss             -> rnf ss
        ISet    v x             -> rnf v `seq` rnf x
        INop                    -> ()
        IPhi    v xls           -> rnf v `seq` rnf xls
        IReturn mx              -> rnf mx
        IBranch l               -> rnf l
        IBranchIf x l1 l2       -> rnf x `seq` rnf l1 `seq` rnf l2
        ISwitch x l lls         -> rnf x `seq` rnf l  `seq` rnf lls
        IUnreachable            -> ()
        IOp     v o x1 x2       -> rnf v `seq` rnf o `seq` rnf x1 `seq` rnf x2
        IConv   v c x           -> rnf v `seq` rnf c `seq` rnf x
        IGet    v x xs          -> rnf v `seq` rnf x `seq` rnf xs
        IAlloca v t             -> rnf v `seq` rnf t
        ILoad   v x             -> rnf v `seq` rnf x
        IStore  x1 x2           -> rnf x1 `seq` rnf x2

        ICmp    v c x1 x2
         -> rnf v `seq` rnf c `seq` rnf x1 `seq` rnf x2

        ICall   mv ct mcc t n nxs as
         -> rnf mv `seq` rnf ct `seq` rnf mcc `seq` rnf t `seq` n `seq` rnf nxs `seq` rnf as


instance NFData Static where
 rnf ss
  = case ss of
        StaticComment str       -> rnf str
        StaticLit lit           -> rnf lit
        StaticUninitType t      -> rnf t
        StaticStr str t         -> rnf str `seq` rnf t
        StaticArray sts t       -> rnf sts `seq` rnf t
        StaticStruct sts t      -> rnf sts `seq` rnf t
        StaticPointer v         -> rnf v
        StaticBitc st t         -> rnf st  `seq` rnf t
        StaticPtoI st t         -> rnf st  `seq` rnf t
        StaticAdd  st1 st2      -> rnf st1 `seq` rnf st2
        StaticSub  st1 st2      -> rnf st1 `seq` rnf st2


instance NFData Global where
 rnf gg
  = case gg of
        GlobalStatic v s        -> rnf v `seq` rnf s
        GlobalExternal v        -> rnf v


instance NFData Function where
 rnf (Function decl params attrs section blocks)
  =     rnf decl `seq` rnf params `seq` rnf attrs
  `seq` rnf section `seq` rnf blocks


instance NFData Section where
 rnf ss
  = case ss of
        SectionAuto             -> ()
        SectionSpecific str     -> rnf str


instance NFData Module where
 rnf (Module comments aliases globals decls fns mdecls)
   =     rnf comments `seq` rnf aliases `seq` rnf globals
   `seq` rnf decls    `seq` rnf fns     `seq` rnf mdecls

