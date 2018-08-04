
module DDC.Llvm.Write.Attr where
import DDC.Llvm.Syntax.Attr
import DDC.Llvm.Write.Base


instance Write Config FuncAttr where
 write o attr
  = case attr of
        AlwaysInline    -> text o "alwaysinline"
        InlineHint      -> text o "inlinehint"
        NoInline        -> text o "noinline"
        OptSize         -> text o "optsize"
        NoReturn        -> text o "noreturn"
        NoUnwind        -> text o "nounwind"
        ReadNone        -> text o "readnon"
        ReadOnly        -> text o "readonly"
        Ssp             -> text o "ssp"
        SspReq          -> text o "ssqreq"
        NoRedZone       -> text o "noredzone"
        NoImplicitFloat -> text o "noimplicitfloat"
        Naked           -> text o "naked"


instance Write Config ParamAttr where
 write o attr
  = case attr of
        ZeroExt         -> text o "zeroext"
        SignExt         -> text o "signext"
        InReg           -> text o "inreg"
        ByVal           -> text o "byval"
        SRet            -> text o "sret"
        NoAlias         -> text o "noalias"
        NoCapture       -> text o "nocapture"
        Nest            -> text o "nest"


instance Write Config CallConv where
 write o cc
  = case cc of
        CC_Ccc          -> text o "ccc"
        CC_Fastcc       -> text o "fastcc"
        CC_Coldcc       -> text o "coldcc"
        CC_Ncc i        -> do text o "cc "; write o i
        CC_X86_Stdcc    -> text o "x86_stdcallcc"
        CC_Intrinsic    -> return ()


instance Write Config Linkage where
 write o lt
  = case lt of
        Internal        -> text o "internal"
        LinkOnce        -> text o "linkonce"
        Weak            -> text o "weak"
        Appending       -> text o "appending"
        ExternWeak      -> text o "extern_weak"

        -- ExternallyVisible does not have a textual representation, it is
        -- the linkage type a function resolves to if no other is specified
        -- in Llvm.
        ExternallyVisible -> return ()

        External        -> text o "external"


instance Write Config CallType where
 write o ct
  = case ct of
        CallTypeStd     -> return ()
        CallTypeTail    -> text o "tail"

