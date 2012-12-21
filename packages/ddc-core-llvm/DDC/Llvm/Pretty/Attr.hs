
-- | Attributes for functions and parameters.
module DDC.Llvm.Pretty.Attr where
import DDC.Llvm.Syntax.Attr
import DDC.Base.Pretty


instance Pretty FuncAttr where
 ppr attr
  = case attr of
        AlwaysInline    -> text "alwaysinline"
        InlineHint      -> text "inlinehint"
        NoInline        -> text "noinline"
        OptSize         -> text "optsize"
        NoReturn        -> text "noreturn"
        NoUnwind        -> text "nounwind"
        ReadNone        -> text "readnon"
        ReadOnly        -> text "readonly"
        Ssp             -> text "ssp"
        SspReq          -> text "ssqreq"
        NoRedZone       -> text "noredzone"
        NoImplicitFloat -> text "noimplicitfloat"
        Naked           -> text "naked"


instance Pretty ParamAttr where
 ppr attr
  = case attr of
        ZeroExt         -> text "zeroext"
        SignExt         -> text "signext"
        InReg           -> text "inreg"
        ByVal           -> text "byval"
        SRet            -> text "sret"
        NoAlias         -> text "noalias"
        NoCapture       -> text "nocapture"
        Nest            -> text "nest"


instance Pretty CallConv where
 ppr cc
  = case cc of
        CC_Ccc          -> text "ccc"
        CC_Fastcc       -> text "fastcc"
        CC_Coldcc       -> text "coldcc"
        CC_Ncc i        -> text "cc "  <> int i
        CC_X86_Stdcc    -> text "x86_stdcallcc"


instance Pretty Linkage where
 ppr lt
  = case lt of
        Internal          -> text "internal"
        LinkOnce          -> text "linkonce"
        Weak              -> text "weak"
        Appending         -> text "appending"
        ExternWeak        -> text "extern_weak"

        -- ExternallyVisible does not have a textual representation, it is
        -- the linkage type a function resolves to if no other is specified
        -- in Llvm.
        ExternallyVisible -> empty

        External          -> text "external"

instance Pretty CallType where
 ppr ct
  = case ct of
        CallTypeStd     -> empty
        CallTypeTail    -> text "tail"
