
module DDC.Llvm.Attr
        ( LlvmFuncAttr       (..)
        , LlvmParamAttr      (..)
        , LlvmCallConvention (..)
        , LlvmLinkageType    (..))
where
import DDC.Base.Pretty

 
-- FuncAttr ---------------------------------------------------------------------------------------
-- | Function attributes are set to communicate additional information about a
--   function. Function attributes are considered to be part of the function,
--   not of the function type, so functions with different parameter attributes
--   can have the same function type. Functions can have multiple attributes.
--
--   Descriptions taken from <http://llvm.org/docs/LangRef.html#fnattrs>
data LlvmFuncAttr
        -- | The inliner should attempt to inline this function into callers
        --   whenever possible, ignoring any active inlining size threshold for
        --   this caller.
        = AlwaysInline

        -- | The source code contained a hint that inlining this function is
        --   desirable (such as the \"inline\" keyword in C/C++). 
        --   It is just a hint; it imposes no requirements on the inliner.
        | InlineHint

        -- | The inliner should never inline this function in any situation. 
        --   This attribute may not be used together with the alwaysinline attribute.
        | NoInline

        -- | Suggests that optimization passes and code generator passes make choices
        --   that keep the code size of this function low, and otherwise do
        --   optimizations specifically to reduce code size.
        | OptSize

        -- | The function never returns normally. 
        --   This produces undefined behavior at runtime if the function ever does
        --   dynamically return.
        | NoReturn

        -- | The function never returns with an unwind or exceptional control flow. 
        --   If the function does unwind, its runtime behavior is undefined.
        | NoUnwind

        -- | The function computes its result (or decides to unwind an exception) 
        --   based strictly on its arguments, without
        --   dereferencing any pointer arguments or otherwise accessing any mutable
        --   state (e.g. memory, control registers, etc) visible to caller functions.
        --   It does not write through any pointer arguments (including byval
        --   arguments) and never changes any state visible to callers. This means
        --   that it cannot unwind exceptions by calling the C++ exception throwing
        --   methods, but could use the unwind instruction.
        | ReadNone

        -- | The function does not write through any
        --   pointer arguments (including byval arguments) or otherwise modify any
        --   state (e.g. memory, control registers, etc) visible to caller functions.
        --   It may dereference pointer arguments and read state that may be set in
        --   the caller. A readonly function always returns the same value (or unwinds
        --   an exception identically) when called with the same set of arguments and
        --   global state. It cannot unwind an exception by calling the C++ exception
        --   throwing methods, but may use the unwind instruction.
        | ReadOnly

        -- | The function should emit a stack smashing protector. 
        --   It is in the form of a \"canary\"—a random value placed on the
        --   stack before the local variables that's checked upon return from the
        --   function to see if it has been overwritten. A heuristic is used to
        --   determine if a function needs stack protectors or not.
        --   If a function that has an ssp attribute is inlined into a function that
        --   doesn't have an ssp attribute, then the resulting function will have an
        --   ssp attribute.
        | Ssp

        -- | The function should always emit a stack smashing protector. 
        --   This overrides the ssp function attribute.
        --   If a function that has an sspreq attribute is inlined into a function
        --   that doesn't have an sspreq attribute or which has an ssp attribute,
        --   then the resulting function will have an sspreq attribute.
        | SspReq

        -- | The code generator should not use a red zone, even if the
        --   target-specific ABI normally permits it.
        | NoRedZone

        -- | Disables implicit floating point instructions.
        | NoImplicitFloat

        -- | Disables prologue / epilogue emission for the function.
        --   This can have very system-specific consequences.
        | Naked
        deriving (Eq, Show)


instance Pretty LlvmFuncAttr where
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



-- ParamAttr --------------------------------------------------------------------------------------
-- | Parameter attributes are used to communicate additional information about
--   the result or parameters of a function
data LlvmParamAttr
        -- | That the parameter or return value should be zero-extended to a 32-bit value
        --   by the caller (for a parameter) or the callee (for a return value).
        = ZeroExt

        -- | The parameter or return value should be sign-extended to a 32-bit value
        --   by the caller (for a parameter) or the callee (for a return value).
        | SignExt

        -- | The parameter or return value should be treated in a special target-dependent
        --   fashion during while emitting code for a function call or return (usually,
        --   by putting it in a register as opposed to memory).
        | InReg

        -- | The pointer parameter should really be passed by value to the function.
        | ByVal

        -- | The pointer parameter specifies the address of a structure that is the
        --   return value of the function in the source program.
        | SRet

        -- | The pointer does not alias any global or any other parameter.
        | NoAlias

        -- | The callee does not make any copies of the pointer that outlive the callee itself.
        | NoCapture

        -- | The pointer parameter can be excised using the trampoline intrinsics.
        | Nest
        deriving (Eq, Show)


instance Pretty LlvmParamAttr where
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


-- CallConvention ---------------------------------------------------------------------------------
-- | Different calling conventions a function can use.
data LlvmCallConvention

  -- | The C calling convention.
  --   This calling convention (the default if no other calling convention is
  --   specified) matches the target C calling conventions. This calling
  --   convention supports varargs function calls and tolerates some mismatch in
  --   the declared prototype and implemented declaration of the function (as
  --   does normal C).
  = CC_Ccc

  -- | This calling convention attempts to make calls as fast as possible
  --   (e.g. by passing things in registers). This calling convention allows
  --   the target to use whatever tricks it wants to produce fast code for the
  --   target, without having to conform to an externally specified ABI
  --   (Application Binary Interface). Implementations of this convention should
  --   allow arbitrary tail call optimization to be supported. This calling
  --   convention does not support varargs and requires the prototype of al
  --   callees to exactly match the prototype of the function definition.
  | CC_Fastcc

  -- | This calling convention attempts to make code in the caller as efficient
  --   as possible under the assumption that the call is not commonly executed.
  --   As such, these calls often preserve all registers so that the call does
  --   not break any live ranges in the caller side. This calling convention
  --   does not support varargs and requires the prototype of all callees to
  --   exactly match the prototype of the function definition.
  | CC_Coldcc

  -- | Any calling convention may be specified by number, allowing
  --   target-specific calling conventions to be used. Target specific calling
  --   conventions start at 64.
  | CC_Ncc Int

  -- | X86 Specific 'StdCall' convention. LLVM includes a specific alias for it
  --   rather than just using CC_Ncc.
  | CC_X86_Stdcc
  deriving (Eq, Show)


instance Pretty LlvmCallConvention where
 ppr cc
  = case cc of
        CC_Ccc          -> text "ccc"
        CC_Fastcc       -> text "fastcc"
        CC_Coldcc       -> text "coldcc"
        (CC_Ncc i)      -> text "cc "  <> int i
        CC_X86_Stdcc    -> text "x86_stdcallcc"


-- LlvmLinkageType --------------------------------------------------------------------------------
-- | Linkage type of a symbol.
--
--   The description of the constructors is copied from the Llvm Assembly Language
--   Reference Manual <http://www.llvm.org/docs/LangRef.html#linkage>, because
--   they correspond to the Llvm linkage types.
data LlvmLinkageType

  -- | Global values with internal linkage are only directly accessible by
  --  objects in the current module. In particular, linking code into a module
  --  with an internal global value may cause the internal to be renamed as
  --  necessary to avoid collisions. Because the symbol is internal to the
  --  module, all references can be updated. This corresponds to the notion
  --  of the @static@ keyword in C.
  = Internal

  -- | Globals with @linkonce@ linkage are merged with other globals of the
  --  same name when linkage occurs. This is typically used to implement
  --  inline functions, templates, or other code which must be generated
  --  in each translation unit that uses it. Unreferenced linkonce globals are
  --  allowed to be discarded.
  | LinkOnce

  -- | @weak@ linkage is exactly the same as linkonce linkage, except that
  --  unreferenced weak globals may not be discarded. This is used for globals
  --  that may be emitted in multiple translation units, but that are not
  --  guaranteed to be emitted into every translation unit that uses them. One
  --  example of this are common globals in C, such as @int X;@ at global
  --  scope.
  | Weak

  -- | @appending@ linkage may only be applied to global variables of pointer
  --  to array type. When two global variables with appending linkage are
  --  linked together, the two global arrays are appended together. This is
  --  the Llvm, typesafe, equivalent of having the system linker append
  --  together @sections@ with identical names when .o files are linked.
  | Appending

  -- | The semantics of this linkage follow the ELF model: the symbol is weak
  --  until linked, if not linked, the symbol becomes null instead of being an
  --  undefined reference.
  | ExternWeak

  -- | The symbol participates in linkage and can be used to resolve external
  --   symbol references.
  | ExternallyVisible

  -- | Alias for 'ExternallyVisible' but with explicit textual form in LLVM
  --   assembly.
  | External
  deriving (Eq, Show)


instance Pretty LlvmLinkageType where
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

