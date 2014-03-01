
module DDC.Core.Llvm.Convert
        ( convertModule
        , convertType
        , convertSuperType)
where
import DDC.Core.Llvm.Convert.Super
import DDC.Core.Llvm.Convert.Type
import DDC.Core.Llvm.LlvmM
import DDC.Llvm.Syntax
import DDC.Core.Salt.Platform
import DDC.Core.Compounds
import Control.Monad.State.Strict               (evalState)
import Control.Monad.State.Strict               (gets)
import Control.Monad
import Data.Map                                 (Map)
import qualified DDC.Llvm.Transform.Clean       as Llvm
import qualified DDC.Llvm.Transform.LinkPhi     as Llvm
import qualified DDC.Core.Salt                  as A
import qualified DDC.Core.Module                as C
import qualified DDC.Core.Exp                   as C
import qualified DDC.Type.Env                   as Env
import qualified DDC.Core.Simplifier            as Simp
import qualified Data.Map                       as Map
import qualified Data.Set                       as Set


-- | Convert a Salt module to LLVM.
-- 
--   If anything goes wrong in the convertion then this function will
--   just call `error`.
--
convertModule :: Platform -> C.Module () A.Name -> Module
convertModule platform mm@(C.ModuleCore{})
 = {-# SCC convertModule #-}
   let  
        prims   = primDeclsMap platform
        state   = llvmStateInit platform prims

        -- Add extra Const and Distinct witnesses where possible.
        --  This helps us produce better LLVM metat data.
        mmElab   = Simp.result $ evalState (Simp.applySimplifier 
                                        A.profile Env.empty Env.empty 
                                        (Simp.Trans Simp.Elaborate) mm)
                          state

        -- Convert to LLVM.
        --  The result contains ISet and INop meta instructions that need to be 
        --  cleaned out. We also need to fixup the labels in IPhi instructions.
        mmRaw    = evalState (convModuleM mmElab) state

        -- Inline the ISet meta instructions and drop INops.
        --  This gives us code that the LLVM compiler will accept directly.
        mmClean  = Llvm.clean   mmRaw

        -- Fixup the source labels in IPhi instructions.
        --  The converter itself sets these to 'undef', so we need to find the 
        --  real block label of each merged variable.
        mmPhi    = Llvm.linkPhi mmClean

   in   mmPhi


convModuleM :: C.Module () A.Name -> LlvmM Module
convModuleM mm@(C.ModuleCore{})
 | ([C.LRec bxs], _)    <- splitXLets $ C.moduleBody mm
 = do   platform        <- gets llvmStatePlatform

        -- The initial environments due to imported names.
        let kenv        = C.moduleKindEnv mm
        let tenv        = C.moduleTypeEnv mm `Env.union` (Env.fromList $ map fst bxs)

        -- Names of exported functions.
        --   We use a different linkage for exported functions.
        let nsExports   = Set.fromList $ map fst $ C.moduleExportValues mm

        -- Forward declarations for imported functions.
        let Just importDecls 
                = sequence
                $ [ importedFunctionDeclOfType platform kenv External 
                        src (C.typeOfImportSource src)
                  | (_, src)    <- C.moduleImportValues mm ]

        -- Add RTS def -------------------------------------------------
        -- If this is the main module then we need to declare
        -- the global RTS state.
        let isMainModule 
                = C.moduleName mm == C.ModuleName ["Main"]

        -- Holds the pointer to the current top of the heap.
        --  This is the byte _after_ the last byte used by an object.
        let vHeapTop    = Var (NameGlobal "_DDC_Runtime_heapTop") (tAddr platform)

        -- Holds the pointer to the maximum heap.
        --  This is the byte _after_ the last byte avaiable in the heap.
        let vHeapMax    = Var (NameGlobal "_DDC_Runtime_heapMax") (tAddr platform)

        let rtsGlobals
                | isMainModule
                = [ GlobalStatic   vHeapTop (StaticLit (LitInt (tAddr platform) 0))
                  , GlobalStatic   vHeapMax (StaticLit (LitInt (tAddr platform) 0)) ]

                | otherwise
                = [ GlobalExternal vHeapTop 
                  , GlobalExternal vHeapMax ]

        ---------------------------------------------------------------
        (functions, mdecls)
                <- liftM unzip 
                $ mapM (uncurry (convSuperM nsExports kenv tenv)) bxs
        
        return  $ Module 
                { modComments   = []
                , modAliases    = [aObj platform]
                , modGlobals    = rtsGlobals
                , modFwdDecls   = primDecls platform ++ importDecls 
                , modFuncs      = functions 
                , modMDecls     = concat mdecls }

 | otherwise    = die "Invalid module"


-- | Global variables used directly by the converted code.
primDeclsMap :: Platform -> Map String FunctionDecl
primDeclsMap pp 
        = Map.fromList
        $ [ (declName decl, decl) | decl <- primDecls pp ]

primDecls :: Platform -> [FunctionDecl]
primDecls pp 
 = [    FunctionDecl
        { declName              = "malloc"
        , declLinkage           = External
        , declCallConv          = CC_Ccc
        , declReturnType        = tAddr pp
        , declParamListType     = FixedArgs
        , declParams            = [Param (tNat pp) []]
        , declAlign             = AlignBytes (platformAlignBytes pp) }

   ,    FunctionDecl
        { declName              = "abort"
        , declLinkage           = External
        , declCallConv          = CC_Ccc
        , declReturnType        = TVoid
        , declParamListType     = FixedArgs
        , declParams            = []
        , declAlign             = AlignBytes (platformAlignBytes pp) } ]

