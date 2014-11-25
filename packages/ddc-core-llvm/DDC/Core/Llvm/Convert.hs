
module DDC.Core.Llvm.Convert
        ( convertModule
        , convertType
        , convertSuperType)
where
import DDC.Core.Llvm.Metadata.Tbaa
import DDC.Core.Llvm.Convert.Super
import DDC.Core.Llvm.Convert.Type
import DDC.Core.Llvm.Convert.Exp.Case
import DDC.Core.Llvm.Convert.Exp
import DDC.Core.Llvm.LlvmM
import DDC.Llvm.Syntax
import DDC.Core.Salt.Platform
import DDC.Core.Compounds
import Control.Monad.State.Strict               (evalState)
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
import qualified Data.List                      as List


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
        state   = llvmStateInit prims

        -- Add extra Const and Distinct witnesses where possible.
        --  This helps us produce better LLVM metat data.
        mmElab  = Simp.result 
                $ evalState (Simp.applySimplifier 
                                A.profile Env.empty Env.empty 
                                (Simp.Trans Simp.Elaborate) mm)
                        state

        -- Convert to LLVM.
        --  The result contains ISet and INop meta instructions that need to be 
        --  cleaned out. We also need to fixup the labels in IPhi instructions.
        mmRaw    = evalState (convertModuleM platform mmElab) state

        -- Inline the ISet meta instructions and drop INops.
        --  This gives us code that the LLVM compiler will accept directly.
        mmClean  = Llvm.clean   mmRaw

        -- Fixup the source labels in IPhi instructions.
        --  The converter itself sets these to 'undef', so we need to find the 
        --  real block label of each merged variable.
        mmPhi    = Llvm.linkPhi mmClean

   in   mmPhi


convertModuleM 
        :: Platform
        -> C.Module () A.Name 
        -> LlvmM Module

convertModuleM pp mm@(C.ModuleCore{})
 | ([C.LRec bxs], _)    <- splitXLets $ C.moduleBody mm
 = do   
        -- Globals for the runtime --------------
        --   If this is the main module then we define the globals
        --   for the runtime system at top-level.

        -- Holds the pointer to the current top of the heap.
        --  This is the byte _after_ the last byte used by an object.
        let vHeapTop    = Var (NameGlobal "_DDC__heapTop") (tAddr pp)

        -- Holds the pointer to the maximum heap.
        --  This is the byte _after_ the last byte avaiable in the heap.
        let vHeapMax    = Var (NameGlobal "_DDC__heapMax") (tAddr pp)

        let globalsRts
                | C.moduleName mm == C.ModuleName ["Main"]
                = [ GlobalStatic   vHeapTop (StaticLit (LitInt (tAddr pp) 0))
                  , GlobalStatic   vHeapMax (StaticLit (LitInt (tAddr pp) 0)) ]

                | otherwise
                = [ GlobalExternal vHeapTop 
                  , GlobalExternal vHeapMax ]
        
        -- Import external symbols --------------
        let kenv        = C.moduleKindEnv mm
        let tenv        = C.moduleTypeEnv mm `Env.union` (Env.fromList $ map fst bxs)

        let Just importDecls 
                = sequence
                $ [ importedFunctionDeclOfType pp kenv 
                        isrc
                        (List.lookup n (C.moduleExportValues mm))
                        n
                        (C.typeOfImportSource isrc)
                  | (n, isrc)    <- C.moduleImportValues mm ]


        -- Super-combinator definitions ---------
        --   This is the code for locally defined functions.
        let ctx = Context
                { contextPlatform       = pp
                , contextModule         = mm
                , contextKindEnvTop     = kenv
                , contextTypeEnvTop     = tenv
                , contextSupers         = C.moduleTopBinds mm
                , contextImports        = Set.fromList $ map fst $ C.moduleImportValues mm
                , contextKindEnv        = kenv
                , contextTypeEnv        = tenv
                , contextMDSuper        = MDSuper Map.empty [] 
                , contextSuperBinds     = Map.empty
                , contextConvertBody    = convertBody
                , contextConvertExp     = convertExp
                , contextConvertCase    = convertCase }


        (functions, mdecls)
                <- liftM unzip 
                $ mapM (uncurry (convertSuper ctx)) bxs
        

        -- Paste everything together ------------
        return  $ Module 
                { modComments   = []
                , modAliases    = [aObj pp]
                , modGlobals    = globalsRts
                , modFwdDecls   = primDecls pp ++ importDecls 
                , modFuncs      = functions 
                , modMDecls     = concat mdecls }

 | otherwise    = die "Invalid module"


-- | C library functions that are used directly by the generated code without
--   having an import declaration in the header of the converted module.
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

