
module DDC.Core.Llvm.Convert
        ( convertModule
        , convertType
        , convertSuperType)
where
import DDC.Core.Llvm.Metadata.Tbaa
import DDC.Core.Llvm.Convert.Exp.Case
import DDC.Core.Llvm.Convert.Exp
import DDC.Core.Llvm.Convert.Super
import DDC.Core.Llvm.Convert.Type
import DDC.Core.Llvm.Convert.Base
import DDC.Core.Llvm.Runtime
import DDC.Core.Salt.Platform
import DDC.Core.Exp.Annot                       as A
import DDC.Llvm.Syntax
import DDC.Control.Check
import qualified Control.Monad.State.Strict     as State
import Control.Monad
import Data.Map                                 (Map)

import qualified DDC.Llvm.Transform.Calls       as Calls
import qualified DDC.Llvm.Transform.Flatten     as Flatten
import qualified DDC.Llvm.Transform.Simpl       as Simpl

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
convertModule 
        :: Platform 
        -> C.Module () A.Name 
        -> Either Error Module

convertModule platform mm@(C.ModuleCore{})
 = let  
        state   = llvmStateInit 

        -- Add extra Const and Distinct witnesses where possible.
        --  This helps us produce better LLVM metadata.
        mmElab  = Simp.result $ fst
                $ flip State.runState state
                $ Simp.applySimplifier 
                        A.profile Env.empty Env.empty 
                        (Simp.Trans Simp.Elaborate)
                        mm
                        
        -- Convert to LLVM.
   in   case runCheck state (convertModuleM platform mmElab) of
         (_state', Left  err)
          -> Left err

         (state', Right mmRaw)
          -> let 
                -- Attach any top-level constants the code generator might have made.
                gsLit    = [ GlobalStatic var (StaticLit lit) 
                           | (var, lit) <- Map.toList $ llvmConstants state' ]

                mmConst  = mmRaw
                         { modGlobals = modGlobals mmRaw ++ gsLit }

                -- Flatten out our extended expression language into raw LLVM instructions.
                mmFlat   = Flatten.flatten mmConst

                -- Clean out nops, v1 = v2 aliases and constant bindings.
                mmSimpl  = Simpl.simpl Simpl.configZero
                                { Simpl.configDropNops    = True
                                , Simpl.configSimplAlias  = True
                                , Simpl.configSimplConst  = True 
                                , Simpl.configSquashUndef = True }
                                mmFlat

                -- Attach calling conventions to call sites.
                mmCalls  = Calls.attachCallConvs mmSimpl

             in Right mmCalls


-- | Convert a Salt module to sugared LLVM.
--   The result contains ISet and INop meta-instructions that LLVM will not accept
--   directly. It also annotates IPhi nodes with undef, and these need to be given
--   real block labels before the LLVM compiler will accept them.
--
convertModuleM 
        :: Platform
        -> C.Module () A.Name 
        -> ConvertM Module

convertModuleM pp mm@(C.ModuleCore{})
 | ([C.LRec bxs], _)    <- splitXLets $ C.moduleBody mm
 = do   
        -- Globals for the runtime --------------
        --   If this is the main module then we define the globals for the
        --   runtime system, otherwise tread them as external symbols.

        -- Holds the pointer to the current top of the heap.
        --  This is the byte _after_ the last byte used by an object.
        let vHeapTop    = Var nameGlobalHeapTop (tAddr pp)

        -- Holds the pointer to the maximum heap.
        --  This is the byte _after_ the last byte avaiable in the heap.
        let vHeapMax    = Var nameGlobalHeapMax (tAddr pp)

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

        let Just msImportDecls 
                = sequence
                $ [ importedFunctionDeclOfType pp kenv 
                        isrc
                        (List.lookup n (C.moduleExportValues mm))
                        n
                        (C.typeOfImportValue isrc)
                  | (n, isrc)    <- C.moduleImportValues mm ]

        importDecls <- sequence msImportDecls

        -- Convert super definitions ---------
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
                , contextNames          = Map.empty
                , contextMDSuper        = MDSuper Map.empty [] 
                , contextSuperBinds     = Map.empty
                , contextPrimDecls      = primDeclsMap pp
                , contextConvertBody    = convertBody
                , contextConvertExp     = convertSimple
                , contextConvertCase    = convertCase }

        let convertSuper' ctx' b x
                = let Right x'  = A.fromAnnot x
                  in  convertSuper ctx' b x'

        (functions, mdecls)
                <- liftM unzip 
                $  mapM (uncurry (convertSuper' ctx)) bxs

        -- Stitch everything together -----------
        return  $ Module 
                { modComments           = []
                , modAliases            = [aObj pp]
                , modGlobals            = globalsRts
                , modFwdDecls           = primDecls pp ++ importDecls 
                , modFuncs              = functions 
                , modMDecls             = concat mdecls }

 | otherwise    
 = throw $ ErrorInvalidModule mm


-- | C library functions that are used directly by the generated code without
--   having an import declaration in the header of the converted module.
primDeclsMap :: Platform -> Map String FunctionDecl
primDeclsMap pp 
        = Map.fromList
        $ [ (declName decl, decl) | decl <- primDecls pp ]

primDecls :: Platform -> [FunctionDecl]
primDecls pp 
 = [    FunctionDecl
        { declName              = textOfName nameGlobalMalloc
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

