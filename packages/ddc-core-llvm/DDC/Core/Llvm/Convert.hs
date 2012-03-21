
module DDC.Core.Llvm.Convert
        (convertModule)
where
import DDC.Llvm.Attr
import DDC.Llvm.Instr
import DDC.Llvm.Function
import DDC.Llvm.Module
import DDC.Core.Llvm.Runtime.Alloc
import DDC.Core.Llvm.Runtime.Object
import DDC.Core.Llvm.Convert.Type
import DDC.Core.Llvm.Platform
import DDC.Core.Llvm.LlvmM
import DDC.Core.Compounds
import DDC.Core.Sea.Output.Name
import DDC.Type.Compounds
import Data.Sequence                            (Seq, (|>), (><))
import Data.Map                                 (Map)
import qualified DDC.Core.Sea.Output.Env        as E
import qualified DDC.Base.Pretty                as P
import qualified DDC.Core.Module                as C
import qualified DDC.Core.Exp                   as C
import qualified Data.Map                       as Map
import qualified Data.Sequence                  as Seq
import qualified Data.Foldable                  as Seq
import Control.Monad.State.Strict               (evalState)
import Control.Monad.State.Strict               (gets)


-- Module ---------------------------------------------------------------------
convertModule :: C.Module () Name -> Module
convertModule mm
 = let  platform        = platform32
        prims           = primGlobals platform
        state           = llvmStateInit platform prims
   in   evalState (llvmOfModuleM mm) state


llvmOfModuleM 
        :: C.Module () Name 
        -> LlvmM Module

llvmOfModuleM mm@(C.ModuleCore{})
 | [C.LRec bxs]         <- C.moduleLets mm   
 = do   platform        <- gets llvmStatePlatform
        functions       <- mapM (uncurry (llvmFunctionOfSuper)) bxs
        return  $ Module 
                { modComments   = []
                , modAliases    = [aObj platform]
                , modGlobals    = []
                , modFwdDecls   = []
                , modFuncs      = functions }

 | otherwise    = die "invalid module"


primGlobals :: Platform -> Map String Var
primGlobals platform
        = Map.fromList
        [ ("malloc", VarGlobal "malloc" 
                        (convType platform (E.tNat `tFunPE` E.tPtr E.tObj))
                        External SectionAuto AlignNone True) ]


-- Super ----------------------------------------------------------------------
-- | Convert a top-level supercombinator to LLVM.
llvmFunctionOfSuper 
        :: C.Bind Name 
        -> C.Exp () Name 
        -> LlvmM Function

llvmFunctionOfSuper (C.BName n tSuper) x
 | Just (bsParam, xBody)     <- takeXLams x
 = do   platform        <- gets llvmStatePlatform

        -- Split off the argument and result types.
        let (tsArgs, tResult)       
                = takeTFunArgResult tSuper

        let params      = map (llvmParameterOfType platform) tsArgs
        let align       = AlignBytes (platformFunctionAlignBytes platform)

        -- Declaration of the super.
        let decl 
                = FunctionDecl 
                { declName               = P.renderPlain $ P.ppr n
                , declLinkage            = External
                , declCallConv           = CC_Ccc
                , declReturnType         = convType platform tResult
                , declParamListType      = FixedArgs
                , declParams             = params
                , declAlign              = align }

        blockId <- newUniqueBlockId
        blocks  <- llvmBlocksOfBody Seq.empty blockId Seq.empty xBody

        return  $ Function
                { functionDecl           = decl
                , functionParams         = map llvmNameOfParam bsParam
                , functionAttrs          = [] 
                , functionSection        = SectionAuto
                , functionBlocks         = Seq.toList blocks }

llvmFunctionOfSuper _ _
        = die "invalid super"


-- | Take the string name to use for a function parameter.
llvmNameOfParam :: C.Bind Name -> String
llvmNameOfParam bb
 = case bb of
        C.BName n _     -> P.renderPlain $ P.ppr n
        _               -> die "invalid parameter name"


-- Body -----------------------------------------------------------------------
llvmBlocksOfBody 
        :: Seq Block            -- ^ Previous blocks.
        -> BlockId              -- ^ Id of current block.
        -> Seq Instr            -- ^ Instrs in current block.
        -> C.Exp () Name        -- ^ Expression being converted.
        -> LlvmM (Seq Block)    -- ^ Final blocks of function body.

llvmBlocksOfBody blocks blockId instrs xx
 = do   platform        <- gets llvmStatePlatform
        case xx of

         -- End of function body must explicitly pass control.
         C.XApp{}
          |  Just (NamePrim p, xs)         <- takeXPrimApps xx
          ,  PrimControl PrimControlReturn <- p
          ,  [C.XType t, x]                <- xs
          ,  Just v                        <- takeVarOfTypedExp platform t x
          -> return  $   blocks 
                     |>  Block blockId (instrs |> IReturn (Just v))

         -- Variable assignment.
         C.XLet _ (C.LLet C.LetStrict (C.BName (NameVar str) t) x1) x2
          -> do t'       <- convTypeM t
                let dst  = VarLocal str t'
                instrs'  <- llvmGetResult platform dst x1
                llvmBlocksOfBody blocks blockId (instrs >< instrs') x2


         -- TODO: Debugging only
         _ -> return $  blocks
                     |> Block blockId (instrs |> IUnreachable)

         -- die "invalid body statement"

takeVarOfTypedExp :: Platform -> C.Type Name -> C.Exp () Name -> Maybe Var
takeVarOfTypedExp platform t xx
 = case xx of
        C.XVar _ (C.UName (NameVar s) _)    
          -> Just $ VarLocal s (convType platform t)

        _ -> Nothing


-- Exp ------------------------------------------------------------------------
llvmGetResult
        :: Platform
        -> Var  
        -> C.Exp () Name
        -> LlvmM (Seq Instr)

llvmGetResult _  dst (C.XVar _ (C.UName (NameVar str) t))
 = do   t'      <- convTypeM t
        return  $ Seq.singleton $ IStore dst (VarLocal str t')

llvmGetResult pp dst xx@C.XApp{}
        -- Primitive operation.
        | Just (NamePrim p, args)       <- takeXPrimApps xx
        = convPrimCallM dst p args

        -- Super call.
        | (xFun : xsArgs) <- takeXApps xx
        , Just vFun       <- takeVar pp xFun
        , Just vsArgs     <- sequence $ map (takeVar pp) xsArgs
        = return 
                $ Seq.singleton
                $ ICall dst StdCall vFun vsArgs []

llvmGetResult _ _ xx
        = return $ Seq.singleton 
        $ IComment [ "llvmGetResult: cannot convert " ++ show xx ]


-- | Convert a primitive call to LLVM.
convPrimCallM 
        :: Show a 
        => Var 
        -> Prim 
        -> [C.Exp a Name] 
        -> LlvmM (Seq Instr)

convPrimCallM dst p xs
 = case p of
        PrimStore (PrimStoreAllocData PrimStoreLayoutRaw)
         | [xTag, xSize]        <- xs
         , Just tag             <- takeLitTag xTag
         , Just size            <- takeLitNat xSize
         -> allocDataRaw dst tag size



        _ -> return $ Seq.singleton 
          $ IComment ["convPrimCallM: cannot convert " ++ show (p, xs)]


takeVar :: Platform -> C.Exp a Name -> Maybe Var
takeVar pp xx
 = case xx of
        C.XVar _ (C.UName (NameVar str) t)
          -> Just $ VarLocal str (convType pp t)
        _ -> Nothing


takeLitTag :: C.Exp a Name -> Maybe Integer
takeLitTag xx
 = case xx of
        C.XCon _ (C.UPrim (NameTag tag) _)      -> Just tag
        _                                       -> Nothing

takeLitNat :: C.Exp a Name -> Maybe Integer
takeLitNat xx
 = case xx of
        C.XCon _ (C.UPrim (NameNat nat) _)      -> Just nat
        _                                       -> Nothing

