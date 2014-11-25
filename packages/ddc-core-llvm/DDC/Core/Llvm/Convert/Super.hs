
module DDC.Core.Llvm.Convert.Super
        (convertSuper)
where
import DDC.Core.Llvm.Convert.Exp
import DDC.Core.Llvm.Convert.Type
import DDC.Core.Llvm.Convert.Erase
import DDC.Core.Llvm.Convert.Context
import DDC.Core.Llvm.LlvmM
import DDC.Llvm.Syntax
import DDC.Core.Salt.Platform
import DDC.Core.Compounds
import DDC.Base.Pretty                          hiding (align)
import qualified DDC.Type.Env                   as Env
import qualified DDC.Core.Llvm.Metadata.Tbaa    as Tbaa
import qualified DDC.Core.Salt                  as A
import qualified DDC.Core.Salt.Convert          as A
import qualified DDC.Core.Module                as C
import qualified DDC.Core.Exp                   as C
import qualified Data.Set                       as Set
import qualified Data.Sequence                  as Seq
import qualified Data.Foldable                  as Seq


-- | Convert a top-level supercombinator to a LLVM function.
--   Region variables are completely stripped out.
convertSuper
        :: Context
        -> C.Bind   A.Name      -- ^ Bind of the top-level super.
        -> C.Exp () A.Name      -- ^ Super body.
        -> LlvmM (Function, [MDecl])

convertSuper ctx (C.BName nSuper tSuper) x
 | Just (bfsParam, xBody)  <- takeXLamFlags x
 = do   
        let pp          = contextPlatform ctx
        let mm          = contextModule   ctx
        let kenv        = contextKindEnv  ctx

        -- Names of exported values.
        let nsExports   = Set.fromList $ map fst $ C.moduleExportValues mm

        -- Sanitise the super name so we can use it as a symbol
        -- in the object code.
        let Just nSuper' = A.seaNameOfSuper
                                (lookup nSuper (C.moduleImportValues mm))
                                (lookup nSuper (C.moduleExportValues mm))
                                nSuper

        -- Add parameters to environments.
        let bfsParam'    = eraseWitBinds bfsParam
        let bsParamType  = [b | (True,  b) <- bfsParam']
        let bsParamValue = [b | (False, b) <- bfsParam']

        mdsup     <- Tbaa.deriveMD (renderPlain nSuper') x
        let ctx'  = ctx
                  { contextKindEnv      = Env.extends bsParamType  $ contextKindEnv ctx
                  , contextTypeEnv      = Env.extends bsParamValue $ contextTypeEnv ctx 
                  , contextMDSuper      = mdsup }

        -- Convert function body to basic blocks.
        label     <- newUniqueLabel "entry"
        blocks    <- convertBody ctx' ExpTop Seq.empty label Seq.empty xBody

        -- Split off the argument and result types of the super.
        let (tsParam, tResult)   
                  = convertSuperType pp kenv tSuper
  
        -- Make parameter binders.
        let align = AlignBytes (platformAlignBytes pp)

        -- Declaration of the super.
        let decl 
                = FunctionDecl 
                { declName              = renderPlain nSuper'

                  -- Set internal linkage for non-exported functions so that they
                  -- they won't conflict with functions of the same name that
                  -- might be defined in other modules.
                , declLinkage           = if Set.member nSuper nsExports
                                                then External
                                                else Internal

                  -- ISSUE #266: Tailcall optimisation doesn't work for exported functions.
                  --   Using fast calls for non-exported functions enables the
                  --   LLVM tailcall optimisation. We can't enable this for exported
                  --   functions as well because we don't distinguish between DDC
                  --   generated functions and functions from the C libararies in 
                  --   our import specifications. We need a proper FFI system so that
                  --   we can get tailcalls for exported functions as well.
                , declCallConv          = if Set.member nSuper nsExports
                                                then CC_Ccc
                                                else CC_Fastcc

                , declReturnType         = tResult
                , declParamListType      = FixedArgs
                , declParams             = [Param t [] | t <- tsParam]
                , declAlign              = align }


        -- Build the function.
        return  ( Function
                  { funDecl     = decl
                  , funParams   = [nameOfParam i b 
                                      | i <- [0..]
                                      | b <- bsParamValue]
                  , funAttrs    = [] 
                  , funSection  = SectionAuto
                  , funBlocks   = Seq.toList blocks }
                , Tbaa.decls mdsup ) 

convertSuper _ _ _
        = die "Invalid super"


-- | Take the string name to use for a function parameter.
nameOfParam :: Int -> C.Bind A.Name -> String
nameOfParam i bb
 = case bb of
        C.BName nm _ 
           | Just n <- A.takeNameVar nm
           -> A.sanitizeName n

        C.BNone _
           -> "_arg" ++ show i

        _  -> die $ "Invalid parameter name: " ++ show bb


