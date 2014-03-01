
module DDC.Core.Llvm.Convert.Super
        (convSuperM)
where
import DDC.Core.Llvm.Convert.Exp
import DDC.Core.Llvm.Convert.Type
import DDC.Core.Llvm.Convert.Erase
import DDC.Core.Llvm.Metadata.Tbaa
import DDC.Core.Llvm.LlvmM
import DDC.Llvm.Syntax
import DDC.Core.Salt.Platform
import DDC.Core.Compounds
import DDC.Type.Env                             (KindEnv, TypeEnv)
import Control.Monad.State.Strict               (gets)
import Data.Set                                 (Set)
import qualified DDC.Core.Salt                  as A
import qualified DDC.Core.Salt.Convert.Name     as A
import qualified DDC.Core.Exp                   as C
import qualified DDC.Type.Env                   as Env
import qualified Data.Set                       as Set
import qualified Data.Sequence                  as Seq
import qualified Data.Foldable                  as Seq


-- | Convert a top-level supercombinator to a LLVM function.
--   Region variables are completely stripped out.
convSuperM 
        :: Set A.Name           -- ^ Names exported from this module.
        -> KindEnv A.Name
        -> TypeEnv A.Name
        -> C.Bind  A.Name       -- ^ Bind of the top-level super.
        -> C.Exp () A.Name      -- ^ Super body.
        -> LlvmM (Function, [MDecl])

convSuperM nsExports kenv tenv bSuper@(C.BName nTop@(A.NameVar strTop) tSuper) x
 | Just (bfsParam, xBody)  <- takeXLamFlags x
 = do   
        platform         <- gets llvmStatePlatform

        -- Sanitise the super name so we can use it as a symbol
        -- in the object code.
        let nTop'       = A.sanitizeGlobal strTop

        -- Add parameters to environments.
        let bfsParam'    = eraseWitBinds bfsParam
        let bsParamType  = [b | (True,  b) <- bfsParam']
        let bsParamValue = [b | (False, b) <- bfsParam']

        let kenv'       =  Env.extends bsParamType  kenv
        let tenv'       =  Env.extends (bSuper : bsParamValue) tenv
        mdsup           <- deriveMD nTop' x

        -- Split off the argument and result types of the super.
        let (tsParam, tResult)   
                        = convertSuperType platform kenv tSuper
  
        -- Make parameter binders.
        let align       = AlignBytes (platformAlignBytes platform)

        -- Declaration of the super.
        let decl 
                = FunctionDecl 
                { declName               = nTop'

                  -- Set internal linkage for non-exported functions so that they
                  -- they won't conflict with functions of the same name that
                  -- might be defined in other modules.
                , declLinkage
                        = if Set.member nTop nsExports
                                then External
                                else Internal

                  -- ISSUE #266: Tailcall optimisation doesn't work for exported functions.
                  --   Using fast calls for non-exported functions enables the
                  --   LLVM tailcall optimisation. We can't enable this for exported
                  --   functions as well because we don't distinguish between DDC
                  --   generated functions and functions from the C libararies in 
                  --   our import specifications. We need a proper FFI system so that
                  --   we can get tailcalls for exported functions as well.
                , declCallConv           
                        = if Set.member nTop nsExports
                                then CC_Ccc
                                else CC_Fastcc

                , declReturnType         = tResult
                , declParamListType      = FixedArgs
                , declParams             = [Param t [] | t <- tsParam]
                , declAlign              = align }

        -- Convert function body to basic blocks.
        label   <- newUniqueLabel "entry"
        blocks  <- convBodyM BodyTop kenv' tenv' mdsup Seq.empty label Seq.empty xBody

        -- Build the function.
        return  $ ( Function
                    { funDecl     = decl
                    , funParams   = [nameOfParam i b 
                                        | i <- [0..]
                                        | b <- bsParamValue]
                    , funAttrs    = [] 
                    , funSection  = SectionAuto
                    , funBlocks   = Seq.toList blocks }
                  , decls mdsup )
                  

convSuperM _ _ _ _ _
        = die "Invalid super"


-- | Take the string name to use for a function parameter.
nameOfParam :: Int -> C.Bind A.Name -> String
nameOfParam i bb
 = case bb of
        C.BName (A.NameVar n) _ 
           -> A.sanitizeName n

        C.BNone _
           -> "_arg" ++ show i

        _  -> die $ "Invalid parameter name: " ++ show bb


