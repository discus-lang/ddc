
module DDC.Driver.Config
        ( Config        (..)
        , ViaBackend    (..))
where
import DDC.Build.Builder                        
import DDC.Core.Simplifier                      (Simplifier)

import qualified DDC.Core.Salt.Runtime          as Salt
import qualified DDC.Core.Salt                  as Salt

import qualified DDC.Core.Lite                  as Lite


-- | Configuration for main compiler stages.
data Config
        = Config
        { -- | Dump intermediate code.
          configDump                    :: Bool

          -- | Use bidirectional type inference on the input code.
        , configInferTypes              :: Bool

          -- | Simplifiers to apply to intermediate code
        , configSimplLite               :: Simplifier Int () Lite.Name
        , configSimplSalt               :: Simplifier Int () Salt.Name

          -- | Backend code generator to use
        , configViaBackend              :: ViaBackend

          -- | Runtime system configuration
        , configRuntime                 :: Salt.Config

          -- | The builder to use for the target architecture
        , configBuilder                 :: Builder

          -- | Suppress imports in Core modules
        , configSuppressCoreImports     :: Bool

          -- | Suppress the #import prelude in C modules
        , configSuppressHashImports     :: Bool 

          -- | Override output file
        , configOutputFile              :: Maybe FilePath

          -- | Override directory for build products
        , configOutputDir               :: Maybe FilePath

          -- | Keep intermediate .ddc.ll files
        , configKeepLlvmFiles           :: Bool

          -- | Keep intermediate .ddc.c files
        , configKeepSeaFiles            :: Bool

          -- | Keep intermediate .ddc.s files
        , configKeepAsmFiles            :: Bool

          -- | Avoid running the type checker where possible.
          --   When debugging program transformations, use this to get
          --   the invalid code rather than just the type error message.
        , configTaintAvoidTypeChecks    :: Bool
        }


data ViaBackend
        -- | Compile via the C backend.
        = ViaC

        -- | Compile via the LLVM backend.
        | ViaLLVM
        deriving Show

