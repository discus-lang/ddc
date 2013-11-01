
module DDC.Build.Pipeline.Error
        (Error (..)) 
where
import DDC.Base.Pretty
import qualified DDC.Core.Salt          as Salt
import qualified DDC.Core.Load          as CL
import Control.DeepSeq


data Error
        = ErrorSaltLoad    (CL.Error Salt.Name)

        -- | Error converting the module to Disciple Core Salt.
        | forall err. Pretty err => ErrorSaltConvert !err

        -- | Error converting the module to Disciple Core Lite.
        | forall err. Pretty err => ErrorLiteConvert !err

        -- | Error when transforming core program.
        | forall err. Pretty err => ErrorCoreTransform !err

        -- | Error when loading a module.
        --   Blame it on the user.
        | forall err. Pretty err => ErrorLoad !err

        -- | Error when type checking a transformed module.
        --   Blame it on the compiler.
        | forall err. Pretty err => ErrorLint !err


instance Pretty Error where
 ppr err
  = case err of
        ErrorSaltLoad err'
         -> vcat [ text "Type error when loading Salt module."
                 , indent 2 (ppr err') ]

        ErrorSaltConvert err'
         -> vcat [ text "Fragment violation when converting Salt module to C code."
                 , indent 2 (ppr err') ]

        ErrorCoreTransform err'
         -> vcat [ text "Error transforming core program."
                 , indent 2 (ppr err') ]

        ErrorLiteConvert err'
         -> vcat [ text "Fragment violation when converting Lite module to Salt module."
                 , indent 2 (ppr err') ]

        ErrorLoad err'
         -> vcat [ text "Error loading module"
                 , indent 2 (ppr err') ]

        ErrorLint err'
         -> vcat [ text "Error in transformed module."
                 , indent 2 (ppr err') ]

instance NFData Error

