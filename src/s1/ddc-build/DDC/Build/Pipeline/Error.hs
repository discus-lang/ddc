{-# OPTIONS_HADDOCK hide #-}
module DDC.Build.Pipeline.Error
        (Error (..))
where
import DDC.Data.Pretty
import DDC.Data.SourcePos
import qualified DDC.Core.Salt          as Salt
import qualified DDC.Core.Load          as CL
import qualified DDC.Data.Pretty        as P
import Control.DeepSeq


data Error
        -- | Error when loading a module.
        --   Blame it on the user.
        = forall err. Pretty err => ErrorLoad !err

        -- | Error when type checking a transformed module.
        --   Blame it on the compiler.
        | forall err. Pretty err => ErrorLint String String !err

        | forall err. Pretty (err (CL.AnTEC SourcePos Salt.Name))
        => ErrorSaltLoad  (CL.Error Salt.Name err)

        -- | Error converting the module to Salt to Sea.
        | forall err
           .  Pretty err
           => ErrorSaltConvert String !err

        -- | Error converting the module from Discus to Salt.
        | forall err. Pretty err => ErrorDiscusConvert !err

        -- | Error converting the module from Discus to Salt.
        | forall err. Pretty err => ErrorFlowConvert  !err

        -- | Error when transforming core program.
        | forall err. Pretty err => ErrorCoreTransform !err


instance Show Error where
 show err
  = P.renderIndent $ P.ppr err


instance Pretty Error where
 ppr err
  = case err of
        ErrorLoad err'
         -> vcat [ text "Error loading module"
                 , indent 2 (ppr err') ]

        ErrorLint stage pipe err'
         -> vcat [ text "Error in transformed module."
                 , text "  in stage " <> string stage
                 , text "  in pipe  " <> string pipe
                 , indent 2 (ppr err') ]

        ErrorSaltLoad err'
         -> vcat [ text "Type error when loading Salt module."
                 , indent 2 (ppr err') ]

        ErrorSaltConvert sStage err'
         -> vcat [ text "Fragment violation when converting Salt module to target code."
                 , indent 2 (string ("stage: " ++ sStage))
                 , indent 2 (ppr err') ]

        ErrorDiscusConvert err'
         -> vcat [ text "Fragment violation when converting Discus module to Salt module."
                 , indent 2 (ppr err') ]

        ErrorFlowConvert err'
         -> vcat [ text "Fragment violation when converting Flow module to Discus module."
                 , indent 2 (ppr err') ]

        ErrorCoreTransform err'
         -> vcat [ text "Error transforming core program."
                 , indent 2 (ppr err') ]


instance NFData Error where
 rnf !_ = ()

