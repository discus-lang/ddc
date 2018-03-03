
module DDC.Build.Interface.Codec.Text.Error where
import DDC.Data.Pretty
import qualified DDC.Core.Load                  as Load
import qualified DDC.Core.Discus                as Discus
import qualified DDC.Core.Salt                  as Salt


-- | Problems that can arise when loading an interface file.
data Error
        -- | Empty Interface file.
        = ErrorEmpty

        -- | No module meta information.
        | ErrorNoMeta

        -- | Duplicate module information.
        | ErrorDuplicate

        -- | Bad magic numbers / header information in alleged interface file.
        --   This probably isn't an interface file.
        | ErrorBadMagic
        { errorFilePath :: FilePath
        , errorLine     :: Int }

        -- | Parse error in Interface file.
        | ErrorParse
        { errorFilePath :: FilePath
        , errorLine     :: Int}

        -- | Parser error at end of input.
        | ErrorParseEnd

        -- | Error when loading a tetra core module from the interface file.
        | ErrorLoadTetra FilePath (Load.Error Discus.Name Discus.Error)

        -- | Error when loading a salt  core module from the interface file.
        | ErrorLoadSalt  FilePath (Load.Error  Salt.Name  Salt.Error)


instance Pretty Error where
 ppr ErrorEmpty
  = vcat [ text "Empty interface file." ]

 ppr ErrorNoMeta
  = vcat [ text "No metadata section in interface file." ]

 ppr ErrorDuplicate
  = vcat [ text "Duplicate section in interface file." ]

 ppr (ErrorBadMagic path l)
  = vcat [ text path <> text ":" <> int l
         , text "Bad header in interface file." ]

 ppr (ErrorParse path l)
  = vcat [ text path <> text ":" <> int l
         , text "Parse error in interface file." ]

 ppr ErrorParseEnd
  = vcat [ text "Parse error at end of interface file." ]

 ppr (ErrorLoadTetra path err)
  = vcat [ text path
         , text "Error when loading Tetra module from interface file."
         , indent 2 $ ppr err ]

 ppr (ErrorLoadSalt path err)
  = vcat [ text path
         , text "Error when loading Salt module from interface file."
         , indent 2 $ ppr err ]


