{-# LANGUAGE OverloadedStrings #-}
module DDC.Build.Platform.Error where
import DDC.Data.Pretty


-- | Things that can go wrong when determining the build platform.
--
--   We want to be very specific why we can't determine the build platform
--   as this is a complete blocker to people getting started with the project.
--
data Error
        = ErrorToolMissing
        { errorTool             :: String }

        | ErrorToolFailed
        { errorCmd              :: FilePath
        , errorOut              :: String
        , errorErr              :: String }

        | ErrorUnameUnknownArch
        { errorCmd              :: String
        , errorOut              :: String }

        | ErrorUnameUnknownOs
        { errorCmd              :: String
        , errorOut              :: String }

        | ErrorUnameUnknownDarwinVersion
        { errorCmd              :: String
        , errorOut              :: String }

        | ErrorLlvmConfigMissing
        { errorExtraPath        :: Maybe FilePath
        , errorNames            :: [String] }

        | ErrorLlvmConfigUnexpectedOutput
        { errorCmd              :: String
        , errorOut              :: String }
        deriving (Eq, Show)


instance Pretty Error where
 ppr (ErrorToolMissing tool)
  = text "Cannot find command line tool" %% dquotes (string tool) %% text "."

 ppr (ErrorToolFailed cmd out err)
  = vcat
  [ text "Execution of command line tool failed."
  , text " Command was:    " %% squotes (string $ show cmd)
  , text " stdout response:" %% pprDump out
  , text " stderr response:" %% pprDump err ]

 ppr (ErrorUnameUnknownArch cmd out)
  = vcat
  [ text "Unrecognized processor architecture reported by 'uname' command line tool."
  , text " Command was:    " %% squotes (string $ show cmd)
  , text " stdout response:" %% pprDump out ]

 ppr (ErrorUnameUnknownOs cmd out)
  = vcat
  [ text "Unrecognized operating system reported by 'uname' command line tool."
  , text " Command was:    " %% squotes (string $ show cmd)
  , text " stdout response:" %% pprDump out ]

 ppr (ErrorUnameUnknownDarwinVersion cmd out)
  = vcat
  [ text "Unrecognized OSX/Darwin version reported by 'uname' command line tool."
  , text " Command was:    " %% squotes (string $ show cmd)
  , text " stdout response:" %% pprDump out ]

 ppr (ErrorLlvmConfigMissing mFilePath names)
  = vcat
  $  [ text "Cannot find a known version of the 'llvm-config' command line tool."
     , text " Looked for: " %% (string $ hsep $ map (string . show) names) ]
  ++ case mFilePath of
        Nothing   -> []
        Just path -> [text " Even at supplied path " % string path]

 ppr (ErrorLlvmConfigUnexpectedOutput cmd out)
  = vcat
  [ text "Unrecognized output from 'llvm-config' command line tool."
  , text " Command was:    " %% dquotes (string $ show cmd)
  , text " stdout response:" %% pprDump out ]


pprDump :: String -> Doc
pprDump out
 = case lines out of
        []      -> string "(empty)"
        [l]
         |  length l < 50
         -> dquotes $ string $ show l

        l : _   -> dquotes $ (string $ show $ take 50 l) % text "..."

