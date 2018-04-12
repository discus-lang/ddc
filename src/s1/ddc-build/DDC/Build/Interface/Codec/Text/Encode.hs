
module DDC.Build.Interface.Codec.Text.Encode
        (encodeInterface)
where
import DDC.Build.Interface.Base
import DDC.Core.Codec.Text.Pretty
import qualified Data.Text      as T


-- | Encode a module interface as a human readable text string.
encodeInterface :: Interface ta sa -> T.Text
encodeInterface i
  = T.pack $ renderIndent
  $ (text "ddc interface" <+> text (interfaceVersion i))
         <> line <> line
         <> vcat [ text $ makeInterfaceTearLine "Meta"
                 , text "module-meta"   <+> lbrace <> line
                        <> indent 8 (vcat
                                [ hsep [text "name:", ppr $ interfaceModuleName i ] ])
                        <> line <> rbrace]

        -- Include the full unoptimised core code in interface files.
        --   We include the unoptimised code so that the modules that inline
        --   functions from this one are not sensitive to the optimisation flags
        --   use here. If we included the optimised code it would make it harder
        --   to reason about the overall compilation process.
        <> (case interfaceDiscusModule i of
                Just m  -> vcat [ line
                                , text $ makeInterfaceTearLine "Tetra"
                                , ppr m ]
                Nothing -> mempty)


-- | Tear line to separate sections of the interface file.
--   We use '~' because it's not used in the core language syntax.
makeInterfaceTearLine :: String -> String
makeInterfaceTearLine name
        = "~~ " ++ name ++ " " ++ replicate (80 - 4 - length name) '~'

