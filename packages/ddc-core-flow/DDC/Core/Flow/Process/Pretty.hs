
module DDC.Core.Flow.Process.Pretty where
import DDC.Core.Flow.Process.Process
import DDC.Core.Flow.Process.Operator
import DDC.Base.Pretty
import DDC.Type.Pretty          ()


instance Pretty Process where
 ppr p
  = vcat
  $     [ ppr (processName p)
        , text "  element type:  " <> ppr (processType p)
        , text "  parameters:    " <> ppr (processParamValues p) ]
        ++ map (indent 2 . ppr) (processOperators p)


instance Pretty Operator where
 ppr op@OpMap{}
        = vcat
        [ text "Map"
        , text " rate: "        <> ppr (opRate op) ]

 ppr op@OpFold{}
        = vcat
        [ text "Fold"
        , text " rate: "        <> ppr (opRate op) ]
