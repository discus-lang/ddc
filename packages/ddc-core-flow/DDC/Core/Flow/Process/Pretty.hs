
module DDC.Core.Flow.Process.Pretty where
import DDC.Core.Flow.Process.Process
import DDC.Core.Flow.Process.Operator
import DDC.Base.Pretty
import DDC.Type.Pretty          ()


instance Pretty Process where
 ppr p
  = vcat
  $     [ ppr (processName p)
        , text "  result type:   " <> ppr (processResultType p)
        , text "  parameters:    " <> ppr (processParamValues p) ]
        ++ map (indent 2 . ppr) (processOperators p)


instance Pretty Operator where
 ppr op@OpId{}
        = vcat
        [ text "Id"
        , text " rate:   "      <> ppr (opInputRate op)
        , text " input:  "      <> ppr (opInputSeries op)
        , text " result: "      <> ppr (opResultSeries op) ]

 ppr op@OpCreate{}
        = vcat
        [ text "Create"
        , text " rate:   "      <> ppr (opInputRate op)
        , text " input:  "      <> ppr (opInputSeries op)        
        , text " result: "      <> ppr (opResultVector op) ]

 ppr op@OpFill{}
        = vcat
        [ text "Fill"
        , text " target: "      <> ppr (opTargetVector op)
        , text " input:  "      <> ppr (opInputSeries  op) ]

 ppr op@OpMap{}
        = vcat
        [ text "Map"
        , text " rate: "        <> ppr (opInputRate op) ]

 ppr op@OpFold{}
        = vcat
        [ text "Fold"
        , text " rate: "        <> ppr (opInputRate op) ]

 ppr op@OpPack{}
        = vcat
        [ text "Pack"
        , text " input  rate: " <> ppr (opInputRate op) 
        , text " output rate: " <> ppr (opOutputRate op) ]
