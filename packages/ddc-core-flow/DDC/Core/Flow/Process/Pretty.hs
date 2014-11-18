
module DDC.Core.Flow.Process.Pretty where
import DDC.Core.Flow.Process.Process
import DDC.Core.Flow.Process.Operator
import DDC.Base.Pretty
import DDC.Core.Pretty          ()


instance Pretty Process where
 ppr p
  = vcat
  $     [ ppr (processName p)
        , text "  parameters:    " <> ppr (processParamValues p) ]
        ++ map (indent 2 . ppr) (processOperators p)


instance Pretty Operator where
 ppr op@OpId{}
        = vcat
        [ text "Id"
        , text " rate:    "     <> ppr (opInputRate     op)
        , text " input:   "     <> ppr (opInputSeries   op)
        , text " result:  "     <> ppr (opResultSeries  op) ]

 ppr op@OpRep{}
        = vcat
        [ text "Rep"
        , text " result:      " <> ppr (opResultSeries  op)
        , text " output rate: " <> ppr (opOutputRate    op)
        , text " type:        " <> ppr (opElemType      op) ]

 ppr op@OpReps{}
        = vcat
        [ text "Reps"
        , text " result:      " <> ppr (opResultSeries  op)
        , text " input rate:  " <> ppr (opInputRate     op)
        , text " output rate: " <> ppr (opOutputRate    op)
        , text " type:        " <> ppr (opElemType      op)
        , text " segd:        " <> ppr (opSegdBound     op)
        , text " input:       " <> ppr (opInputSeries   op) ]
 
 ppr op@OpIndices{}
        = vcat
        [ text "Indices"
        , text " result:      " <> ppr (opResultSeries  op)
        , text " input rate:  " <> ppr (opInputRate     op)
        , text " output rate: " <> ppr (opOutputRate    op) ]

 ppr op@OpFill{}
        = vcat
        [ text "Fill"
        , text " target:  "     <> ppr (opTargetVector  op)
        , text " input:   "     <> ppr (opInputSeries   op) ]

 ppr op@OpGather{}
        = vcat
        [ text "Gather"
        , text " result:  "     <> ppr (opResultBind    op)
        , text " vector:  "     <> ppr (opSourceVector  op)
        , text " indices: "     <> ppr (opSourceIndices op)
        , text " rate:    "     <> ppr (opInputRate     op)
        , text " type:    "     <> ppr (opElemType      op) ]

 ppr op@OpScatter{}
        = vcat
        [ text "Scatter"
        , text " vector:  "     <> ppr (opTargetVector  op)
        , text " indices: "     <> ppr (opSourceIndices op)
        , text " elems:   "     <> ppr (opSourceElems   op)
        , text " rate:    "     <> ppr (opInputRate     op)
        , text " type:    "     <> ppr (opElemType      op) ]

 ppr op@OpGenerate{}
        = vcat
        [ text "Generate"
        , text " rate:    "     <> ppr (opOutputRate    op) ]

 ppr op@OpReduce{}
        = vcat
        [ text "Reduce"
        , text " rate:    "     <> ppr (opInputRate     op)
        , text " input:   "     <> ppr (opInputSeries   op) ]

 ppr op@OpMap{}
        = vcat
        [ text "Map"
        , text " rate:    "     <> ppr (opInputRate     op) ]

 ppr op@OpPack{}
        = vcat
        [ text "Pack"
        , text " input  rate: " <> ppr (opInputRate     op) 
        , text " output rate: " <> ppr (opOutputRate    op) ]

 ppr op@OpSeries{}
        = vcat
        [ text "Series"
        , text " rate:    "     <> ppr (opInputRate     op)
        , text " input:   "     <> ppr (opInputRateVec  op)
        , text " result:  "     <> ppr (opResultSeries  op) ]

