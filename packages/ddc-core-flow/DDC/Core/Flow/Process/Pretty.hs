
module DDC.Core.Flow.Process.Pretty where
import DDC.Core.Flow.Process.Process
import DDC.Core.Flow.Process.Operator
import DDC.Core.Flow.Context
import DDC.Base.Pretty
import DDC.Core.Pretty          ()


instance Pretty Process where
 ppr p
  = vcat
  $     [ ppr (processName p)
        , text "  parameters:    " <> ppr (processParamValues p) 
        , indent 2 $ ppr $ processContext p ]


instance Pretty Context where
 ppr cc
  = case cc of
    ContextRate{}
       -> vcat
        $ [ text "Rate " <> ppr (contextRate cc) ]
          ++ ops
          ++ inner
    ContextSelect{}
       -> vcat
        $ [ text "Select " <> ppr (contextInnerRate cc) <> text " <= " <> ppr (contextOuterRate cc)
          , text " flags: " <> ppr (contextFlags cc) 
          , text " sel:   " <> ppr (contextSelector cc) ]
          ++ ops
          ++ inner
    ContextSegment{}
       -> vcat
        $ [ text "Segment " <> ppr (contextInnerRate cc) <> text " <= " <> ppr (contextOuterRate cc)
          , text " lens:  " <> ppr (contextLens cc) 
          , text " segd:  " <> ppr (contextSegd cc) ]
          ++ ops
          ++ inner
    ContextAppend{}
       -> vcat
        $ [ text "Append " <> ppr (contextRate1 cc) <> text " " <> ppr (contextRate2 cc)
          , indent 2 $ ppr $ contextInner1 cc
          , indent 2 $ ppr $ contextInner2 cc ]

  where
   ops = map (indent 2 . ppr) (contextOps cc)
   inner = map (indent 2 . ppr) (contextInner cc)


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
        , text " rate:    "     <> ppr (opInputRate     op)
        , text " result:  "     <> ppr (opResultSeries  op) ]

 ppr op@OpPack{}
        = vcat
        [ text "Pack"
        , text " input  rate: " <> ppr (opInputRate     op) 
        , text " output rate: " <> ppr (opOutputRate    op) ]

 ppr op@OpSeriesOfRateVec{}
        = vcat
        [ text "SeriesOfRateVec"
        , text " rate:    "     <> ppr (opInputRate     op)
        , text " input:   "     <> ppr (opInputRateVec  op)
        , text " result:  "     <> ppr (opResultSeries  op) ]

 ppr op@OpSeriesOfArgument{}
        = vcat
        [ text "SeriesOfArgument"
        , text " rate:    "     <> ppr (opInputRate     op)
        , text " result:  "     <> ppr (opResultSeries  op) ]


