
module DDC.Core.Check.Judge.Type.Sub
        (checkSub)
where
import DDC.Core.Check.Judge.Type.Base


-- This is the subtyping rule for the type checking judgment.
checkSub table !a ctx0 xx0 tExpect
 = do
        (xx1, tSynth, eff, clo, ctx1)
         <- tableCheckExp table table ctx0 xx0 Synth

        -- Substitute context into synthesised and expected types.
        let tExpect'    = applyContext ctx1 tExpect
        let tSynth'     = applyContext ctx1 tSynth

        (xx2, ctx2)     <- makeSub a ctx1 xx1 tSynth' tExpect'

        ctrace  $ vcat
                [ text "* Sub"
                , indent 2 $ ppr xx0
                , text "  tExpect:  " <> ppr tExpect
                , text "  tSynth:   " <> ppr tSynth
                , text "  tExpect': " <> ppr tExpect'
                , text "  tSynth':  " <> ppr tSynth'
                , indent 2 $ ppr ctx0
                , indent 2 $ ppr ctx1
                , indent 2 $ ppr ctx2 
                , empty ]

        returnX a
                (\_ -> xx2)
                tExpect
                eff clo ctx2
