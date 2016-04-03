
module DDC.Core.Check.Judge.Type.Sub
        (checkSub)
where
import DDC.Core.Check.Judge.Type.Base


-- This is the subtyping rule for the type checking judgment.
checkSub table !a ctx0 demand xx0 tExpect
 = do   
        ctrace  $ vcat 
                [ text "*>  Sub Check"
                , text "    demand:  " <> (text $ show demand)
                , text "    tExpect: " <> (ppr tExpect) 
                , empty ]

        let config      = tableConfig table

        (xx1, tSynth, eff, ctx1)
         <- tableCheckExp table table ctx0 Synth demand xx0 

        -- Substitute context into synthesised and expected types.
        let tSynth'     = applyContext ctx1 tSynth
        let tExpect'    = applyContext ctx1 tExpect

        ctrace  $ vcat
                [ text "*.  Sub Check"
                , text "    demand:  " <> (text $ show demand)
                , text "    tExpect: " <> (ppr tExpect) 
                , empty ]

        (xx2, ctx2)     <- makeSub config a
                                ctx1 xx1 tSynth' tExpect'
                        $  ErrorMismatch a  tSynth' tExpect' xx0

        ctrace  $ vcat
                [ text "*<  Sub"
                , indent 4 $ ppr xx0
                , text "    tExpect:  " <> ppr tExpect
                , text "    tSynth:   " <> ppr tSynth
                , text "    tExpect': " <> ppr tExpect'
                , text "    tSynth':  " <> ppr tSynth'
                , indent 4 $ ppr ctx0
                , indent 4 $ ppr ctx1
                , indent 4 $ ppr ctx2
                , empty ]

        returnX a
                (\_ -> xx2)
                tExpect
                eff ctx2
