
module DDC.Core.Check.Exp.Inst
        (checkSub)
where
import DDC.Core.Check.Exp.Base
import Debug.Trace

checkSub table !a ctx0 xx tExpect
 = do
        (xx', tSynth, eff, clo, ctx1)
         <- tableCheckExp table table ctx0 xx Synth

        ctx2    <- checkInst table ctx1 tSynth tExpect

        returnX a
                (\_ -> xx')
                tSynth
                eff clo ctx2


checkInst _table ctx0 tL tR 
 = trace 
   (renderIndent $ vcat
        [ text "* checkInst"
        , indent 2 $ ppr ctx0
        , text "  tL: " <> ppr tL
        , text "  tR: " <> ppr tR 
        , empty ])
 $ return ctx0

