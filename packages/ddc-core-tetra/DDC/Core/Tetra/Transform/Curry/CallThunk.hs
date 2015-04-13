
module DDC.Core.Tetra.Transform.Curry.CallThunk
        (makeCallThunk)
where
import DDC.Core.Tetra.Transform.Curry.Interface
import DDC.Core.Annot.AnTEC
import DDC.Core.Tetra
import DDC.Core.Tetra.Compounds
import DDC.Core.Exp


-- | Apply a thunk to some more arguments.
makeCallThunk
        :: Show a
        => AnTEC a Name                 -- ^ Annotation from functional part of application.
        -> Name                         -- ^ Name of thunk.
        -> [Exp (AnTEC a Name) Name]    -- ^ Arguments to thunk.
        -> Bool                         -- ^ Whether the result was run
        ->  Exp (AnTEC a Name) Name

makeCallThunk aF nF xsArgs bRun
 = let  -- tsArgs          = map annotType $ map annotOfExp xsArgs
        (tsArgs, tResult)    = takeTFunArgResult $ annotType aF
   in   makeRun aF bRun
         $ xFunApply aF tsArgs tResult (XVar aF (UName nF)) xsArgs

